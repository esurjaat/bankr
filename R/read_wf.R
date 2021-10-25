#' Read Wells Fargo Account Statement (Checking)
#'
#' @param file File path to the checking account statement PDF
#'
#' @return A tibble containing transactional data
#' @export
#'
#' @examples
#' read_wf("Checking_Account.pdf")
read_wf <- function(file){
  # Define Pages and Context ====
  pages <-
    file %>%
    pdftools::pdf_text() %>%
    stringr::str_split(pattern = "\n")

  initial_page <-
    pages %>%
    purrr::map_lgl(.f = ~ str_detect(.x, "^Transaction history$") %>% any()) %>%
    which() %>%
    .[[1]]

  additional_pages <-
    pages %>%
    purrr::map_lgl(.f = ~ stringr::str_detect(.x, "^Transaction history \\(continued\\)$") %>% any()) %>%
    which()

  ending_balance_lines <-
    pages[initial_page] %>%
    .[[1]] %>%
    stringr::str_which("^( )+(Ending balance on )([0-9]{1,2})/([0-9]{1,2})")


  starting_page_list <-
    initial_page %>%
    list(
      page = .,
      start = pages[[.]] %>% stringr::str_which("( )+(Date)( )+(Number Description)") + 1,
      end = if(length(ending_balance_lines) == 2){
        ending_balance_lines[[2]] - 1
      } else {
        length(pages[[initial_page]]) - 1
      }
    )

  additional_page_list <-
    additional_pages %>%
    map(.f = ~list(
      page = .x,
      start = pages[[.x]] %>% stringr::str_which("^Transaction history \\(continued\\)$") + 3,
      end = pages[[.x]] %>% stringr::str_which("^( )+Ending balance on [0-9]{1,2}/[0-9]{1,2}") %>% .[[1]] - 1
    ))




  # Scrape Report Date ====
  report_date <-
    pages[initial_page] %>%
    .[[1]] %>%
    .[1] %>%
    stringr::str_extract("^([A-z]+ [0-9]{1,2}, [0-9]{4})") %>%
    lubridate::mdy()

  # Prep table to be formatted ====
  transactions <-
    if(is_empty(additional_pages)) {
      pages %>%
        .[[starting_page_list$page]] %>%
        .[starting_page_list$start:starting_page_list$end] %>%
        dplyr::tibble(text_raw = .)
    } else {
      bind_rows(
        pages %>%
          .[[starting_page_list$page]] %>%
          .[starting_page_list$start:starting_page_list$end] %>%
          dplyr::tibble(text_raw = .),
        additional_page_list %>%
          purrr::map_df(.f =
                   ~pages %>%
                   .[[.x$page]] %>%
                   .[(.x$start + 1):.x$end] %>%
                   dplyr::tibble(text_raw = .)
          )
      )
    }


  # Putting it together ====
  temp <-
    transactions %>%
    dplyr::mutate(
      date = stringr::str_extract(text_raw, "^(.){15}") %>% stringr::str_replace_all(" ", "") %>% dplyr::na_if(""),
      line_type = dplyr::case_when(!is.na(date) ~ "Transaction",
                                   TRUE ~ "Helper"),
      value = dplyr::case_when(line_type == "Transaction" ~ 1,
                               TRUE ~ 0),
      cummulative_value = cumsum(value)
    )

  temp_helpers <-
    temp %>%
    dplyr::filter(line_type == "Helper") %>%
    dplyr::bind_rows(data.frame(text_raw = "ABC", date = NA, line_type = "Helper", value = 0, cummulative_value = 13)) %>%
    dplyr::group_by(cummulative_value) %>%
    dplyr::summarize(text_helper = paste(text_raw, collapse = " ") %>% stringr::str_replace("^( +)", ""))

  temp_transactional <-
    temp %>%
    dplyr::filter(line_type == "Transaction") %>%
    dplyr::left_join(temp_helpers, by = "cummulative_value") %>%
    dplyr::mutate(
      ending_balance = case_when(
        nchar(text_raw) == 166 ~ stringr::str_extract(text_raw, "([0-9\\.,]+)$")
      ),
      text_revised = text_raw %>% stringr::str_replace("^( )+[0-9]{1,2}/[0-9]{1,2}( )+", ""),
      text = text_revised %>% stringr::str_replace("( ){4,999}.+$", ""),
      numbers = text_revised %>% stringr::str_replace("^.{65}", ""),
      ending_balance_logic = !(numbers %>% stringr::str_extract("([0-9\\.,]+)( )+([0-9\\.,]+)$") %>% is.na()),
      numbers_noBalance = dplyr::case_when(
        ending_balance_logic ~ numbers %>% stringr::str_replace("( )+([0-9\\.,]+)$", ""),
        TRUE ~ numbers
      ),
      transaction_type =
        dplyr::case_when(
          nchar(numbers_noBalance) < 32 ~ "Credit",
          TRUE ~ "Debit"
        ),
      amount =
        text_revised %>%
        stringr::str_extract("(([0-9\\.,]+)( +)?)$"),
      amount = numbers_noBalance %>% stringr::str_replace_all(",", "") %>% as.numeric(),
      ending_balance = dplyr::case_when(
        ending_balance_logic ~ numbers %>% stringr::str_extract("([0-9\\.,]+)$") %>% stringr::str_replace_all(",", "") %>% as.numeric(),
        TRUE ~ as.numeric(NA)
      ),
      description =
        case_when(
          is.na(text_helper) ~ text,
          TRUE ~ paste(text, text_helper, sep = " ")
        )
    ) %>%
    dplyr::select(date,
                  description,
                  transaction_type,
                  amount,
                  ending_balance) %>%
    dplyr::mutate(date = paste(date, lubridate::year(report_date), sep = "/") %>% lubridate::mdy(),
                  amount = amount %>% stringr::str_replace_all(",", "") %>% as.numeric())




  temp_transactional
}
