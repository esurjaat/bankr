#' Read Wells Fargo Statement - Credit
#'
#' @param file File path to the credit account statement PDF
#' @param type Specifies output type. detail = transaction details. summary = statement summary
#'
#' @return A tibble containing transactional data
#' @export
#'
#' @examples
#'
read_wf_credit <- function(file, type = "detail"){
  # Checks ====
  stopifnot("File must be a pdf" = stringr::str_detect(file, "pdf$"))

  # Prep ====
  pages <-
    file %>%
    pdftools::pdf_text() %>%
    stringr::str_split(pattern = "\n") %>%
    unlist()

  payment_lines <-
    list(start = pages %>% str_which("^Payments"),
         end = pages %>% str_which("TOTAL PAYMENTS"))

  credit_lines <-
    list(start = pages %>% str_which("^Other Credits"),
         end = pages %>% str_which("TOTAL OTHER CREDITS"))

  charges_lines <-
    list(start = pages %>% str_which("^Purchases, Balance Transfers"),
         end = pages %>% str_which("TOTAL PURCHASES,"))

  fees_lines <-
    list(start = pages %>% str_which("^Fees Charged"),
         end = pages %>% str_which("TOTAL FEES CHARGED FOR THIS"))

  dates <-
    pages %>%
    subset(str_detect(., "([0-9]{2}/[0-9]{2}/[0-9]{4}) to ([0-9]{2}/[0-9]{2}/[0-9]{4})")) %>%
    str_extract_all("([0-9]{2}/[0-9]{2}/[0-9]{4})") %>%
    .[[1]]

  start_month <-
    dates[[1]] %>%
    str_extract("^[0-9]{2}")

  start_year <-
    dates[[1]] %>%
    str_extract("[0-9]{4}$")

  end_month <-
    dates[[2]] %>%
    str_extract("^[0-9]{2}")

  end_year <-
    dates[[2]] %>%
    str_extract("[0-9]{4}$")

  # Segmented Text Extraction ====
  transaction_data <-
    purrr::pmap_df(
      .l = list(
        a = list(payment_lines,
                 credit_lines,
                 charges_lines,
                 fees_lines),
        b = list(pages,
                 pages,
                 pages,
                 pages),
        c = list("Payments",
                 "Credit",
                 "Charges",
                 "Fees")
      ),
      .f = function(a, b, c) {
        if (!purrr::is_empty(a$start)) {
          tibble(raw =
                   b[a$start[[1]]:a$end[[1]]] %>%
                   subset(
                     str_detect(., "^([0-9]{2}/[0-9]{2}( )+[0-9]{2}/[0-9]{2})")
                   )) %>%
            mutate(type = c)
        }
      }
    )

  # Extract Interest Charged ====
  interest <-
    pages %>%
    subset(str_detect(., "INTEREST CHARGE ON (PURCHASES|CASH ADVANCES)")) %>%
    tibble(raw = .,
           type = "Interest",
           transaction_date = mdy(dates[[2]]),
           post_date = mdy(dates[[2]]),
           id = as.character(NA)) %>%
    mutate(amount = raw %>% str_extract("([0-9]+(,)?)+\\.[0-9]{2}$") %>% str_replace_all(",", "") %>% as.numeric(),
           raw = raw %>% str_replace(pattern = "[0-9]+\\.[0-9]{2}$", replacement = ""),
           description = raw %>% str_trim()) %>%
    select(-raw)

  # Putting it together ====
  temp <-
    transaction_data %>%
    mutate(transaction_date = raw %>% str_extract("^[0-9]{2}/[0-9]{2}"),
           raw = raw %>% str_replace(pattern = "^[0-9]{2}/[0-9]{2}( )+", replacement = ""),
           post_date = raw %>% str_extract("^[0-9]{2}/[0-9]{2}"),
           raw = raw %>% str_replace(pattern = "^[0-9]{2}/[0-9]{2}( )+", replacement = ""),
           id = raw %>% str_extract("[A-z0-9]{17}"),
           raw = raw %>% str_replace(pattern = "^[A-z0-9]{17}( )+", replacement = ""),
           amount = raw %>% str_extract("([0-9]+(,)?)+\\.[0-9]{2}$") %>% str_replace_all(",", "") %>% as.numeric(),
           raw = raw %>% str_replace(pattern = "[0-9]+\\.[0-9]{2}$", replacement = ""),
           description = raw %>% str_trim()) %>%
    select(-raw) %>%
    mutate(
      transaction_date = case_when(
        str_extract(transaction_date, "^[0-9]{2}") == start_month ~ mdy(paste(transaction_date, start_year, sep = "/")),
        str_extract(transaction_date, "^[0-9]{2}") == end_month ~ mdy(paste(transaction_date, end_year, sep = "/")),
        TRUE ~ as.Date(NA)
        ),
      post_date = case_when(
        str_extract(post_date, "^[0-9]{2}") == start_month ~ mdy(paste(post_date, start_year, sep = "/")),
        str_extract(post_date, "^[0-9]{2}") == end_month ~ mdy(paste(post_date, end_year, sep = "/")),
        TRUE ~ as.Date(NA)
        )
      ) %>%
    bind_rows(interest) %>%
    arrange(type, transaction_date, post_date)

  # Summary ====
  activity_summary <-
    pages[1:30] %>%
    .[str_which(., "Previous Balance")[[1]]:str_which(., "= New Balance")[[1]]] %>%
    .[-c(2, 4, 6, 9)] %>%
    tibble(text = .) %>%
    mutate(type = c("Previous Balance", "Payments", "Other Credits", "Cash Advances", "Purchases", "Fees", "Interest", "New Balance"),
           transaction = c("Balance", "Debit", "Debit", "Credit", "Credit", "Credit", "Credit", "Balance"),
           amount = str_extract(text, pattern = "(\\$| )[0-9]+(,[0-9]+)?+\\.[0-9]{2}") %>%
             str_replace_all("(\\$| |,)", "") %>%
             as.numeric(),
           date = dates[[2]] %>% mdy()) %>%
    select(type, transaction, amount, date)

  # Output
  if(type == "detail"){
    temp
  } else if(type == "summary"){
    activity_summary
  }

}

