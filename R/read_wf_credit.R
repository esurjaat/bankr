#' Read Wells Fargo Statement - Credit
#'
#' @param file File path to the credit account statement PDF
#'
#' @return A tibble containing transactional data
#' @export
#'
#' @examples
#'
file <- files[[24]]
read_wf_credit <- function(file){

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
  # Function - Extract Transactions ====
  extract_transactions <- function(text){
    text
  }
  # Payments ====
  if(!purrr::is_empty(payment_lines$start)){
    pages[payment_lines$start[[1]]:payment_lines$end[[1]]] %>%
      subset(str_detect(., "^([0-9]{2}/[0-9]{2}( )+[0-9]{2}/[0-9]{2})"))
  }

  # Other Credits ====

  transaction_data <-
    pages %>%
    unlist() %>%
    subset(str_detect(., "^([0-9]{2}/[0-9]{2}( )+[0-9]{2}/[0-9]{2})"))

  dates <-
    pages[[1]] %>%
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

  # Putting it together ====
  temp <-
    tibble(raw = transaction_data) %>%
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
    arrange(transaction_date, post_date)

  # Output
  temp

}
