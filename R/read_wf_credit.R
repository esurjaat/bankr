#' Read Wells Fargo Statement - Credit
#'
#' @param file File path to the credit account statement PDF
#'
#' @return A tibble containing transactional data
#' @export
#'
#' @examples
file <- "~/Desktop/Projects/Personal_Finance/Data/Wells Fargo/Account Statements/Credit/Statement of Accounts_Wells Fargo_Credit_2018-09.pdf"
read_wf_credit <- function(file){

  # Prep ====
  pages <-
    file %>%
    pdftools::pdf_text() %>%
    stringr::str_split(pattern = "\n")

  transaction_pages <-
    pages %>%
    purrr::map_lgl(.f = ~str_detect(.x, "^Transactions \\(Continued\\.\\.\\.\\)") %>% any()) %>%
    which()

  transaction_data <-
    pages[transaction_pages] %>%
    unlist() %>%
    subset(str_detect(., "^[0-9]{2}/[0-9]{2}"))

  dates <-
    pages[[1]] %>%
    subset(str_detect(., "([0-9]{2}/[0-9]{2}/[0-9]{4}) to ([0-9]{2}/[0-9]{2}/[0-9]{4})")) %>%
    str_extract_all("([0-9]{2}/[0-9]{2}/[0-9]{4})") %>%
    .[[1]]

  # Putting it together ====
  temp <-
    tibble(raw = transaction_data) %>%
    mutate(transaction_date = raw %>% str_extract("^[0-9]{2}/[0-9]{2}"),
           raw = raw %>% str_replace(pattern = "^[0-9]{2}/[0-9]{2}( )+", replacement = ""),
           post_date = raw %>% str_extract("^[0-9]{2}/[0-9]{2}"),
           raw = raw %>% str_replace(pattern = "^[0-9]{2}/[0-9]{2}( )+", replacement = ""),
           id = raw %>% str_extract("[A-z0-9]{17}"),
           raw = raw %>% str_replace(pattern = "^[A-z0-9]{17}( )+", replacement = ""),
           amount = raw %>% str_extract("[0-9]+\\.[0-9]{2}$"),
           raw = raw %>% str_replace(pattern = "[0-9]+\\.[0-9]{2}$", replacement = ""),
           description = raw %>% str_trim()) %>%
    select(-raw) %>%
    arrange(transaction_date, post_date)

  # Output
  temp

}
