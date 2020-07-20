#' Convert pdf to table
#'
#' This function converts a pdf to a UTF-8 string. It removes all digits and
#' converts to lowercase. It then adds a row to a tibble with the name and
#' date of each report.
#'
#' @param file List of files
#' @export
tidy_reports <- function(file) {

  # Make text analyzable
  text <- extract_text(file) %>%
    str_remove_all("[[:digit:]]") %>%
    str_to_lower()

  # Company Name
  without_date <- file %>%
    str_remove("\\d\\d\\d\\d.pdf")
  company_name <- gsub("_", " ", without_date)

  # Create dataset
  reports_dataset <<- reports_dataset %>%
    add_row(report_text = text,
            company = company_name,
            year = as.numeric(as.character(file %>%
                                             str_extract("\\d\\d\\d\\d"))),
            n_page = get_n_pages(file),
            n_words = ntoken(text))
}
