#' Convert pdf to table
#'
#' This function converts a pdf to a UTF-8 string. It removes all digits and
#' converts to lowercase. It then adds a row to a tibble with the name and
#' date of each report.
#' 
#' Please note that the following packages must be loaded:
#' - tabulizer
#' - tidyverse
#' - tidytext
#' - quanteda
#' 
#' In addition, a dataframe with six variable (report_text, company, industry,
#' year, n_page, n_words) must be created before the function is used.
#'
#' @param file PDF file of the form companyname_year_country_industry.pdf
#' @export
tidy_reports <- function(file) {

  # Make text analyzable
  text <- extract_text(file) %>%
    str_remove_all("[[:digit:]]") %>%
    str_to_lower()

  # Company Name
  without_pdf <- str_remove(file, ".pdf")
  without_underscore <- gsub("_", " ", without_pdf)

  # Create dataset
  reports_dataset <<- reports_dataset %>%
    add_row(report_text = text,
            company = strsplit(without_underscore," ")[[1]][1],
            industry = strsplit(without_underscore," ")[[1]][4],
            year = as.numeric(as.character(strsplit(without_underscore, " ")[[1]][2])),
            n_page = get_n_pages(file),
            n_words = ntoken(text))
}
