#' Add positivity values of a pdf to a database.
#' 
#' Please note that the following packages must be loaded:
#' - tabulizer
#' - tidyverse
#' - tidytext
#' - quanteda
#' - sentimentanalysis
#' 
#' In addition, a dataframe with six variable (company, year, industry, n_page,
#' n_words, env_words, env_freq, n_pos, pos_wgt) must be created before the
#' function is used.
#' 
#' @param file PDF file of the form companyname_year_country_industry.pdf
#' @export
positive <- function(file) {
  
  # Import text
  text <- extract_text(file) %>%  # This is the key function from the tabulizer package
    str_remove_all("\n") %>%
    str_remove_all("[[:punct:]]") %>%
    str_remove_all("[[:digit:]]") %>%
    str_to_lower()
  
  # Count n of Environment words
  env_salience <- text %>%
    str_count(paste(pull(csr_dictionary, word), collapse = "|"))
  
  # Extract window of words around Environment words
  env_text <- corpus(text) %>%
    tokens(split_hyphens = T)
  env_token <- tokens_keep(env_text,
                           pattern = phrase(pull(csr_dictionary, word)),
                           window = c(20,50))
  
  # Extract counts
  data <- dfm(env_token,
              dictionary = dictionary(list(positivity = DictionaryLM$positive)))
  data <- data[1,] %>%
    convert(to = "data.frame")
  
  # Name
  without_pdf <- str_remove(file, ".pdf")
  without_underscore <- gsub("_", " ", without_pdf)
  
  # Other variables
  data <- data %>%
    mutate(env_words = env_salience,
           company = strsplit(without_underscore," ")[[1]][1],
           industry = strsplit(without_underscore," ")[[1]][4],
           year = as.numeric(as.character(strsplit(without_underscore, " ")[[1]][2])),
           n_page = get_n_pages(file),
           n_words = ntoken(text),
           env_freq = env_words / n_words,
           n_pos = positivity,
           pos_wgt = positivity / n_words)
  
  # Bind with tibble
  positivity <<- bind_rows(positivity, data)
  
}