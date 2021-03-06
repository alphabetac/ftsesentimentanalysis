% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_reports.R
\name{tidy_reports}
\alias{tidy_reports}
\title{Convert pdf to table}
\usage{
tidy_reports(file)
}
\arguments{
\item{file}{List of files}
}
\description{
This function converts a pdf to a UTF-8 string. It removes all digits and
converts to lowercase. It then adds a row to a tibble with the name and
date of each report.
}
