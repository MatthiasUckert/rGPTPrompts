% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{get_function_code}
\alias{get_function_code}
\title{Get Function Name and Code as a Character String}
\usage{
get_function_code(.fun, .name = NULL)
}
\arguments{
\item{.fun}{A function for which the name and code should be returned.}

\item{.name}{(Optional) A character string representing the name of the input function.}
}
\value{
A character string containing the function name and code.
}
\description{
This function takes a function as input and returns its name and code as a single character string.
An optional argument allows you to provide the function name directly.
}
\examples{
sample_function <- function(x) {
  return(x * 2)
}
result <- get_function_code(sample_function)
}
