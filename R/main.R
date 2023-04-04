#' Get Function Name and Code as a Character String
#'
#' This function takes a function as input and returns its name and code as a single character string.
#' An optional argument allows you to provide the function name directly.
#'
#' @param .fun A function for which the name and code should be returned.
#' @param .fun_name (Optional) A character string representing the name of the input function.
#'
#' @return A character string containing the function name and code.
#' @export
#'
#' @examples
#' sample_function <- function(x) {
#'   return(x * 2)
#' }
#' result <- get_function_code(sample_function)
get_function_code <- function(.fun, .fun_name = NULL) {
  # Check if the input is a function
  if (class(.fun) != "function") {
    # Stop and display an error message if the input is not a function
    stop("Input must be a function.")
  }

  # Check if the function name is provided
  if (is.null(.fun_name)) {
    # If the function name is not provided, extract it from the input
    func_name <- deparse(substitute(.fun))
  } else {
    # If the function name is provided, use it
    func_name <- .fun_name
  }

  # Get the code of the input function as a character string
  code_string <- deparse(.fun)

  # Return the function name and code combined into a single character string
  return(paste(func_name, "<-", paste(code_string, collapse = "\n")))
}


#' Generate a Formatted Message for Function Analysis
#'
#' This function takes an input function and generates a formatted message with the input function
#' and instructions for analyzing it.
#'
#' @param .fun A function to be analyzed.
#'
#' @return No return value, the function prints a formatted message to the console.
#' @export
#'
#' @examples
#' example_function <- function(x) { return(x * 2) }
#' gpt_comment_function(example_function)
gpt_comment_function <- function(.fun) {
  # Get the name of the input function
  func_name <- deparse(substitute(.fun))

  # Concatenate and display the formatted message with the function and instructions
  cat(paste0(
    "Your task is to analyze the following function:\n\n",
    get_function_code(.fun, func_name),
    "\n\nSubsequently you should do the following:
    1. Ask me any clarifying question you have about the functionality of this function.
       Only after I clarified those open points you continue with points 2-4.
       (Never assume that you already know the answers! Always wait for my input)
    2. Every single function call within the function should have the package specified.
       If the package is not specified you have to add it before the function call.
       (Except the function is called from base R)
       Subsequently, you should comment every single line of this function
       (You should put the comments above the line).
    3. Provide a Roxygen Skeleton of this function.
    4. Write a unit test for this function"
  ))

}


gpt_comment_function(gpt_comment_function)
