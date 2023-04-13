#' Get Function Name and Code as a Character String
#'
#' This function takes a function as input and returns its name and code as a single character string.
#' An optional argument allows you to provide the function name directly.
#'
#' @param .fun A function for which the name and code should be returned.
#' @param .name (Optional) A character string representing the name of the input function.
#'
#' @return A character string containing the function name and code.
#' @export
#'
#' @examples
#' sample_function <- function(x) {
#'   return(x * 2)
#' }
#' result <- get_function_code(sample_function)
get_function_code <- function(.fun, .name = NULL) {
  # Check if the input is a function
  if (!inherits(.fun, what = "function")) {
    # Stop and display an error message if the input is not a function
    stop("Input must be a function.")
  }

  # Check if the function name is provided
  if (is.null(.name)) {
    # If the function name is not provided, extract it from the input
    func_name <- deparse(substitute(.fun))
  } else {
    # If the function name is provided, use it
    func_name <- .name
  }

  # Get the code of the input function as a character string
  code_string <- deparse(.fun)

  # Return the function name and code combined into a single character string
  return(paste(func_name, "<-", paste(code_string, collapse = "\n")))
}


#' gpt_function
#'
#' This function provides a standardized interface that returns a prompt that can be used for GPT. It takes a function as input and generates a\cr
#' standardized prompt based on the user's selected question about the function.
#'
#' @param .fun A function to be analyzed. The input function should be a valid R function.
#' @param .prompt A character vector specifying the type of question about the input function. The available options are:\cr
#'   - "understand": Ask clarifying questions to understand the function in its entirety.\cr
#'   - "document": Write clarifying comments above every single line of the function.\cr
#'   - "reference": Specify the package of every single function call within the function.\cr
#'   - "roxygen": Provide a filled-in Roxygen Skeleton (see roxygen2 package).\cr
#'   - "unittest": Write a unit test for the function.\cr
#'   - "suggest": Give suggestions on how to improve the function.\cr
#' @return A character string containing the generated prompt based on the input function and the selected question type.
#'
#' @export
#'
#' @examples
#' example_function <- function(x) {
#'   return(x * 2)
#' }
#'
#' # Generate a prompt for understanding the example_function
#' gpt_function(example_function, .prompt = c("understand", "document"))
#'
#' # Generate a prompt for referencing the example_function
#' gpt_function(example_function, .prompt = "reference")
#'
#' # Generate a Roxygen skeleton for the example_function
#' gpt_function(example_function, .prompt = "roxygen")
#'
#' # Generate a prompt for writing a unit test for the example_function
#' gpt_function(example_function, .prompt = "unittest")
#'
#' # Generate a prompt for suggesting improvements for the example_function
#' gpt_function(example_function, .prompt = "suggest")
gpt_function <- function(.fun, .prompt = c("understand", "document", "reference", "roxygen", "unittest", "suggest")) {
  # Get the name of the input function
  func_name <- deparse(substitute(.fun))

  . <- NULL

  task_desc_ <- c(
    "Your task is to analyze the following function.",
    "Make sure you understand its entire structure and use case.",
    "Function:"
    ) %>% paste(., collapse = "\n")

  prompts_ <- list()

  prompts_[["understand"]] <- c(
    "Ask me clarifying question so that you understand the function in its entirety",
    "When asking me those questions, also give me instructions on how I should answer those questions",
    "After receiving my answers, you can reiterate this step as long as there are no queston remaining",
    "Never assume that you already know the answers! Always wait for my input!",
    "Only after I clarified those open points you continue with the remaining points."
  ) %>% paste(., collapse = "\n")

  prompts_[["document"]] <- c(
    "Write a clarifying comment above every single line of this function.",
    "(You should put the comments above every single line, not doing so results in a failure of the task!).",
    "You are NOT ALLOWED to change anything in this function, except that you put a newline to write you comments."
  ) %>% paste(., collapse = "\n")

  prompts_[["reference"]] <- c(
    "Every single function call within the function should have the package specified.",
    "If the package is not specified you have to add it before the function call.",
    "(Except the function is called from base R)"
  ) %>% paste(., collapse = "\n")

  prompts_[["roxygen"]] <- c(
    "Provide a filled in Roxygen Skeleton (see roxygen2 package)"
  )

  prompts_[["unittest"]] <- c(
    "Write a unit test for this function"
  )

  prompts_[["suggest"]] <- c(
    "Give me suggestions on how to improve this function.",
    "Those improvements include:",
    "- Making this function more performant (e.g., by using more efficient packages)",
    "- Reducing the code complexity",
    "- Making the function interface more accesible",
    "- ... or anything else you can come up with",
    "You are supposed to rewrite the complete functions and explain you suggestions in in-code comments."
  ) %>% paste(., collapse = "\n")


  prompts_ <- paste(prompts_[.prompt], collapse = "\n\n")


  paste(
    task_desc_,
    get_function_code(.fun, func_name),
    prompts_,
    sep = "\n\n"
  ) %>% cat()

}


#' Extract functions and roxygen skeletons from R scripts
#'
#' This function takes a list of R scripts and extracts the functions and associated roxygen skeletons.
#' It returns a tibble with the extracted information for each function, including its message number,
#' the script name, the line number, the function name, the roxygen skeleton,
#' the number of tokens (words) in the roxygen skeleton, and the number of characters in the roxygen skeleton.
#'
#' @param .paths A character vector of file paths to the R scripts containing the functions.
#' @param .max_token An integer specifying the maximum number of tokens (words) per roxygen skeleton.
#'   The default value is 2000.
#' @return A tibble with the following columns:
#' \describe{
#'   \item{type}{A character vector indicating if the text is a function or dataset text.}
#'   \item{msg_no}{A numeric vector indicating the message number of each roxygen skeleton.}
#'   \item{script}{A character vector indicating the name of the script that the function was extracted from.}
#'   \item{no}{An integer vector indicating the line number of the function within the script.}
#'   \item{name}{A character vector indicating the name of the function.}
#'   \item{text}{A character vector indicating the roxygen skeleton for the function.}
#'   \item{n_token}{An integer vector indicating the number of tokens (words) in the roxygen skeleton.}
#'   \item{n_chars}{An integer vector indicating the number of characters in the roxygen skeleton.}
#' }
#' @export
extract_functions <- function(.paths, .max_token = 2000) {

  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (is.null(sys.calls())) {
    # .paths <- "R/main.R"
    # .max_token = 2000
    cat("DEBUGING MODE: This Message should not appear when calling the function")
  }

  # Assign NULL to Global Variables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  fun <- name <- n_token <- msg_no <- script <- no <-  n_chars <- text <- type <- tmp <- NULL

  tab_ <- purrr::map_dfr(
    .x = purrr::set_names(.paths, basename(.paths)),
    .f = ~ tibble::tibble(line = readLines(.x)) %>%
      dplyr::mutate(
        no = as.integer(startsWith(line, "#'") & !startsWith(dplyr::lag(line), "#'")),
        no = dplyr::if_else(is.na(no), 1L, no),
        no = cumsum(no)
      ) %>%
      dplyr::group_by(no) %>%
      dplyr::summarise(text = paste(line, collapse = "\n")),
    .id = "script"
  ) %>%
    dplyr::filter(!no == 0) %>%
    dplyr::mutate(
      name = stringi::stri_extract_last_regex(text, "(?<!#\\s').+?<-\\s+?function\\(.*?\\)"),
      name = trimws(stringi::stri_replace_first_regex(name, "<-\\s+?function\\(.*?\\)", "")),
      type = dplyr::if_else(!is.na(name), "Function", "Dataset"),
      name = dplyr::if_else(
        is.na(name), stringi::stri_extract_last_regex(text, '".+?"$'), name
        ),
      n_token = stringi::stri_count_regex(text, "[[:punct:]]|[[:blank:]]|[[:space:]]"),
      n_chars = nchar(text)
    ) %>%
    dplyr::group_by(type) %>%
    dplyr::mutate(
      text = paste(
        glue::glue("{type}:"),
        stringi::stri_pad_left(dplyr::row_number(), 3, 0), paste(rep("-", 25), collapse = ""),
        "\n", text
        ),
    ) %>%
    dplyr::mutate(
      msg_no = ceiling(cumsum(n_token) / .max_token)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(type, msg_no, script, no, name, text, n_token, n_chars)

  return(tab_)
}

#' Generate a GPT-4 prompt for analyzing an R package
#'
#' This function generates a GPT-4 prompt for analyzing an R package, including its functions and datasets.
#' The prompt can be output to the console or a text file.
#'
#' @param .dir The directory of the R package.
#' @param .max_token Maximum tokens per message (integer).
#' @param .output Output format: "txt" (text file) or "console" (console output).
#'
#' @return NULL. The function outputs the generated prompt either to a text file or the console, depending on the value of the .output parameter.
#' @export
gpt_package_prompt <- function(.dir, .max_token = 4000, .output = c("txt", "console")) {
  # DEBUG -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  if (is.null(sys.calls())) {
    # .dir <- "../rGPTPrompts/"
    # .max_token = 2000
    # .output = c("txt", "console")
    cat("DEBUGING MODE: This Message should not appear when calling the function")
  }

  # Assign NULL to Global Variables -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- ---
  msg_no <- type <- tmp <- text <- msg_no <- NULL


  output_ <- match.arg(.output, c("txt", "console"))

  name_ <- basename(.dir)
  fils_ <- list.files(file.path(.dir, "R"), full.names = TRUE)
  .paths <- fils_ <- fils_[!basename(fils_) == "utils-pipe.R"]

  tab_ <- extract_functions(fils_, .max_token = 2000)
  fun_ <- dplyr::filter(tab_, type == "Function")
  dat_ <- dplyr::filter(tab_, type == "Dataset")


  txt_ <- dplyr::bind_rows(fun_, dat_) %>%
    dplyr::group_by(msg_no) %>%
    dplyr::summarise(text = paste(text, collapse = "\n")) %>%
    dplyr::mutate(
      tmp = paste0("MESSAGE: ", dplyr::row_number(), paste(rep("=", 100), collapse = "")),
      text = paste0(tmp, "\n\n\n", text)
    ) %>%
    dplyr::summarise(text = paste(text, collapse = "\n")) %>%
    dplyr::pull(text)


  prompt_ <- glue::glue(
    paste(
      "In the following your task is to Analyze my R Package with the title '{name_}'.",
      "The complete Package consist of {nrow(fun_)} functions and {nrow(dat_)} datasets.",
      "Your task is to analyze all functions and datasets, especially in regards to their interdependencies, functionalities, and purpose",
      "Due to character limitations I will provide you with {max(fun_$msg_no)} messages for the function and {max(dat_$msg_no)} messages for the datasets.",
      "Until you received all functions ({nrow(fun_)} function in {max(fun_$msg_no)} messages), and datasets ({nrow(dat_)} datasets in {max(dat_$msg_no)} messages), you only confirm the prompt, with providong me a list of the functions or datasets as an answer",
      "When you have received all the functions and datasets you confirm this with the message: 'I received all information', and than you list all functions and datasets again.",
      "Only after I received this message from you, I will give you more task related to the package.",
      "Is the task clear or do you have any further questions? (In case it is clear, please provide me with a quick summary of the task)",
      sep = "\n"
    )
  )

  txt_ <- paste(prompt_, "\n\n", txt_, collapse = "\n\n\n\n\n\n\n")

  if (output_ == "txt") {
    tmp_ <- tempfile(fileext = ".txt")
    write(txt_, tmp_)
    utils::browseURL(tmp_)
  } else {
    cat(txt_)
  }
}
