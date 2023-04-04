library(testthat)

test_that("Function returns the correct name and code", {
  # Sample function
  sample_function <- function(x) {
    return(x * 2)
  }

  # Expected result
  expected_result <- "sample_function <- function (x) \n{\n    return(x * 2)\n}"

  # Test
  expect_equal(get_function_code(sample_function), expected_result)
})

test_gpt_comment_function <- function() {
  example_function <- function(x) { return(x * 2) }

  # Capture the output of gpt_comment_function
  output <- capture.output(gpt_comment_function(example_function))

  # Check if the output contains the required elements
  testthat::expect_true(any(grepl("Your task is to analyze the following function", output)))
  testthat::expect_true(any(grepl("example_function", output)))
  testthat::expect_true(any(grepl("Provide a Roxygen Skeleton", output)))
  testthat::expect_true(any(grepl("Write a unit test for this function", output)))
}

# Run the test
#
test_gpt_comment_function()
