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
