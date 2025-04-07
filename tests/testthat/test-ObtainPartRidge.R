# test-ObtainPartRidge.R
library(testthat)
library(glmnet)

# Create a small mock data frame for testing
set.seed(123)
df_test <- data.frame(
  X1 = rnorm(100),
  X2 = rnorm(100),
  Target_Variable = rnorm(100)
)

# Test case for ObtainPartRidge function
test_that("ObtainPartRidge returns the correct components", {
  lambda <- 0.1  # example lambda value

  # Run the function
  result <- ObtainPartRidge(df_test, "Target_Variable", lambda)

  # Check if the result is a list
  expect_type(result, "list")

  # Check if the list contains the correct elements
  expect_true("model" %in% names(result))
  expect_true("predictions" %in% names(result))
  expect_true("actual" %in% names(result))
  expect_true("partial_residuals" %in% names(result))

  # Check if the 'model' component is of class 'elnet' (from glmnet)
  expect_true(inherits(result$model, "elnet"))

  # Check if 'predictions' is a matrix
  expect_true(inherits(result$predictions, "matrix"))

  # Check if 'actual' is a matrix with the correct dimensions
  expect_true(inherits(result$actual, "matrix"))
  expect_equal(nrow(result$actual), length(df_test$Target_Variable) * 0.2)  # 20% of the data is used for testing

  # Check if the partial residuals are a data frame with the correct dimensions
  expect_true(inherits(result$partial_residuals, "data.frame"))
  expect_equal(nrow(result$partial_residuals), length(df_test$Target_Variable) * 0.2)
  expect_equal(ncol(result$partial_residuals), ncol(df_test) - 1)  # Exclude the target variable
})

test_that("Partial residuals are calculated correctly", {
  lambda <- 0.1  # example lambda value

  # Run the function
  result <- ObtainPartRidge(df_test, "Target_Variable", lambda)

  # Check that partial residuals are not NA
  expect_false(any(is.na(result$partial_residuals)))

  # Check that partial residuals are calculated for each predictor (excluding the target variable)
  expect_equal(ncol(result$partial_residuals), ncol(df_test) - 1)
})
