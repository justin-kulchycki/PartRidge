# test-PlotPartRidge.R
library(testthat)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

# Create a small mock partial residuals data frame for testing
set.seed(123)
partial_residuals_df <- data.frame(
  X1 = rnorm(100),
  X2 = rnorm(100),
  X3 = rnorm(100)
)

# Test case 1: Check if the function works without dummy encoding
test_that("PlotPartRidge works without dummy encoding", {
  # Run the function
  plot_result <- PlotPartRidge(partial_residuals_df, dummy = FALSE)

  # Check if the result is a ggplot object
  expect_true(inherits(plot_result, "gg"))
})

# Test case 2: Check if the function works with dummy encoding, and dummy_vars is provided
test_that("PlotPartRidge works with dummy encoding when dummy_vars is provided", {
  # Add some dummy-encoded variables to the partial_residuals_df for this test
  partial_residuals_df$dummy_var1 <- sample(c(0, 1), 100, replace = TRUE)
  partial_residuals_df$dummy_var2 <- sample(c(0, 1), 100, replace = TRUE)

  # Run the function with dummy encoding
  plot_result <- PlotPartRidge(partial_residuals_df, dummy = TRUE, dummy_vars = c("dummy_var1", "dummy_var2"))

  # Check if the result is a ggplot object
  expect_true(inherits(plot_result, "gg"))
})

# Test case 3: Ensure that the function raises an error if dummy_vars is missing when dummy is TRUE
test_that("PlotPartRidge raises error when dummy_vars is missing", {
  # Add some dummy-encoded variables to the partial_residuals_df for this test
  partial_residuals_df$dummy_var1 <- sample(c(0, 1), 100, replace = TRUE)

  # Run the function and expect an error
  expect_error(PlotPartRidge(partial_residuals_df, dummy = TRUE),
               "When dummy is TRUE, you must specify the dummy-encoded variables in 'dummy_vars'.")
})

# Test case 4: Ensure that the function raises an error if specified dummy_vars are missing
test_that("PlotPartRidge raises error if specified dummy_vars are missing", {
  # Add some dummy-encoded variables to the partial_residuals_df for this test
  partial_residuals_df$dummy_var1 <- sample(c(0, 1), 100, replace = TRUE)

  # Run the function and expect an error due to missing dummy_var2
  expect_error(PlotPartRidge(partial_residuals_df, dummy = TRUE, dummy_vars = c("dummy_var1", "dummy_var2")),
               "The following dummy variables are missing in the partial residuals dataframe: dummy_var2")
})

# Test case 5: Check that the function works with an empty partial_residuals_df
test_that("PlotPartRidge works with an empty partial_residuals_df", {
  # Create an empty data frame
  partial_residuals_empty <- data.frame()

  # Run the function and check for the correct behavior
  plot_result <- PlotPartRidge(partial_residuals_empty, dummy = FALSE)

  # Check if the result is a ggplot object
  expect_true(inherits(plot_result, "gg"))
})
