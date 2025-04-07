library(testthat)

# Test 1: Check that the function runs and returns the correct components
test_that("ObtainPartRidge returns the correct components", {
  # Create a simple data frame
  df <- data.frame(
    target = c(1, 2, 3, 4, 5),
    predictor1 = c(1, 2, 3, 4, 5),
    predictor2 = c(5, 4, 3, 2, 1)
  )

  # Run the function with a specific lambda value (e.g., lambda = 0.1)
  result <- ObtainPartRidge(df, Target_Variable = "target", lambda = 0.1)

  # Check if the output is a list with the expected components
  expect_is(result, "list")
  expect_true("model" %in% names(result))
  expect_true("predictions" %in% names(result))
  expect_true("actual" %in% names(result))
  expect_true("partial_residuals" %in% names(result))

  # Check if the model is of class 'elnet' (Ridge model from glmnet)
  expect_is(result$model, c("elnet", "glmnet"))

  # Check if predictions are a matrix
  expect_is(result$predictions, "matrix")

  # Check if the actual values are a matrix
  expect_is(result$actual, "matrix")

  # Check if partial residuals are a data.frame
  expect_is(result$partial_residuals, "data.frame")
})

# Test 2: Check dimensions of the output
test_that("ObtainPartRidge returns correct dimensions for output", {
  # Create a simple data frame
  df <- data.frame(
    target = c(1, 2, 3, 4, 5),
    predictor1 = c(1, 2, 3, 4, 5),
    predictor2 = c(5, 4, 3, 2, 1)
  )

  # Run the function with a specific lambda value
  result <- ObtainPartRidge(df, Target_Variable = "target", lambda = 0.1)

  # Check dimensions of the partial residuals
  expect_equal(dim(result$partial_residuals), c(1, 2))  # 1 row (test set), 2 predictors

  # Check dimensions of the predictions (should match the number of rows in the test set)
  expect_equal(nrow(result$predictions), 1)  # 1 prediction per row in the test set

  # Check dimensions of the actual values (should match the number of rows in the test set)
  expect_equal(nrow(result$actual), 1)  # 1 actual value per row in the test set
})

# Test 3: Check behavior with minimal data (e.g., 2 observations)
test_that("ObtainPartRidge works with minimal data", {
  # Create a minimal data frame
  df <- data.frame(
    target = c(1, 2),
    predictor1 = c(1, 2),
    predictor2 = c(2, 1)
  )

  # Run the function with a specific lambda value
  result <- ObtainPartRidge(df, Target_Variable = "target", lambda = 0.1)

  # Check that the model is returned and is of correct class
  expect_is(result$model, c("elnet", "glmnet"))

  # Check that partial residuals are still returned as a data frame, even with minimal data
  expect_is(result$partial_residuals, "data.frame")
})

# Test 4: Check if it handles very small lambda values (lambda close to 0)
test_that("ObtainPartRidge handles very small lambda values", {
  # Create a simple data frame
  df <- data.frame(
    target = c(1, 2, 3, 4, 5),
    predictor1 = c(1, 2, 3, 4, 5),
    predictor2 = c(5, 4, 3, 2, 1)
  )

  # Run the function with a very small lambda value
  result <- ObtainPartRidge(df, Target_Variable = "target", lambda = 1e-5)

  # Check that the function runs and the results are valid
  expect_is(result$model, c("elnet", "glmnet"))
  expect_is(result$predictions, "matrix")
  expect_is(result$actual, "matrix")
  expect_is(result$partial_residuals, "data.frame")
})

# Test 5: Check if an error is raised if the target variable doesn't exist in the data
test_that("ObtainPartRidge throws an error for non-existing target variable", {
  # Create a simple data frame
  df <- data.frame(
    target = c(1, 2, 3, 4, 5),
    predictor1 = c(1, 2, 3, 4, 5),
    predictor2 = c(5, 4, 3, 2, 1)
  )

  # Expect an error when the target variable is not in the data
  expect_error(ObtainPartRidge(df, Target_Variable = "nonexistent_target", lambda = 0.1),
               "object 'nonexistent_target' not found")
})
