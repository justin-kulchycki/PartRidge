library(testthat)

# Test 1: Check basic functionality (one-hot encoding)
test_that("DummyEncode performs one-hot encoding correctly", {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c("cat", "dog", "cat"),
    C = c("blue", "green", "blue")
  )

  # Expected result
  expected <- data.frame(
    A = c(1, 2, 3),
    cat = c(1, 0, 1),
    dog = c(0, 1, 0),
    blue = c(1, 0, 1),
    green = c(0, 1, 0)
  )

  result <- DummyEncode(df, Categorical_Variables = c("B", "C"))

  # Check that the result is as expected
  expect_equal(result, expected)
})

# Test 2: Check that an error is thrown if a column doesn't exist
test_that("DummyEncode throws an error for non-existing columns", {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c("cat", "dog", "cat")
  )

  # Expect an error if "C" doesn't exist
  expect_error(DummyEncode(df, Categorical_Variables = c("B", "C")),
               "Column C does not exist in the data frame")
})

# Test 3: Check if the function works with no categorical variables
test_that("DummyEncode returns the original data when no categorical variables are specified", {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6)
  )

  result <- DummyEncode(df, Categorical_Variables = c())

  # Should return the original data
  expect_equal(result, df)
})

# Test 4: Check behavior with a column that has only one unique value
test_that("DummyEncode handles categorical columns with one unique value", {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c("cat", "cat", "cat")
  )

  # The result should have a column 'cat' with all 1s and the original column 'A'
  expected <- data.frame(
    A = c(1, 2, 3),
    cat = c(1, 1, 1)
  )

  result <- DummyEncode(df, Categorical_Variables = c("B"))

  # Check that the result is as expected
  expect_equal(result, expected)
})

# Test 5: Check if no changes happen when no categorical variables are provided
test_that("DummyEncode does not change data if no categorical variables are provided", {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c(4, 5, 6)
  )

  result <- DummyEncode(df, Categorical_Variables = c())

  # The result should be identical to the input data frame
  expect_equal(result, df)
})
