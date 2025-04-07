#' Obtain partial residuals for a ridge regression rodel fit with glmnet.
#'
#' This function takes a data frame, a target variable within that data frame (all other variables are predictors), and a lambda value to be used to fit a Ridge Regression model using glmnet. The function returns a list containg the ridge model (class "elnet" "glmnet"), the predicted values of the target variable (class = "matrix" "array"), the actual values of the target variable (class = "matrix" "array"), and the partial residuals for each coefficient in the model (class = "data.frame").
#' @import glmnet
#' @param df A dense data frame used to fit a ridge regression model.
#' @param Target_Variable The Y variable to be predicted using the ridge regression rodel. All other variables in the data frames are predictors.
#' @param lambda The labmda value to use for the Ridge penalty. This should be determined prior using cross-validation.
#' @return The function returns a list containg the ridge model (class "elnet" "glmnet"), the predicted values of the target variable (class = "matrix" "array"), the actual values of the target variable (class = "matrix" "array"), and the partial residuals for each coefficient in the model (class = "data.frame").
#' @export
ObtainPartRidge <- function(df, Target_Variable, lambda) {
  # Step 1: Split the data into training (80%) and testing (20%) sets
  set.seed(2112)  # For reproducibility
  train_indices <- sample(1:nrow(df), size = 0.8 * nrow(df))
  train_set <- df[train_indices, ]
  test_set <- df[-train_indices, ]

  # Step 2: Separate the data into X (predictors) and Y (target)
  X_train <- train_set[, setdiff(names(df), Target_Variable)]  # Exclude the Y column
  Y_train <- train_set[[Target_Variable]]  # Target variable

  X_test <- test_set[, setdiff(names(df), Target_Variable)]
  Y_test <- test_set[[Target_Variable]]

  # Step 3: Perform Ridge Regression
  ridge_model <- glmnet::glmnet(as.matrix(X_train), Y_train, alpha = 0)  # alpha = 0 for Ridge

  # Step 4: Make predictions on the test set using the specified lambda
  Y_pred <- predict(ridge_model, as.matrix(X_test), s = lambda)  # Use the specified lambda

  # Step 5: Calculate Partial Residuals using the test set only
  Y_hat_test <- predict(ridge_model, as.matrix(X_test), s = lambda)  # Fitted values for the test set

  # Initialize a matrix to store partial residuals for each predictor on the test set
  partial_residuals_matrix <- matrix(NA, nrow = nrow(X_test), ncol = ncol(X_test))
  colnames(partial_residuals_matrix) <- names(X_test)
  rownames(partial_residuals_matrix) <- rownames(X_test)  # Using rownames of test_set as sample indices

  # Extract coefficients for the specified lambda
  beta <- as.vector(coef(ridge_model, s = lambda))  # Convert to a numeric vector
  names(beta) <- rownames(coef(ridge_model, s = lambda))  # Assign names to the coefficients

  # Loop through each predictor and calculate the partial residuals on the test set
  for (i in 1:ncol(X_test)) {
    X_k <- as.numeric(X_test[, i])  # Ensure the predictor is numeric
    beta_k <- beta[i + 1]  # Get the coefficient for the predictor (offset by 1)

    # Calculate partial residuals for this predictor and store in the matrix
    partial_residuals_matrix[, i] <- Y_test - Y_hat_test + beta_k * X_k
  }

  # Convert the matrix of partial residuals into a data frame
  partial_residuals_df <- as.data.frame(partial_residuals_matrix)

  # Convert the 'actual' to matrix and array class
  actual_matrix <- as.matrix(Y_test)
  dim(actual_matrix) <- c(length(Y_test), 1)  # Ensure it's a 2D matrix (array)

  # Step 6: Return the results (model, predictions, actual as matrix/array, partial residuals)
  return(list(
    model = ridge_model,
    predictions = Y_pred,
    actual = actual_matrix,  # Now it's a matrix
    partial_residuals = partial_residuals_df
  ))
}
