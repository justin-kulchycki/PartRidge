#Test Push

#'Dummy encode (one-hot encode) your categorical variables
#'
#'This function allows the user to one-hot encode categorical variables to include in a ridge regression analysis. The user passes in a data frame and a vector of categorical (Character) columns to be encoded. DummyEncode returns a data frame where the categorical columns are one-hot encoded. Each unique category in the original column maps to a new column in the resulting data frame. If a particular observation was of that category, it will contain a 1 in this column. If not, it will contain a 0.
#' @param data A dense data frame containing all variables to be included in a ridge regression.
#' @param Categorical_Variables A vector of variables in the data set the user would like to dummy encode.
#' @return A data frame where the categorical columns are one-hot encoded. Each unique category in the original column maps to a new column in the resulting data frame. If a particular observation was of that category, it will contain a 1 in this column. If not, it will contain a 0.
#' @export
DummyEncode <- function(data, Categorical_Variables) {
  # Make a copy of the original data frame to avoid modifying the original
  data_copy <- data

  # Loop through each categorical variable
  for (var in Categorical_Variables) {

    # Check if the variable exists in the data
    if (!(var %in% names(data))) {
      stop(paste("Column", var, "does not exist in the data frame"))
    }

    # Get the unique values for the categorical column
    unique_values <- unique(data[[var]])

    # Loop through each unique value and create the dummy columns
    for (value in unique_values) {
      # Create a new column using only the unique value (without the original column name)
      new_column_name <- as.character(value)
      data_copy[[new_column_name]] <- ifelse(data[[var]] == value, 1, 0)
    }

    # Remove the original categorical column after creating dummies
    data_copy[[var]] <- NULL
  }

  return(data_copy)
}
