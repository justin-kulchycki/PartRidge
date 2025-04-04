#' Plot the partial residuals obtained with PartRidge
#'
#' This function produces box plots of the data frame of partial residuals.
#' @param partial_residuals_df The data frame titled "partial_residuals" located in the output list of the ObtainPartRidge function.
#' @param dummy Argument tells PlotPartRidge if the input data set contained any variables that were dummy encoded.
#' @param dummy_vars If there are dummy encoded variables, dummy_vars takes in a vector of those variables. Additional pre-processing of dummy encoded partial residual values is necessary for gaining insight into these coefficients.
#' @return An object of class "gg" "ggplot" containing you visualization of the partial residuals of your model coefficients.
#' @export
PlotPartRidge <- function(partial_residuals_df, dummy = FALSE, dummy_vars = NULL) {
  # Load necessary libraries for plotting
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(reshape2)  # For reshaping the data
  library(cowplot)   # For arranging multiple plots

  # If dummy is TRUE, we need to process dummy-encoded variables
  if (dummy) {
    if (is.null(dummy_vars)) {
      stop("When dummy is TRUE, you must specify the dummy-encoded variables in 'dummy_vars'.")
    }

    # Ensure the specified dummy variables exist in the partial residuals dataframe
    missing_vars <- setdiff(dummy_vars, colnames(partial_residuals_df))
    if (length(missing_vars) > 0) {
      stop(paste("The following dummy variables are missing in the partial residuals dataframe:",
                 paste(missing_vars, collapse = ", ")))
    }

    # Store the dummy variables as a separate dataframe
    dummy_df <- partial_residuals_df[, dummy_vars, drop = FALSE]

    # Remove duplicate values across rows and retain unique values
    dummy_df_transposed <- as.data.frame(t(dummy_df))

    dummy_df_transposed <- dummy_df_transposed %>%
      mutate(across(everything(), ~ {
        Duplicated_Values <- names(table(.))[table(.) > 1]
        .[ . %in% Duplicated_Values ] <- NA
        .
      }))

    dummy_df_transposed_sparse <- as.data.frame(t(dummy_df_transposed))
    dummy_df_transposed_sparse$Sample <- row.names(dummy_df_transposed_sparse)
    row.names(dummy_df_transposed_sparse) <- 1:nrow(dummy_df_transposed_sparse)

    # Pivot data to long format for dummy variables
    dummy_df_transposed_sparse_long <- dummy_df_transposed_sparse %>%
      pivot_longer(cols = -Sample,
                   names_to = "variable",
                   values_to = "partial_residuals")
    dummy_df_transposed_sparse_long_cleaned <- dummy_df_transposed_sparse_long[!is.na(dummy_df_transposed_sparse_long$partial_residuals), ]

    # Plot for dummy variables
    plot_dummy <- ggplot(dummy_df_transposed_sparse_long_cleaned, aes(x = variable, y = partial_residuals)) +
      geom_boxplot() +
      geom_point() +
      labs(
        title = "Categorical Predictors",
        x = "",
        y = "Partial Residuals"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
            plot.title = element_text(face = "bold", hjust = 0.5),  # Center title and make it bold
            axis.title.x = element_text(face = "bold"),  # Bold x-axis title
            axis.title.y = element_text(face = "bold"))  # Bold y-axis title

    # Now we plot the non-dummy (continuous) variables
    continuous_vars <- setdiff(colnames(partial_residuals_df), dummy_vars)
    continuous_df <- partial_residuals_df[, continuous_vars, drop = FALSE]

    # Reshape the continuous variables dataframe into long format
    continuous_df_long <- reshape2::melt(continuous_df, id.vars = NULL)

    # Plot for continuous variables
    plot_continuous <- ggplot(continuous_df_long, aes(x = variable, y = value)) +
      geom_boxplot() +
      geom_point() +
      labs(
        title = "Numeric Predictors",
        x = "",
        y = "Partial Residuals"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
            plot.title = element_text(face = "bold", hjust = 0.5),  # Center title and make it bold
            axis.title.x = element_text(face = "bold"),  # Bold x-axis title
            axis.title.y = element_text(face = "bold"))  # Bold y-axis title

    # Arrange the two plots side-by-side using cowplot
    plot_grid(plot_dummy, plot_continuous, ncol = 2)

  } else {
    # If dummy is FALSE, just plot the partial residuals for all predictors
    partial_residuals_long <- reshape2::melt(partial_residuals_df, id.vars = NULL)

    ggplot(partial_residuals_long, aes(x = variable, y = value)) +
      geom_boxplot() +
      geom_point() +
      labs(
        title = "Partial Residuals for Each Predictor",
        x = "",
        y = "Partial Residuals"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text
            plot.title = element_text(face = "bold", hjust = 0.5),  # Center title and make it bold
            axis.title.x = element_text(face = "bold"),  # Bold x-axis title
            axis.title.y = element_text(face = "bold"))  # Bold y-axis title
  }
}
