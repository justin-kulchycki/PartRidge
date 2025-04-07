
# PartRidge

<!-- badges: start -->
<!-- badges: end -->

PartRidge allows the user to extract and visualize partial residuals
from a ridge regression model fit with glmnet.

## Installation

You can install the development version of PartRidge from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("justin-kulchycki/PartRidge")
```

```{r example}
library(PartRidge)
```

# Encode Categorical Variables

PartRidge has a one-hot encoder function to encode categorical variables. 

The function takes in a data.frame and a vector of categorical variables to be dummy encoded. 

Each unique category in the original column maps toa bew column in the resulting data frame. 

If a particular observation was of that category, it will contain a 1 in this column. If not, it will contain a 0.

```{r}
#Switch names to variables in your data frame
data_dummy <- DummyEncode(data, Categorical_Variables = c("categorical_variable_1", "categorical_variable_2")) 
```

Now we have a data frame suitable for analysis with PartRidge!

# Obtain Partial Residuals for Predictor Variables

Let's use our encoded data frame to fit a glmnet ridge regression model and examine the partial residuals of the X variables. 

The function ObtainPartRidge() takes in a data frame, a target variable within that data frame (all other variables are predictors), and a lambda value to be used to fit a Ridge Regression model using glmnet. 

This function does not perform lambda optimization through cross-validation. 

However, it does allow the user to examine partial residual values for any given lambda. 

The data are split using an 80/20 percent train/test strategy. Partial residuals for the test set are returned in a data frame within the functions output. 

```{r}
#"Y" is the variable in your data frame that you want to predict using all other variables
Model <- ObtainPartRidge(data_dummy, Target_Variable = "Y", lambda = 0.1) 
```

Lets extract the partial residuals from the output list for downstream analysis.

```{r}
Res <- Model$partial_residuals
```

# Visualize the Partial Residuals for Each Individual Predictor Variable

Now that we have obtained partial residuals using ObtainPartRidge(), we can visualize them graphically to gain insight into our predictor variables.

The function PlotPartRidge() produces plots of partial residuals. 

The function contains two additional arguments to specify if a given model was fit to categorical data using dummy encoding. 

The default of the function assumes that all predictor variables are numeric. 

However, the benefit of examining partial residuals of categorical variables results from additional preprocessing of the partial residual data frame. 


Let's create a vector of all dummy encoded categorical variables to pass into the PlotPartRidge() function

```{r}
#Switch names to variables in your data frame. Each category from the original categorical data frame will have its own column in the dummy encoded data frame
Cat_Vars <- colnames(Res[, c("categorical_variable_1_category_1", "categorical_variable_1_categor_2", "catergorical_variable_2_category_1", "categorical_variable_category_2")]) 
```

Now let's plot our data!

```{r}
Plot <- PlotPartRidge(Res, dummy = TRUE, dummy_vars = Cat_Vars)
```

Now for some basic interpretations. The position of each group on the y-axis gives us information regarding the effect of that given variable on body mass, after adjusting for the effects of all other variables in the model!


