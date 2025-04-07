## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(PartRidge)

## -----------------------------------------------------------------------------
library(palmerpenguins)
penguins <- na.omit(penguins) #PartRidge should be run with a complete data set w/ no NAs
penguins_df <- as.data.frame(penguins) #Objects processed by PartRidge should be of class "data.frame" exclusively

## -----------------------------------------------------------------------------
penguins_dummy <- DummyEncode(penguins_df, Categorical_Variables = c("species", "island", "sex", "year"))

## -----------------------------------------------------------------------------
Model <- ObtainPartRidge(penguins_dummy, Target_Variable = "body_mass_g", lambda = 0.1)

## -----------------------------------------------------------------------------
Res <- Model$partial_residuals

## -----------------------------------------------------------------------------
Species_Res <- Res[, c("Adelie", "Gentoo", "Chinstrap")]

## -----------------------------------------------------------------------------
Cat_Vars <- colnames(Res[, 4:14])

## -----------------------------------------------------------------------------
Plot <- PlotPartRidge(Res, dummy = TRUE, dummy_vars = Cat_Vars)
Plot

