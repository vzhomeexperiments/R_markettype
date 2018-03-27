# R_markettype

# Purpose

Read time series data from financial asset and train the Deep Learning Autoencoder model

Use this model on new data to guess current market type

# Main code components

1. Script 7_CombineTrain.R allows to the user to import market data, select the dataset, perform classification problem
2. Script 8_ScoreData.R allows user to deploy R code to production. Code contains functions that will prepare data and use model for every currency pair
3. Script evaluate_market_type.R contains function that performs data scoring
4. Script to_m.R contains function that convert time-series data to the matrix with desired number of columns
5. R-Notebook Research.Rmd describes the process and provide commented code
6. Script 6_h2o_Install.R is allowing to install/re-install h2o package

# Want to learn how to apply this code?

This is a part of a bigger project, join the course series here:

https://www.udemy.com/draft/1482458/?couponCode=LAZYTRADE6-20