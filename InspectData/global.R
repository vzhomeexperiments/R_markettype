#global.r
#read dataset
library(readr)
#path to user repo:
#!!!Change this path!!! 
path_user <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype"
path_data <- file.path(path_user, "_DATA")

#data for this App to work
macd_ai <- readr::read_rds(file.path(path_data, 'macd_ai_classified.rds'))

#output data from this app
file_checked <- file.path(path_data, "macd_checked_60M.rds")