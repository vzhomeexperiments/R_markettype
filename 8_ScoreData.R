# ----------------------------------------------------------------------------------------
# R Script to score current period of each currency based on the newly entered data
# ----------------------------------------------------------------------------------------

# Expected output: Table containing market type number for every of the 28 currency pairs
library(tidyverse)
library(lubridate)
# Reading the data from the Sandbox
sbx <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files"
sbx_price <- file.path(sbx, "AI_CP15.csv")
sbx_macd <- file.path(sbx, "AI_Macd15.csv")

#price <- read_csv(sbx_price, col_names = F)
macd <- read_csv(sbx_macd, col_names = F, col_types = "cdddddddddddddddddddddddddddd")
macd$X1 <- ymd_hms(macd$X1)

# once the data are there we can start the virtual machine and load all our models
# later on we will need to perform one operation for every column
## Adapting function to_m

# Function converting time series data to matrix
to_m <- function(x, n_cols) {
  ### PURPOSE: Transform Time Series Column of the dataframe to the matrix
  #            with specified number of columns. Number of rows will be automatically
  #            found and remaining data points discarded
  # # Uncomment variable to debug function
  # x -< dataframe with one column that is 
  
  # x <- DF_TEMP
  # n_cols <- 150
  
  # get intermediate object and dimension
  Step1 <- x
  # find number of rows of data frame
  nrows <- Step1 %>% nrow()
  # find the number of row in a matrix (Whole Rows), the value will have decimals...
  WN <- nrows/n_cols
  ## extract the whole number uncomment for debug/test
  # WN <- 19.2
  # WN <- 19.8
  if((WN - round(WN)) < 0){WN <- round(WN) - 1} else {WN <- round(WN)}
  # find number of rows to extract data
  n <- n_cols * WN
  # extract relevant matrix
  Step2 <- Step1 %>% 
    head(n) %>% #only use whole number to avoid errors
    t() %>%  # this brings us a matrix
    matrix(nrow = WN, ncol = n_cols, byrow = TRUE) # transforming that into matrix size 20x150
  # return the result of the function
  return(Step2)
}


evaluate_market_type <- function(x, model_path, num_cols){
  # x is a 1 column dataframe containing 32 observations
  # Convert to matrix
  X_m <- to_m(x, num_cols) %>% as.data.frame()
  colnames(X_m) <- c(paste("X",1:num_cols,sep=""))
  # load the dataset to h2o 
  test  <- as.h2o(x = X_m, destination_frame = "test")
  
  # load all models
  m1 <- h2o.loadModel(model_path) 
  
  # retrieve the error on each
  e1 <- h2o.predict(m1, test) 
  
  # round the number to achieve class
  result <- round(e1) %>% as.vector()
  
  # manage negatives and/or bizzare numbers
  if(result <= 0 || result > 6) {element <- -1} else {element <- result}
  
  # output result of prediction from the function
  return(element)
  
}

#1. Extract last 100 observations
#2. Convert them to matrix
#3. Load them to h2o
#4. Score this dataset into each model and save model MSE
#5. find the index of the lowest MSE
#6. record market type for that column into the dataframe or vector

# once all columns are completed:
#7. write the obtained result to the csv file back to sandbox

# Vector of currency pairs
Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   

# Prepare data frame with last 100 observations and remove date/time column
macd_100 <- macd %>% select(c(X2:X29)) %>% head(32)

# Rename the column?
names(macd_100) <- Pairs

# # Solve for one
# X <- macd_100 %>% select(EURUSD)
# 
# source("to_m.R")
# X_m <- to_m(X, 32) %>% as.data.frame()
# colnames(X_m) <- c(paste("X",1:32,sep=""))
# load the virtual machine
library(h2o)

# initialize the virtual machine
h2o.init()

# test function
#my_market <- market_type_num(x = X)

# test for all columns
for (PAIR in Pairs) {
  df <- macd_100 %>% select(PAIR)
  my_market <- evaluate_market_type(x = df,
                                    model_path = "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/regression.bin/DeepLearning_model_R_1514534458554_3",
                                    num_cols = 32) %>% as.data.frame()
  names(my_market) <- PAIR
  write_csv(my_market, file.path(sbx, paste0("AI_MarketType_", PAIR, ".csv")))
}


# shutdown
h2o.shutdown(prompt = F)

