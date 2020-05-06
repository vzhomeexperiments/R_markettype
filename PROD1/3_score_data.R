# ----------------------------------------------------------------------------------------
# R Script to score current period of each currency based on the newly entered data
# ----------------------------------------------------------------------------------------

# Expected output: Table containing market type number for every of the 28 currency pairs written as files to the sandboxes of terminals
#library(tidyverse)
library(dplyr)
library(magrittr)
library(lubridate)
library(readr)
library(h2o)
library(lazytrade)
#source("C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/TEST1/evaluate_market_type.R")
#source("C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/to_m.R")

# Defining variables to be re-used in the code
sbx <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files"
sbx_masterT1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files"
sbx_slaveT3 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files"
sbx_slaveT4 <- "C:/Program Files (x86)/FxPro - Terminal4/MQL4/Files"
sbx_slaveT5 <- "C:/Program Files (x86)/FxPro - Terminal5/MQL4/Files"
chart_period <- 60 #this variable will define market type period
num_cols <- 128

#absolute path to store model objects (useful when scheduling tasks)
path_model <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models"

data_update_path <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/PROD1/data_update"
data_initial_path <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/PROD1/data_initial"

# Vector of currency pairs
Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   

# Reading the data from the Sandbox of Terminal 2 --> !!!Make sure that DataWriter robot is attached and working in Terminal 2!!!
sbx_price <- file.path(sbx, paste0("AI_CP",chart_period,"-300.csv"))
sbx_macd <- file.path(sbx, paste0("AI_Macd", chart_period,"-300.csv"))
#price <- read_csv(sbx_price, col_names = F)
macd <- read_csv(sbx_macd, col_names = F, col_types = "cdddddddddddddddddddddddddddd")
macd$X1 <- ymd_hms(macd$X1)

# Prepare data frame with last 64 observations of all 28 pairs and remove date/time column (16 hours)
macd_100 <- macd %>% select(c(X2:X29)) %>% head(128)

# Rename the columns
names(macd_100) <- Pairs

# initialize the virtual machine
h2o.init(nthreads = 1)

# test for all columns
for (PAIR in Pairs) {
  # PAIR <- "EURUSD"
  # PAIR <- "GBPUSD"
  # PAIR <- "EURGBP"
  # Extract one column with Indicator data for 1 pair (e.g. "EURUSD")
  df <- macd_100 %>% select(PAIR)
  # Use function to score the data to the model
  my_market_prediction <- evaluate_market_type(x = df,
                                    #model_path = "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/regression.bin/DL_Regression",
                                    model_path = "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/classification.bin/DL_Classification",
                                    num_cols = 128) 
  # predicted value to write
  my_market <- my_market_prediction  %>% select(predict)
  
  # Join data to the predicted class and save to the temporary dataframe, only if predicted confidence is higher than 0.95
  # get predicted confidence
  my_market_conf <- my_market_prediction %>% select(-1) %>% select(which.max(.))
  # write dataframe to the temporary dataframe
  if(!exists("df_temp") && my_market_conf[1,1]> 0.96){
    # join data to predicted class and write to the new object df_temp
    df_row <- t(df) %>% as.data.frame(row.names = 1)
    colnames(df_row) <- c(paste("X",1:num_cols,sep=""))
    df_temp <- df_row %>% transform(M_T = my_market[1,1]) %>% mutate_at("M_T", as.character)
    
  } else if(exists("df_temp") && my_market_conf[1,1]> 0.96) { 
    # in case df_temp is already exists
    # join data to predicted class and write to the new object df_temp
    df_row <- t(df) %>% as.data.frame(row.names = 1)
    colnames(df_row) <- c(paste("X",1:num_cols,sep=""))
    df_temp <-  df_row %>% transform(M_T = as.character(my_market[1,1])) %>% mutate_at("M_T", as.character) %>% bind_rows(df_temp)
    }
  
  # Return the name of the output
  names(my_market) <- PAIR
  # Write obtained result to the sandboxes
  #write_csv(my_market, file.path(sbx, paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_market, file.path(sbx_masterT1, paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_market, file.path(sbx_slaveT3,  paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_market, file.path(sbx_slaveT4,  paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
  write_csv(my_market, file.path(sbx_slaveT5,  paste0("AI_MarketType_", PAIR, chart_period, ".csv")))
}

# retrieve already recorded data >> add temporary dataframe >> write to the data_update folder
# check if there is a rds file in the data_update folder and add the df_temp there
if(exists("df_temp") && !file.exists(file.path(data_update_path, "macd_ai_classified.rds")))
  {
    # write file first time
    write_rds(df_temp, file.path(data_update_path, "macd_ai_classified.rds"))
} else if(exists("df_temp")) {
    # read previous file
      read_rds(file.path(data_update_path, "macd_ai_classified.rds")) %>% 
      # join obtained data
      bind_rows(df_temp) %>% 
      # write data back
      write_rds(file.path(data_update_path, "macd_ai_classified.rds"))
      #verify generated data
      # x1 <- read_rds(file.path(data_update_path, "macd_ai_classified.rds"))
  }

# shutdown  the virtual machine
h2o.shutdown(prompt = F)
