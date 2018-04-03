# ----------------------------------------------------------------------------------------
# R Script to score current period of each currency based on the newly entered data
# ----------------------------------------------------------------------------------------

# Expected output: Table containing market type number for every of the 28 currency pairs written as files to the sandboxes of terminals
library(tidyverse)
library(lubridate)
library(h2o)
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/TEST/evaluate_market_type.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/TEST/to_m.R")

# Defining variables to be re-used in the code
sbx <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files"
sbx_masterT1 <- "C:/Program Files (x86)/FxPro - Terminal1/MQL4/Files"
sbx_slaveT3 <- "C:/Program Files (x86)/FxPro - Terminal3/MQL4/Files"
sbx_slaveT4 <- "C:/Program Files (x86)/FxPro - Terminal4/MQL4/Files"

# Vector of currency pairs
Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   

# Reading the data from the Sandbox of Terminal 2 --> !!!Make sure that DataWriter robot is attached and working in Terminal 2!!!
sbx_price <- file.path(sbx, "AI_CP15.csv")
sbx_macd <- file.path(sbx, "AI_Macd15.csv")
#price <- read_csv(sbx_price, col_names = F)
macd <- read_csv(sbx_macd, col_names = F, col_types = "cdddddddddddddddddddddddddddd")
macd$X1 <- ymd_hms(macd$X1)

# Prepare data frame with last 64 observations of all 28 pairs and remove date/time column (16 hours)
macd_100 <- macd %>% select(c(X2:X29)) %>% head(64)

# Rename the columns
names(macd_100) <- Pairs

# initialize the virtual machine
h2o.init()

# test for all columns
for (PAIR in Pairs) {
  # PAIR <- "EURUSD"
  # Extract one column with Indicator data for 1 pair (e.g. "EURUSD")
  df <- macd_100 %>% select(PAIR)
  # Use function to score the data to the model
  my_market <- evaluate_market_type(x = df,
                                    model_path = "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/regression.bin/DL_Regression",
                                    num_cols = 64) %>% as.data.frame()
  # Return the name of the output
  names(my_market) <- PAIR
  # Write obtained result to the sandboxes
  write_csv(my_market, file.path(sbx_masterT1, paste0("AI_MarketType_", PAIR, ".csv")))
  write_csv(my_market, file.path(sbx_slaveT3,  paste0("AI_MarketType_", PAIR, ".csv")))
  write_csv(my_market, file.path(sbx_slaveT4,  paste0("AI_MarketType_", PAIR, ".csv")))
}


# shutdown  the virtual machine
h2o.shutdown(prompt = F)
