# ----------------------------------------------------------------------------------------
# R Script to score current period of each currency based on the newly entered data
# ----------------------------------------------------------------------------------------
# Note: select desired model in lines 41/42
# Expected output: Table containing market type number for every of the 28 currency pairs
library(tidyverse)
library(lubridate)
library(h2o)
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/evaluate_market_type.R")
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/to_m.R")

# Reading the data from the Sandbox
sbx <- "C:/Program Files (x86)/FxPro - Terminal2/MQL4/Files"
sbx_price <- file.path(sbx, "AI_CP15.csv")
sbx_macd <- file.path(sbx, "AI_Macd15.csv")

#price <- read_csv(sbx_price, col_names = F)
macd <- read_csv(sbx_macd, col_names = F, col_types = "cdddddddddddddddddddddddddddd")
macd$X1 <- ymd_hms(macd$X1)

# Vector of currency pairs
Pairs = c("EURUSD", "GBPUSD", "AUDUSD", "NZDUSD", "USDCAD", "USDCHF", "USDJPY",
          "EURGBP", "EURJPY", "EURCHF", "EURNZD", "EURCAD", "EURAUD", "GBPAUD",
          "GBPCAD", "GBPCHF", "GBPJPY", "GBPNZD", "AUDCAD", "AUDCHF", "AUDJPY",
          "AUDNZD", "CADJPY", "CHFJPY", "NZDJPY", "NZDCAD", "NZDCHF", "CADCHF")   

# Prepare data frame with last 64 observations and remove date/time column (6 hours)
macd_100 <- macd %>% select(c(X2:X29)) %>% head(64)

# Rename the column?
names(macd_100) <- Pairs

# initialize the virtual machine
h2o.init()

# test for all columns
for (PAIR in Pairs) {
  # PAIR <- "EURUSD"
  df <- macd_100 %>% select(PAIR)
  my_market <- evaluate_market_type(x = df,
                                    model_path = "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/regression.bin/DL_Regression",
                                    #model_path = "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models/classification.bin/DL_Classification",
                                    num_cols = 64) %>% as.data.frame()
  names(my_market) <- PAIR
  write_csv(my_market, file.path(sbx, paste0("AI_MarketType_", PAIR, ".csv")))
}

# shutdown
h2o.shutdown(prompt = F)
