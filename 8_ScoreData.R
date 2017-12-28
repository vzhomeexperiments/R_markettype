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
macd_100 <- macd %>% head(50) %>% select(c(X2:X29))

# Rename the column?
names(macd_100) <- Pairs

# Solve for one
X <- macd_100 %>% select(EURUSD)


# load the virtual machine
library(h2o)

# initialize the virtual machine
h2o.init()

market_type_num <- function(x){
# x is a 1 column dataframe containing 100 values
# Convert to matrix
source("to_m.R")
X_m <- to_m(x, 50)  
# load the dataset 
test  <- as.h2o(x = X_m, destination_frame = "test")

# load all models
m1 <- h2o.loadModel("models/1_bull_norm_v2.bin/DeepLearning_model_R_1514456875628_1") 
m2 <- h2o.loadModel("models/2_bull_volat_v2.bin/DeepLearning_model_R_1514457608582_2") 
m3 <- h2o.loadModel("models/3_bear_norm_v2.bin/DeepLearning_model_R_1514457755690_1") 
m4 <- h2o.loadModel("models/4_bear_volat_v2.bin/DeepLearning_model_R_1514457975299_2") 
m5 <- h2o.loadModel("models/5_rang_norm_v2.bin/DeepLearning_model_R_1514458137225_1") 
m6 <- h2o.loadModel("models/6_rang_volat_v2.bin/DeepLearning_model_R_1514458239855_1") 

# retrieve the error on each
e1 <- h2o.anomaly(m1, test) %>% as.vector()
e2 <- h2o.anomaly(m2, test) %>% as.vector()
e3 <- h2o.anomaly(m3, test) %>% as.vector()
e4 <- h2o.anomaly(m4, test) %>% as.vector()
e5 <- h2o.anomaly(m5, test) %>% as.vector()
e6 <- h2o.anomaly(m6, test) %>% as.vector()

# join and find the minimum (best fit)
results <- c(e1, e2, e3, e4, e5, e6)

# element that has minimum value
element <- which.min(results)

return(element)

}

# test function
#my_market <- market_type_num(x = X)

# test for all columns
for (PAIR in Pairs) {
  df <- macd_100 %>% select(PAIR)
  my_market <- market_type_num(x = df) %>% as.data.frame()
  names(my_market) <- PAIR
  write_csv(my_market, file.path(sbx, paste0(PAIR, ".csv")))
}

#my_markets <- apply(macd_100, MARGIN = 2, market_type_num)


# shutdown
h2o.shutdown(prompt = F)

