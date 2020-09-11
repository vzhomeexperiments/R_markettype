# ----------------------------------------------------------------------------------------
# R Script to select and train the Deep Learning model on Financial Asset Time Series Data
# ----------------------------------------------------------------------------------------
# ## Manually select data into big matrix with label of 6 classes
# ----------------------------------------------------------------------------------------
#
# Supervised Deep Learning Classification Modelling
#
# load libraries to use and custom functions
library(tidyverse)
library(h2o)
library(lubridate)
library(plotly)
library(lazytrade)
#source("to_m.R")

#### Read asset prices and indicators ==========================================
# load prices of 28 currencies
prices <- read_csv(file.path("inst/extdata", "AI_CP60-12000.csv"), col_names = F)
prices$X1 <- ymd_hms(prices$X1)
# load macd indicator of 28 currencies
macd <- read_csv(file.path("inst/extdata", "AI_Macd60-12000.csv"), col_names = F)
macd$X1 <- ymd_hms(macd$X1)

#### Manually Selecting data... =================================================
# Market Periods
# 1. Bull normal, BUN
# 2. Bull volatile, BUV
# 3. Bear normal, BEN
# 4. Bear volatile, BEV
# 5. Sideways quiet, RAN
# 6. Sideways volatile, RAV

##########################################################################
## ---------- # 1. Bull normal, BUN ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X5))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-01-01", X1 < "2018-01-20") %>% select(X1, X5)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X5))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X5) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X5.y, col = X5.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun1 <- macd_df %>% select(X5.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X6))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-10-01", X1 < "2019-01-02") %>% select(X1, X6)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X6))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X6) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X6.y, col = X6.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun2 <- macd_df %>% select(X6.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X7))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-02-20", X1 < "2018-04-15") %>% select(X1, X7)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X7))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X7) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X7.y, col = X7.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun3 <- macd_df %>% select(X7.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-05-20", X1 < "2019-07-20") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun4 <- macd_df %>% select(X9.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X24))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-01-10", X1 < "2019-03-02") %>% select(X1, X24)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X24))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X24) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X24.y, col = X24.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bun5 <- macd_df %>% select(X24.x) %>% to_m(64)
# --------------------

#########################################################################
# add new column to this matrix with value BUN
macd_m_bun1 <- transform(macd_m_bun1, M_T = "BUN")
macd_m_bun2 <- transform(macd_m_bun2, M_T = "BUN")
macd_m_bun3 <- transform(macd_m_bun3, M_T = "BUN")
macd_m_bun4 <- transform(macd_m_bun4, M_T = "BUN")
macd_m_bun5 <- transform(macd_m_bun5, M_T = "BUN")
##########################################################################


##########################################################################
## ---------- # 2. Bull volatile, BUV ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X6))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-02-19", X1 < "2018-03-20") %>% select(X1, X6)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X6))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X6) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X6.y, col = X6.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv1 <- macd_df %>% select(X6.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X7))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-04-18", X1 < "2018-05-10") %>% select(X1, X7)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X7))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X7) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X7.y, col = X7.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv2 <- macd_df %>% select(X7.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-05-08", X1 < "2019-05-20") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv3 <- macd_df %>% select(X9.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-04-01", X1 < "2019-04-22") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv4 <- macd_df %>% select(X11.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X12))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-07-23", X1 < "2019-08-07") %>% select(X1, X12)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X12))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X12) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X12.y, col = X12.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv5 <- macd_df %>% select(X12.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X14))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-08-20", X1 < "2018-09-07") %>% select(X1, X14)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X14))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X14) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X14.y, col = X14.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv6 <- macd_df %>% select(X14.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X14))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-07-20", X1 < "2019-08-07") %>% select(X1, X14)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X14))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X14) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X14.y, col = X14.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv7 <- macd_df %>% select(X14.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X15))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-03-13", X1 < "2018-03-23") %>% select(X1, X15)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X15))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X15) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X15.y, col = X15.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv8 <- macd_df %>% select(X15.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X23))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-03-23", X1 < "2019-04-23") %>% select(X1, X23)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X23))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X23) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X23.y, col = X23.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_buv9 <- macd_df %>% select(X23.x) %>% to_m(64)
# --------------------

#########################################################################
macd_m_buv1 <- transform(macd_m_buv1, M_T = "BUV") 
macd_m_buv2 <- transform(macd_m_buv2, M_T = "BUV")
macd_m_buv3 <- transform(macd_m_buv3, M_T = "BUV") 
macd_m_buv4 <- transform(macd_m_buv4, M_T = "BUV") 
macd_m_buv5 <- transform(macd_m_buv5, M_T = "BUV")
macd_m_buv6 <- transform(macd_m_buv6, M_T = "BUV")
macd_m_buv7 <- transform(macd_m_buv7, M_T = "BUV")
macd_m_buv8 <- transform(macd_m_buv8, M_T = "BUV")
macd_m_buv9 <- transform(macd_m_buv9, M_T = "BUV") 
#########################################################################


##########################################################################
## ---------- # 3. Bear normal, BEN ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X3))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-11-19", X1 < "2018-12-15") %>% select(X1, X3)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X3))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X3) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X3.y, col = X3.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben1 <- macd_df %>% select(X3.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X5))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-03-19", X1 < "2019-06-01") %>% select(X1, X5)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X5))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X5) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X5.y, col = X5.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben2 <- macd_df %>% select(X5.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X10))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-04-19", X1 < "2019-06-01") %>% select(X1, X10)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X10))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X10) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X10.y, col = X10.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben3 <- macd_df %>% select(X10.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-05-10", X1 < "2019-06-01") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben4 <- macd_df %>% select(X11.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X17))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-05-20", X1 < "2019-07-20") %>% select(X1, X17)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X17))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X17) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X17.y, col = X17.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ben5 <- macd_df %>% select(X17.x) %>% to_m(64)
# --------------------

#########################################################################
macd_m_ben1 <- transform(macd_m_ben1, M_T = "BEN")
macd_m_ben2 <- transform(macd_m_ben2, M_T = "BEN")
macd_m_ben3 <- transform(macd_m_ben3, M_T = "BEN")
macd_m_ben4 <- transform(macd_m_ben4, M_T = "BEN")
macd_m_ben5 <- transform(macd_m_ben5, M_T = "BEN")

#########################################################################


##########################################################################
## ---------- # 4. Bear volatile, BEV ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-04-22", X1 < "2018-05-10") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev1 <- macd_df %>% select(X2.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X3))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-04-22", X1 < "2018-05-05") %>% select(X1, X3)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X3))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X3) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X3.y, col = X3.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev2 <- macd_df %>% select(X3.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-05-15", X1 < "2018-06-01") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev3 <- macd_df %>% select(X11.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X12))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-10-15", X1 < "2018-11-01") %>% select(X1, X12)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X12))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X12) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X12.y, col = X12.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev4 <- macd_df %>% select(X12.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X13))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-03-15", X1 < "2018-04-15") %>% select(X1, X13)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X13))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X13) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X13.y, col = X13.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev5 <- macd_df %>% select(X13.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X16))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2019-05-13", X1 < "2019-05-23") %>% select(X1, X16)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X16))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X16) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X16.y, col = X16.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev6 <- macd_df %>% select(X16.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X24))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-02-05", X1 < "2018-02-19") %>% select(X1, X24)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X24))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X24) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X24.y, col = X24.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_bev7 <- macd_df %>% select(X24.x) %>% to_m(64)
# --------------------
#########################################################################
macd_m_bev1 <- transform(macd_m_bev1, M_T = "BEV")
macd_m_bev2 <- transform(macd_m_bev2, M_T = "BEV")
macd_m_bev3 <- transform(macd_m_bev3, M_T = "BEV")
macd_m_bev4 <- transform(macd_m_bev4, M_T = "BEV")
macd_m_bev5 <- transform(macd_m_bev5, M_T = "BEV")
macd_m_bev6 <- transform(macd_m_bev6, M_T = "BEV")
macd_m_bev7 <- transform(macd_m_bev7, M_T = "BEV")
#########################################################################


##########################################################################
## ---------- # 5. Sideways quiet, RAN ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-02-20", X1 < "2018-04-10") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran1 <- macd_df %>% select(X2.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X4))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-07-01", X1 < "2018-08-10") %>% select(X1, X4)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X4))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X4) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X4.y, col = X4.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran2 <- macd_df %>% select(X4.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-06-25", X1 < "2018-07-10") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran3 <- macd_df %>% select(X9.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X12))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-06-25", X1 < "2018-07-10") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_ran3 <- macd_df %>% select(X9.x) %>% to_m(64)
# --------------------
#########################################################################
macd_m_ran1 <- transform(macd_m_ran1, M_T = "RAN") 
macd_m_ran2 <- transform(macd_m_ran2, M_T = "RAN") 
macd_m_ran3 <- transform(macd_m_ran3, M_T = "RAN") 
#########################################################################

##########################################################################
## ---------- # 6. Sideways volatile, RAV ---------------
##########################################################################
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X2))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-02-01", X1 < "2018-04-01") %>% select(X1, X2)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X2))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X2) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X2.y, col = X2.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_rav1 <- macd_df %>% select(X2.x) %>% to_m(64)
# --------------------
# --------------------
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-11-01", X1 < "2017-12-01") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m_rav2 <- macd_df %>% select(X9.x) %>% to_m(64)
# --------------------

#########################################################################
macd_m_rav1 <- transform(macd_m_rav1, M_T = "RAV")
macd_m_rav2 <- transform(macd_m_rav2, M_T = "RAV")
#########################################################################
#########################################################################
#########################################################################

# Combine all of that :)
macd_ML2 <- rbind(macd_m_bun1,macd_m_bun2,macd_m_bun3,macd_m_bun4,macd_m_bun5,
                  macd_m_buv1,macd_m_buv2,macd_m_buv3,macd_m_buv4,macd_m_buv5,macd_m_buv6,macd_m_buv7,macd_m_buv8,macd_m_buv9,
                  macd_m_ben1,macd_m_ben2,macd_m_ben3,macd_m_ben4,macd_m_ben5,
                  macd_m_bev1,macd_m_bev2,macd_m_bev3,macd_m_bev4,macd_m_bev5,macd_m_bev6,macd_m_bev7,
                  macd_m_ran1,macd_m_ran2,macd_m_ran3,
                  macd_m_rav1,macd_m_rav2)

### NOTE Number of rows Matrices needs to be roughly equal

# Record data into the folder
write_rds(macd_ML2, "PROD1/data_initial/macd_ML2.rds")

## Visualize new matrix in 3D
plot_ly(z = as.matrix(macd_ML2[,1:64]), type = "surface")

#### End