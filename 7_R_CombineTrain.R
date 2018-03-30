# ----------------------------------------------------------------------------------------
# R Script to select and train the Deep Learning model on Financial Asset Time Series Data
# ----------------------------------------------------------------------------------------
# ## Manually select data into big matrix with label of 6 classes
# ----------------------------------------------------------------------------------------
#
# Supervised Deep Learning Regression Modelling
#
# load libraries to use and custom functions
library(tidyverse)
library(h2o)
library(lubridate)
library(plotly)
source("to_m.R")

#### Read asset prices and indicators ==========================================
# load prices of 28 currencies
prices <- read_csv("AI_CP15.csv", col_names = F)
prices$X1 <- ymd_hms(prices$X1)
# load macd indicator of 28 currencies /use "AI_Macd15.csv" or "AI_Stoch15.csv" or... other data...
macd <- read_csv("AI_Macd15.csv", col_names = F)
macd$X1 <- ymd_hms(macd$X1)

#### Manually Selecting data... =================================================
# Market Periods
# 1. Bull normal
# 2. Bull volatile
# 3. Bear normal
# 4. Bear volatile
# 5. Sideways quiet
# 6. Sideways volatile
##########################################################################
## ---------- # 1. Bull normal, BUN ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X5))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-11-05", X1 < "2018-01-20") %>% select(X1, X5)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X5))+geom_line()

# Extract corresponding piece of indicator dataframe:
macd_df <- macd %>% select(X1, X5) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X5.y, col = X5.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X5.x) %>% to_m(64)
#########################################################################
# add new column to this matrix with value BUN
macd_m_1 <- transform(macd_m, M_T = 1)
##########################################################################


##########################################################################
## ---------- # 2. Bull volatile, BUV ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X15))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-03-10", X1 < "2018-04-25") %>% select(X1, X15)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X15))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X15) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X15.y, col = X15.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X15.x) %>% to_m(64)

#########################################################################
macd_m_2 <- transform(macd_m, M_T = 2) 
#########################################################################


##########################################################################
## ---------- # 3. Bear normal, BEN ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X29))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-01-10", X1 < "2018-03-02") %>% select(X1, X29)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X29))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X29) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X29.y, col = X29.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X29.x) %>% to_m(64)

#########################################################################
macd_m_3 <- transform(macd_m, M_T = 3)
#########################################################################


##########################################################################
## ---------- # 4. Bear volatile, BEV ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X7))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-01-10", X1 < "2018-02-01") %>% select(X1, X7)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X7))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X7) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X7.y, col = X7.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X7.x) %>% to_m(64)

#########################################################################
macd_m_4 <- transform(macd_m, M_T = 4)
#########################################################################


##########################################################################
## ---------- # 5. Sideways quiet, RAN ---------------
##########################################################################
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
macd_m <- macd_df %>% select(X2.x) %>% to_m(64)

#########################################################################
macd_m_5 <- transform(macd_m, M_T = 5) 
#########################################################################

##########################################################################
## ---------- # 6. Sideways volatile, RAV ---------------
##########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X9))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2018-01-01", X1 < "2018-11-20") %>% select(X1, X9)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X9))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X9) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X9.y, col = X9.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
macd_m <- macd_df %>% select(X9.x) %>% to_m(64)

#########################################################################
macd_m_6 <- transform(macd_m, M_T = 6)
#########################################################################
#########################################################################
#########################################################################

# Combine all of that :)
macd_ML1 <- rbind(macd_m_1,macd_m_2,macd_m_3,macd_m_4,macd_m_5,macd_m_6)

### NOTE Number of rows Matrices needs to be roughly equal(?)
#macd_ML1$M_T <- as.factor(macd_ML1$M_T)


## Visualize new matrix in 3D
plot_ly(z = as.matrix(macd_ML1[,1:32]), type = "surface")
#### Fitting Deep Learning Net =================================================
## Fit model now:
# start h2o virtual machine
h2o.init()
# load data into h2o environment
macd_ML  <- as.h2o(x = macd_ML1, destination_frame = "macd_ML")

# fit models from simplest to more complex
ModelR <- h2o.deeplearning(
  model_id = "DL_Regression",
  x = names(macd_ML[,1:64]), 
  y = "M_T",
  training_frame = macd_ML,
  activation = "Tanh",
  overwrite_with_best_model = TRUE, 
  autoencoder = FALSE, 
  hidden = c(100,100), 
  loss = "Automatic",
  sparse = TRUE,
  l1 = 1e-4,
  distribution = "AUTO",
  stopping_metric = "AUTO",
  #balance_classes = T,
  epochs = 200)

#ModelR
summary(ModelR)
h2o.performance(ModelR)

# to return predicted classes
predicted <- h2o.predict(ModelR, macd_ML) %>% as.data.frame()


## Save the model
h2o.saveModel(ModelR, "models/regression.bin", force = TRUE)

# shutdown the virtual machine
h2o.shutdown(prompt = F)

#### End