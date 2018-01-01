# ----------------------------------------------------------------------------------------
# R Script to select and train the Deep Learning model on Financial Asset Time Series Data
# ----------------------------------------------------------------------------------------
# ## Manually select data into big matrix with label of 6 classes
# ----------------------------------------------------------------------------------------

# Market Type 1 Bull Normal
library(tidyverse)
library(h2o)
library(lubridate)
library(plotly)

#### Read asset prices and indicators ==========================================
# load prices of 28 currencies
prices <- read_csv("AI_CP15.csv", col_names = F)
prices$X1 <- ymd_hms(prices$X1)
# load macd indicator of 28 currencies
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
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X3))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-11-05", X1 < "2017-11-25") %>% select(X1, X3)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X3))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X3) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X3.y, col = X3.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
source("to_m.R")
macd_m <- macd_df %>% select(X3.x) %>% to_m(32)

#########################################################################

# add new column to this matrix with value 1
macd_m_1 <- transform(macd_m, M_T = 1)

##########################################################################

# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X4))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-10-02", X1 < "2017-10-07") %>% select(X1, X4)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X4))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X4) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X4.y, col = X4.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
source("to_m.R")
macd_m <- macd_df %>% select(X4.x) %>% to_m(32)

#########################################################################
macd_m_2 <- transform(macd_m, M_T = 2) 
#########################################################################

# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X12))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-09-20", X1 < "2017-10-20") %>% select(X1, X12)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X12))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X12) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X12.y, col = X12.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
source("to_m.R")
macd_m <- macd_df %>% select(X12.x) %>% to_m(32)

#########################################################################
macd_m_3 <- transform(macd_m, M_T = 3)
#########################################################################

# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X6))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-10-18", X1 < "2017-10-30") %>% select(X1, X6)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X6))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X6) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X6.y, col = X6.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
source("to_m.R")
macd_m <- macd_df %>% select(X6.x) %>% to_m(32)

#########################################################################
macd_m_4 <- transform(macd_m, M_T = 4)
#########################################################################

# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X11))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-09-20", X1 < "2017-10-24") %>% select(X1, X11)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X11))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X11) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X11.y, col = X11.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
source("to_m.R")
macd_m <- macd_df %>% select(X11.x) %>% to_m(32)

#########################################################################
macd_m_5 <- transform(macd_m, M_T = 5) 
#########################################################################
# Choose the asset corresponding to this period /find by replacing 'y' argument/
ggplot(prices, aes(X1, X13))+geom_line()

# Extract approximate date and choose only relevant columns
price_df <- prices %>% filter(X1 > "2017-10-10", X1 < "2017-11-20") %>% select(X1, X13)

# Visualize it to confirm 
ggplot(price_df, aes(X1, X13))+geom_line()

# Extract corresponding piece of macd dataframe:
macd_df <- macd %>% select(X1, X13) %>% inner_join(price_df, by = c("X1" = "X1"))

# Visualize both things together
ggplot(macd_df, aes(X1, X13.y, col = X13.x))+geom_line()

# transform to matrix, number of columns will correspond to model sensitivity e.g. 100 columns ~ 24 Hours
source("to_m.R")
macd_m <- macd_df %>% select(X13.x) %>% to_m(32)

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
#plot_ly(z = macd_ML1[,1:32], type = "surface")
#### Fitting Deep Learning Net =================================================
## Fit model now:
# start h2o virtual machine
h2o.init()
# load data into h2o environment
macd_ML  <- as.h2o(x = macd_ML1, destination_frame = "macd_ML")

# fit models from simplest to more complex
ModelA <- h2o.deeplearning(
  x = names(macd_ML[,1:32]), 
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
  stopping_metric = "MSE",
  #balance_classes = T,
  epochs = 600)

ModelA
summary(ModelA)
plot(h2o.performance(ModelA))

# to return predicted classes
predicted <- h2o.predict(ModelA, macd_ML) %>% as.data.frame()


## Save the model
if(!file.exists("models/regression.bin")){
h2o.saveModel(ModelA, "models/regression.bin")
}
# shutdown the virtual machine
h2o.shutdown(prompt = F)

#### End