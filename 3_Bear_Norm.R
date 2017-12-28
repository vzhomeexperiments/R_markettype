# ----------------------------------------------------------------------------------------
# R Script to select and train the Deep Learning model on Financial Asset Time Series Data
# ----------------------------------------------------------------------------------------

# Market Type 2 Bull Volat
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

#### Fitting Deep Learning Net =================================================
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
macd_m <- macd_df %>% select(X12.x) %>% to_m(50)

## Visualize new matrix in 3D
plot_ly(z = macd_m, type = "surface")

## Fit model now:
# start h2o virtual machine
h2o.init()
# load data into h2o environment
macd_vm  <- as.h2o(x = macd_m, destination_frame = "macd_m")

# fit models from simplest to more complex
ModelA <- h2o.deeplearning(
  x = names(macd_vm), 
  training_frame = macd_vm, 
  activation = "Tanh", 
  autoencoder = TRUE, 
  hidden = c(30,10,30), 
  sparse = TRUE,
  l1 = 1e-4, 
  epochs = 100)
ModelB <- h2o.deeplearning(
  x = names(macd_vm), 
  training_frame = macd_vm, 
  activation = "Tanh", 
  autoencoder = TRUE, 
  hidden = c(50,20,50), 
  sparse = TRUE,
  l1 = 1e-4, 
  epochs = 100)
ModelC <- h2o.deeplearning(
  x = names(macd_vm), 
  training_frame = macd_vm, 
  activation = "Tanh", 
  autoencoder = TRUE, 
  hidden = c(70,20,70), 
  sparse = TRUE,
  l1 = 1e-4, 
  epochs = 100)
ModelD <- h2o.deeplearning(
  x = names(macd_vm), 
  training_frame = macd_vm, 
  activation = "Tanh", 
  autoencoder = TRUE, 
  hidden = c(90,20,90), 
  sparse = TRUE,
  l1 = 1e-4, 
  epochs = 100)
ModelE <- h2o.deeplearning(
  x = names(macd_vm), 
  training_frame = macd_vm, 
  activation = "Tanh", 
  autoencoder = TRUE, 
  hidden = c(100,50, 20,50, 100), 
  sparse = TRUE,
  l1 = 1e-4, 
  epochs = 100)
ModelF <- h2o.deeplearning(
  x = names(macd_vm), 
  training_frame = macd_vm, 
  activation = "Tanh", 
  autoencoder = TRUE, 
  hidden = c(100,70, 20,70, 100), 
  sparse = TRUE,
  l1 = 1e-4, 
  epochs = 100)

mod_errA <- h2o.anomaly(ModelA, macd_vm) %>% as.data.frame() %>% summarise(mean_mse = mean(Reconstruction.MSE))
mod_errB <- h2o.anomaly(ModelB, macd_vm) %>% as.data.frame() %>% summarise(mean_mse = mean(Reconstruction.MSE))
mod_errC <- h2o.anomaly(ModelC, macd_vm) %>% as.data.frame() %>% summarise(mean_mse = mean(Reconstruction.MSE))
mod_errD <- h2o.anomaly(ModelD, macd_vm) %>% as.data.frame() %>% summarise(mean_mse = mean(Reconstruction.MSE))
mod_errE <- h2o.anomaly(ModelE, macd_vm) %>% as.data.frame() %>% summarise(mean_mse = mean(Reconstruction.MSE))
mod_errF <- h2o.anomaly(ModelF, macd_vm) %>% as.data.frame() %>% summarise(mean_mse = mean(Reconstruction.MSE))

errors = c(mod_errA, mod_errB, mod_errC, mod_errD, mod_errE, mod_errF)

## Save the model
if(!file.exists("models/3_bear_norm_v2.bin")){
h2o.saveModel(ModelA, "models/3_bear_norm_v2.bin")
}
# shutdown the virtual machine
h2o.shutdown(prompt = F)

#### End