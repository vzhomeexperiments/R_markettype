# ----------------------------------------------------------------------------------------
# R Script to train the Deep Learning model on Financial Asset Time Series Data
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
#library(plotly)
source("C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/to_m.R")

#absolute path to store model objects (useful when scheduling tasks)
path_model <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/models"

data_update_path <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/TEST1/data_update"
data_initial_path <- "C:/Users/fxtrams/Documents/000_TradingRepo/R_markettype/TEST1/data_initial"

#### Manually Selecting data... =================================================
# see script 1_data_selection.R
# data stored in TEST1/data_initial/macd_ML2.rds
# Read data from recorded rds file
macd_ML2 <- read_rds(file.path(data_initial_path, "macd_ML2.rds")) %>% mutate_at("M_T", as.character)

#### Automatically selected and stored data... ==================================
macd_ai_classified <- read_rds(file.path(data_update_path, "macd_ai_classified.rds")) %>% 
# keep the joined data for the next update
  bind_rows(macd_ML2) %>% 
  # write the data to the data initial folder
  write_rds(file.path(data_initial_path, "macd_ML2.rds"))

# delete the data collected so far
file.remove(file.path(data_update_path, "macd_ai_classified.rds"))

# restore label column as factor
macd_ML2 <- macd_ai_classified %>% mutate_at("M_T", as.factor)
#### ============================================================================

# Market Periods
# 1. Bull normal, BUN
# 2. Bull volatile, BUV
# 3. Bear normal, BEN
# 4. Bear volatile, BEV
# 5. Sideways quiet, RAN
# 6. Sideways volatile, RAV


#### Fitting Deep Learning Net =================================================
## Fit model now:
# start h2o virtual machine
h2o.init()
# load data into h2o environment
macd_ML  <- as.h2o(x = macd_ML2, destination_frame = "macd_ML")

# fit models from simplest to more complex
ModelC <- h2o.deeplearning(
  model_id = "DL_Classification",
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

#ModelC
summary(ModelC)
h2o.performance(ModelC)

# to return predicted classes
predicted <- h2o.predict(ModelC, macd_ML)  %>% as.data.frame()

## Save the model, include logic to check the previous model and overwrite only if new model is better
### get the previously obtained model object
ModelPrev <- try(h2o.loadModel(file.path(path_model, "classification.bin/DL_Classification")),silent = T)
# perform comparison only if there is a previous model
if(!class(ModelPrev)[1] == "try-error"){
# get performance of previously obtained model
ModelPrevPerf <- h2o.performance(ModelPrev,newdata = macd_ML) %>% as.list.data.frame()
# get mean per class error
ModelPrevMeanPerClassError <- ModelPrevPerf@metrics$mean_per_class_error

### get the new model object metricx
# get performance of current model
ModelCPerf <- h2o.performance(ModelC) %>% as.list.data.frame()
# get mean per class error
ModelCMeanPerClassError <- ModelCPerf@metrics$mean_per_class_error

# compare metrics of both models
if(ModelCMeanPerClassError < ModelPrevMeanPerClassError) {
  # this will save new model if it's metrics are better
  h2o.saveModel(ModelC, file.path(path_model, "classification.bin"), force = TRUE)
}
  # this will save ModelC in case previous model did not existed
} else { h2o.saveModel(ModelC, file.path(path_model, "classification.bin"), force = TRUE) }



# shutdown the virtual machine
h2o.shutdown(prompt = F)

#### End