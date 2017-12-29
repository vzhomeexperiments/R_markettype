### PURPOSE: Function that uses Deep Learning model and Time Series Column of the dataframe
#            to find out specific market type of the financial asset
#            it will also discard bad result outputting -1 if it is the case

# x -< dataframe with one column containing asset indicator in descending order
# model_path <- path to the model
# num_cols - number of columns (features) in the final vector input to the model
# note: it is mandatory to switch on the virtual h2o machine with h2o.init()
#       also to shut it down with h2o.shutdown(prompt = F)

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
