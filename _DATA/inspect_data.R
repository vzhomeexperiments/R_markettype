# manipulating datasets ;)

# how to retrieve dataset?

#A. using data set in the file macd_ML2.rds
DF1 <- readr::read_rds('_DATA/macd_ML2.rds')

#B. using data in the package lazytrade
library(lazytrade)
data("macd_ML60M")

# how to convert column to factor
macd_ML60M$M_T <- as.factor(macd_ML60M$M_T)

# Summary
summary(macd_ML60M$M_T)
