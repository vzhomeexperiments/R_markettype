# manipulating datasets ;)

# how to retrieve dataset?

#A. using data set in the file macd_ML2.rds
DF1 <- readr::read_rds('_DATA/macd_ML2.rds')
summary(DF1)

#B. using data in the package lazytrade
library(lazytrade)
data("macd_ML60M")

# how to convert column to factor
macd_ML60M$M_T <- as.factor(macd_ML60M$M_T)

# Summary
summary(macd_ML60M$M_T)

library(plotly)
## Visualize new matrix in 3D
# how to convert column to factor
macd_ai_classified$M_T <- as.factor(macd_ai_classified$M_T)
summary(macd_ai_classified$M_T)

plot_ly(z = as.matrix(macd_ai_classified[,1:64]), type = "surface")

library(magrittr)
df <- macd_ai_classified %>% dplyr::filter(M_T == "BEN")

plot_ly(z = as.matrix(macd_ai_classified[,1:64]), type = "surface")

# example of the plot:
#To Do create a Shiny APP to scroll through!
plot(x = 1:64, y = df[1, 1:64])



