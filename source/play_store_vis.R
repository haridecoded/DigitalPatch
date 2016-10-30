install.packages('dplyr')
install.packages('ggplot2')

library(dplyr)
library(ggplot2)
library(data.table)

data = fread('playstore.csv')

x <- c('smoking', 'nicotine', 'alcohol', 'depression', 'health', 'substance')
y <- c('smoking', 'nicotine', 'alcohol', 'depression', 'health', 'substance')
apps <- data <- expand.grid(X=x, Y=y)

