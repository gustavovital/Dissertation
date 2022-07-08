# script providing Uit Root Test:
# 
# Gustavo Vital

# Packages

library(tidyverse)
library(urca)

# set wd ----
setwd('C:/Users/gusta/Documents/GitHub/dissertation/')

# read dataset
data <- read_csv('data\\data_model.csv')

data.test <- data[, c(2:7, 11, 13)]

sink(file = "data\\unit_root.txt")

cat('===================================================\n')
cat('Augmented-Dickey-Fuller\n')
cat('===================================================\n')
for(name in 1:length(names(data.test))){
  variable <- names(data.test)[name]
  control <- data.test[, name]
  
  for(test in c("none", "drift", "trend")){
    teste <- summary(ur.df(as.ts(control), type = test))
    cat('---------------------------------------------------\n')
    cat('TYPE OF THE TEST: ', test, '\n')
    cat('\n')
    cat('VARIABLE TO BE TESTED:', variable, '\n\n')
    cat('Confidence Intervals: ', teste@cval[1, 1], teste@cval[1, 2], teste@cval[1, 3], '\n')
    cat('P-value: ', teste@teststat[1], '\n')
  }
}

cat('===================================================\n')
cat('Phillips & Perron Unit Root Test\n')
cat('===================================================\n')
for(name in 1:length(names(data.test))){
  variable <- names(data.test)[name]
  control <- data.test[, name]
  
  for(test in c("constant", "trend")){
    teste <- summary(ur.pp(as.ts(control), model = test, type = 'Z-tau'))
    cat('---------------------------------------------------\n')
    cat('TYPE OF THE TEST: ', test, '\n')
    cat('\n')
    cat('VARIABLE TO BE TESTED:', variable, '\n\n')
    cat('Confidence Intervals: ', teste@cval[1, 1], teste@cval[1, 2], teste@cval[1, 3], '\n')
    cat('P-value: ', teste@teststat, '\n')
  }
}

sink(file = NULL)
