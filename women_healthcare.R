setwd("~/Siying/WomenHealthCareData")

#load libraries
library(data.table)
library(dismo)

#input datasets
train.values <- read.csv('train_values.csv')
train.labels <- read.csv('train_labels.csv')
test.values <- read.csv('test_values.csv')

#traning set
train.combined <- cbind(train.labels[,2],train.values[,-1])
colnames(train.combined) <- c(names(train.labels)[2],names(train.values)[-1])

#remove variables with only missing values
train.combined <- train.combined[,colSums(is.na(train.combined) | train.combined == 0)<nrow(train.combined)] 

#Naive logsitic regression

#Random Forest 

#Gradient Boosted Tree
build.tc5.lr01 <- gbm.step(data=train.combined, gbm.x = names(train.combined)[names(train.combined)!="service_a"], gbm.y = "service_a",
                           family = "bernoulli", tree.complexity = 5,
                           learning.rate = 0.01, bag.fraction = 0.5)
summary(build.tc5.lr01)

#Naives Bayes


