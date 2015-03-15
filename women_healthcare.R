setwd("~/Siying/WomenHealthCareData")

train.values <- read.csv('train_values.csv')
train.labels <- read.csv('train_labels.csv')
test.values <- read.csv('test_values.csv')

View(head(train.values))
head(train.labels)
