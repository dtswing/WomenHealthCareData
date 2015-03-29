setwd("~/Siying/WomenHealthCareData")

#load libraries
library(data.table)
library(dismo)
library(Amelia)

#input datasets
train.values <- read.csv('train_values.csv')
train.labels <- read.csv('train_labels.csv')
test.values <- read.csv('test_values.csv')

#traning set
train.combined <- cbind(train.labels[,2],train.values[,-1])
colnames(train.combined) <- c(names(train.labels)[2],names(train.values)[-1])

#remove variables with only missing values
train.combined <- train.combined[,colSums(is.na(train.combined) | train.combined == 0)<nrow(train.combined)]
train.combined <- train.combined[rowSums(is.na(train.combined) | train.combined == 0)<ncol(train.combined),] 
#remove variables with same values 
uniquelength <- sapply(train.combined,function(x) length(unique(x)))
train.combined <- subset(train.combined, select=uniquelength>1)

#summary of training set
summary(train.combined)

#Small training set with no missing values
train.combined.nonmissing <- na.omit(train.combined)

#Generate dataset excluding variables with x% of missing values
train.combined.v2 <- train.combined[,colSums(is.na(train.combined) | train.combined == 0 | train.combined == '') < nrow(train.combined)*0.6] 

#Data imputation
#Define varialbes for imputation
colNumeric <- names(train.combined.v2)[grepl('n_', names(train.combined.v2))]
colOrdinal <- names(train.combined.v2)[grepl('o_', names(train.combined.v2))]
colCategorical <- names(train.combined.v2)[grepl('c_', names(train.combined.v2))]

#Take empty categorical variables as NAs
train.combined.v2[,colCategorical] <- lapply(train.combined.v2[,colCategorical], function(x) ifelse(is.na(x)|x=='','NA',x))
train.combined.v2[,colCategorical] <- lapply(train.combined.v2[,colCategorical], factor)
summary(train.combined.v2[,colCategorical])

#Impute numerical and oridanl variables
train.impute.prep <- train.combined.v2[,c(colNumeric, colOrdinal)]
train.impute.prep <- train.impute.prep[rowSums(is.na(train.impute.prep) | train.impute.prep == 0)<ncol(train.impute.prep),]
train.imputed <- amelia(train.impute.prep,ords = colOrdinal, m=5, incheck = FALSE)

save(train.imputed, file = "imputationsNumericOrdinalOnly.RData") 

#Naive logsitic regression

#Random Forest 

#Gradient Boosted Tree
build.tc5.lr01 <- gbm.step(data=train.combined.nonmissing, gbm.x = names(train.combined.nonmissing)[names(train.combined.nonmissing)!="service_a"], gbm.y = "service_a",
                           family = "bernoulli", tree.complexity = 5,
                           learning.rate = 0.01, bag.fraction = 0.5)
summary(build.tc5.lr01)

#Naives Bayes


