setwd("~/Siying/WomenHealthCareData")
source("modelingutils.r")

#load libraries
library(data.table)
library(dismo)
library(Amelia)
library(randomForest)
library(ROCR)
library(caret)

#input datasets
train.values <- read.csv('train_values.csv')
train.labels <- read.csv('train_labels.csv')
test.values <- read.csv('test_values.csv')

#traning set
train.combined <- cbind(train.labels,train.values)
train.combined$id <- NULL

#remove variables with only missing values
train.combined <- train.combined[,colSums(is.na(train.combined) | train.combined == 0)<nrow(train.combined)]
train.combined <- train.combined[rowSums(is.na(train.combined) | train.combined == 0)<ncol(train.combined),] 
#remove variables with same values 
uniquelength <- sapply(train.combined,function(x) length(unique(x)))
train.combined <- subset(train.combined, select=uniquelength>1)

#summary of training set
summary(train.combined)

#Generate dataset excluding variables with x% of missing values
train.combined.v2 <- train.combined[,colSums(is.na(train.combined)| train.combined == '') < nrow(train.combined)*0.7] 

#Data imputation
colNumeric <- names(train.combined.v2)[grepl('n_', names(train.combined.v2))]
colOrdinal <- names(train.combined.v2)[grepl('o_', names(train.combined.v2))]
colCategorical <- names(train.combined.v2)[grepl('c_', names(train.combined.v2))]
colRelease <- names(train.combined.v2)[grepl('release', names(train.combined.v2))]
colService <- names(train.combined.v2)[grepl('service_', names(train.combined.v2))]

#Take empty categorical variables as NAs
train.combined.v2[,colCategorical] <- lapply(train.combined.v2[,colCategorical], function(x) ifelse(is.na(x)|x=='',NA,x))
train.combined.v2[,colCategorical] <- lapply(train.combined.v2[,colCategorical], factor)
summary(train.combined.v2[,colCategorical])

#Impute numerical and oridanl variables
train.impute.prep <- train.combined.v2[,c(colNumeric, colOrdinal, colCategorical, colRelease)]
train.impute.prep <- train.impute.prep[rowSums(is.na(train.impute.prep) | train.impute.prep == '')<ncol(train.impute.prep),]
train.imputed <- amelia(train.impute.prep, noms = c(colRelease,colCategorical) ords = colOrdinal,m=5, incheck = FALSE)

save(train.imputed, file = "imputationsNumericOrdinalOnly.RData") 

train.combined.v2[,colService] <- lapply(train.combined.v2[,colService], factor)
train.combined.v2$release <- as.factor(train.combined.v2$release)
train.combined.v2$id <- NULL

#Random Forest 

#K-fold validation
folds <- createFolds(train.set$service_a, k=20)
split_up <- lapply(folds, function(ind, dat) dat[ind,], dat = train.set)

aucs <- NULL
for (i in 1:10){
  test <- split_up[[i]]
  train <- train.set[-as.numeric(row.names(test)),]
  rf_fit <- randomForest(service_a~., data=train, maxnodes=20, importance=TRUE, na.action=na.omit)
  
  pred_train <- predict(rf_fit, train, type = "prob")[,2]
  pred_obj_train <- prediction(pred_train, train$service_a)
  
  pred_test <- predict(rf_fit, test, type = "prob")[,2]
  pred_obj_test <- prediction(pred_test, test$service_a)
  
  aucs <- rbind(aucs,c(round(performance(pred_obj_test, "auc")@y.values[[1]], digits=3), round(performance(pred_obj_train, "auc")@y.values[[1]], digits=3)))
}

aucs <- data.frame(aucs)
names(aucs) <- c("validate_auc", "build_auc")
print(paste0("validate: ", median(aucs$validate_auc), "   build: ",median(aucs$build_auc)))


#fit on all observastions

train.set <- cbind(train.combined.v2[,colService], train.combined.v2$release, train.imputed$imputations[[1]],train.combined.v2[,colCategorical])
colnames(train.set) <- c(colService,colRelease,names(train.imputed$imputations[[1]]),colCategorical)

y_pred <- NULL
auc_pred <- NULL
fit_list <- NULL

for (y in c('service_a','service_b','service_c')){
rf_fit <- randomForest(train.set[,y] ~., data=train.set[,names(train.set)[!names(train.set) %in% colService]], maxnodes=20, importance=TRUE, na.action=na.omit)
fit_list <- list(fit_list, rf_fit)
pred_train_all <- predict(rf_fit, train.set, type = "prob")[,2]
pred_obj_train_all <- prediction(pred_train_all, train.set[,y])
auc_train_all <- round(performance(pred_obj_train_all, "auc")@y.values[[1]], digits=3)
logloss_train <- LogLoss(as.numeric(train.set[,y])-1,pred_train_all)
y_pred <- cbind(y_pred, pred_train_all)
auc_pred <- cbind(auc_pred,auc_train_all)
} 

#GLM

#Gradient Boosted Tree
#build.tc5.lr01 <- gbm.step(data=train.combined.nonmissing, gbm.x = names(train.combined.nonmissing)[names(train.combined.nonmissing)!="service_a"], gbm.y = "service_a",
#                           family = "bernoulli", tree.complexity = 5,
#                           learning.rate = 0.01, bag.fraction = 0.5)
#summary(build.tc5.lr01)

#Naives Bayes


