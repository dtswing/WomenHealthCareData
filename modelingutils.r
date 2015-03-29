LogLoss <- function(actual, predicted, eps=0.00001) {
  predicted <- pmin(pmax(predicted, eps), 1-eps)
  -1/length(actual)*(sum(actual*log(predicted)+(1-actual)*log(1-predicted)))
}

calculateAUC_rf<-function(model,build,bad){
  b<-build
  b$predicted<-as.numeric(predict(model,type="prob",build)[,2])
  b$actual<-as.numeric(b[,bad])-1
  b$errors<-b$actual-b$predicted
  mse<-mean(b$errors^2)
  
  b$predict<- predict(model,b, type = "prob")
  b$predict2<- floor(b$predict[,2]*10)/10
  #xtabs(~predict2+bad,data=b)
  
  pred_obj = prediction(b$predict[,2], b[,bad])
  perf <- performance(pred_obj, measure = "tpr", x.measure = "fpr")
  #plot(perf, col=rainbow(10)) #red line plot
  auc = round(performance(pred_obj, "auc")@y.values[[1]], digits=3)
  #print(paste("Test set AUC:",auc))
  auc
}
