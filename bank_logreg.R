Bank_data <- read.csv(file.choose(),sep=";")
View(Bank_data)
Bank_data <- na.omit(Bank_data)
View(Bank_data)
### Logistic Regression Model ###
Bank_Model <- glm(y~.,data=Bank_data,family="binomial")
summary(Bank_Model)
## Summary of Bank_Model shows age,marital,default,pdays,previous are not significant
# So we can reject that variables and perform the logistic model
Bank <- Bank_data[,-c(1,3,5,14,15)]
View(Bank)
Bank_Model1 <- glm(y~.,data=Bank,family="binomial")
summary(Bank_Model1)

# Confusion Matrix Table
proba <- predict(Bank_Model1,type=c("response"),Bank)
proba

Confusion_Bank <-table(proba>0.5,Bank$y) 
Confusion_Bank
## Accuracy
Accuracy_Model <- sum(diag(Confusion_Bank))/sum(Confusion_Bank)
Accuracy_Model
## Model has accuracy of 90.17%
sum(Confusion_Bank[cbind(2:1,1:2)])/sum(Confusion_Bank)

predict_values <- NULL
Yes_No <- NULL
for(i in 1:45211){
  predict_values[i] <- ifelse(proba[i]>=0.5,1,0)
  Yes_No[i] <- ifelse(proba[i]>=0.5,"Yes","No")
}
proba <- Bank_data[,"proba"] 
View(proba)
predict_values <- Bank_data[,"predict_values"] 
View(predict_values)
Yes_No <- Bank_data[,"Yes_No"] 
View(Bank_data)
table_Bank <- table(Bank_data$Yes_No)
table_Bank

# ROC Curve 
library(ROCR)
rocrpred<-prediction(proba,Bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
