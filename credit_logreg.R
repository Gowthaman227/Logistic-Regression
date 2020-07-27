Credit <- read.csv(file.choose())
View(Credit)
Credit <- Credit[,-1]
View(Credit)
Credit <- na.omit(Credit)
Credit$card <- ifelse(Credit$card=="yes",1,0)
Credit$owner <- ifelse(Credit$owner=="yes",1,0)
Credit$selfemp <- ifelse(Credit$selfemp=="yes",1,0)
View(Credit)
### Logistic Regression Model ###
Credit_Model <- glm(card~reports+income+share+expenditure+selfemp+dependents+months+majorcards+active,data=Credit,family="binomial",control=list(maxit=120))
summary(Credit_Model)
?glm()
# Confusion matrix table 
prob <- predict(Credit_Model,type=c("response"),Credit)
prob

confusion<-table(prob>0.5,Credit$card)
confusion

confusion# Model Accuracy 
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy
## Model has Accuracy of 88.93%
sum(confusion[cbind(2:1, 1:2)])/sum(confusion)


pred_values <- NULL
yes_no <- NULL
for (i in 1:1319){
  pred_values[i] <- ifelse(prob[i]>=0.5,1,0)
  yes_no[i] <- ifelse(prob[i]>=0.5,"yes","no")
}

prob <- Credit[,"prob"] 
View(prob)
pred_values <- Credit[,"pred_values"] 
View(pred_values)
yes_no <- Credit[,"yes_no"] 
View(Credit)
table <- table(Credit$yes_no)
table
### 889 Customer application has been accepted for credit card and 430 has been
# rejected.

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,Credit$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
