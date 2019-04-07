#part1
getwd()

banking<-read.csv("C:/Users/Arvind/Desktop/Intermediate Analytics/Week 4/banking_data.csv")
str(banking)
install.packages("gmodels")
library(gmodels)

#part 2
logistic_model<-glm(formula = y~1,family = binomial(link="logit"),data=banking)
summary(logistic_model)
q<-logistic_model$coefficients
w<-exp(q)/(1+exp(q))
CrossTable(banking$y)

#part 3
logistic_model2 <- glm(y ~factor(education==0), family=binomial(link="logit"), data=banking)
summary(logistic_model2)
1.969+0.37821
logistic_model3<- glm(y ~factor(education==2), family=binomial(link="logit"), data=banking)
summary(logistic_model3)
-2.23852+0.33836
Success1<-exp(coef(logistic_model)[1]+coef(logistic_model)[2])
Success1
Success2<-exp(coef(logistic_model2)[1]+coef(logistic_model2)[2])
Success2
Sucess_ratio<-Success2/Success1
Sucess_ratio



#part 4

best_day<-glm(y~factor(day),family=binomial(link = "logit"),data=banking)
summary(best_day)
CrossTable(banking$day,banking$y)
prob1<-exp(coef(best_day)[1])
prob1
prob2<-exp(coef(best_day)[1]) + exp(coef(best_day)[2])
prob2
prob3<-exp(coef(best_day)[1]) + exp(coef(best_day)[3])
prob3
prob4<-exp(coef(best_day)[1]) + exp(coef(best_day)[4])
prob4
prob5<-exp(coef(best_day)[1]) + exp(coef(best_day)[5])
prob5

#probability for each day
prob1/(1+prob1)
prob2/(1+prob2)
prob3/(1+prob3)
prob4/(1+prob4)
prob5/(1+prob5)

#part 5
set.seed(123)
row.number <- sample(x=1:nrow(banking), size=0.8*nrow(banking))
train = banking[row.number,]
test = banking[-row.number,]
head(train)

model<-glm(y~.,data=train, family="binomial")
step(model,direction = "backward")

model2<-glm(y ~ X + age + marital + education + occupation + 
            +default + contact + duration + campaign + pdays + poutcome,family = "binomial", data = train)
model2

#predicted values of outcome from test dataset
predicted_value<-predict(model2,newdata = test,type = "response")
predicted_value
head(predicted_value,1)

test$y_pred_num <- ifelse(predicted_value> 0.5, 1, 0)
str(test)
test$y_predicted<-factor(test$y_pred_num, levels=c(0, 1))
str(test)
test$y1<-factor(test$y_predicted,labels=c("no","yes"),levels=c(0,1))
sensitivity(as.integer(test$y), as.integer(test$y1), threshold = 0.5)
specificity(as.integer(test$y), as.integer(test$y1), threshold = 0.5)

#Creating a confusion matrix
Confusion_Matrix<-table(test$y,test$y_predicted)
Confusion_Matrix

#calclating Classification rate
sum(diag(Confusion_Matrix))/sum(Confusion_Matrix)
#calculating Missclassification rate
1-sum(diag(Confusion_Matrix))/sum(Confusion_Matrix)
table(test$y)
7333/8238
y_observed <- test$y
mean(test$y1==test$y)

#part 6
install.packages("rpart")
library(rpart)
install.packages("InformationValue")
install.packages("ROCR")
library(ROCR)
library(InformationValue)
cutoff<-optimalCutoff(test$y,test$y_pred_num)[1]

optcf <- optimalCutoff(test$y,y_pred_num)[1]
ypred21 <- Ifelse(y_pred_num > optcf,1,0)
ypred22 <- factor(ypred21,levels=c(0,1))
yobs <- test$y
mean(ypred == yobs)

qmodel<-rpart(y~.,method = "class", train1)
qmodel

printcp(q6model)

plotcp(q6model)

plot(qmodel, uniform = TRUE, main="Classification Tree")
text(qmodel,use.n = TRUE, all = TRUE, cex=0.8)

prune.t<-prune(qmodel,cp=qmodel$cptable[which.min(qmodel$cptable[,"xerror"]),"CP"])

plot(prune_t,uniform = TRUE, main="Prune Tree for kyphosis")
text(prune_t,use.n = TRUE, all=TRUE,cex=0.9)





