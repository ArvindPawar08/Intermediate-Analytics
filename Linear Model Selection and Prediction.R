#part1
setwd("C:/Users/Arvind/Desktop/Intermediate Analytics/Week 3")

a3 <- read.csv("assignment3.csv")
a3

set.seed(12345)
train <- floor(0.75*nrow(a3))

train_ind <-sample(seq_len(nrow(a3)),size = train)
trainset <- a3[train_ind, ]

testset <- a3[-train_ind, ]
dim(trainset)
dim(testset)
names(a3)
Model <- lm(EP~wind+pressure+humidity+visability+FFMC+DMC+DC+ISI, data = trainset)#build the regression model for EP
summary(Model)


Model<- lm(EP~wind+pressure+humidity+visability+FFMC+DMC+ISI, data = trainset)#removed DC
summary(Model)

Model<- lm(EP~wind+pressure+humidity+visability+FFMC+DMC, data = trainset)#removed ISI
summary(Model)


Model<- lm(EP~wind+pressure+visability+FFMC+DMC, data = trainset)#removed humidity
summary(Model)

Model<- lm(EP~wind+pressure+FFMC+DMC, data = trainset)#removed visability
summary(Model)

Model<- lm(EP~wind+pressure+DMC, data = trainset)#removed FFMC
summary(Model)

Model<- lm(EP~pressure+DMC, data = trainset)#removed wind
summary(Model)

Reduced_Model<- lm(EP~pressure, data = trainset)#removed DMC
summary(Model)

anova(Reduced_Model, Model)

install.packages("leaps")
library(leaps)

predict_model<-predict(Model,newdata = testset)
head(predict_model)#using saturated model

Observed_Values <- testset$EP
head(Observed_Values)

#calculate the the R_Squared Value and RMSE for Model 
SSE<-sum((Observed_Values-predict_model)^2)
SST<-sum((Observed_Values-mean(Observed_Values))^2)
r_squared<-1-SSE/SST
r_squared 

RMSE<-sqrt(mean((Observed_Values-predict_model)^2))
RMSE 

RSS <- sum((predict_model - Observed_Values)^2)
RSS
MAE <- mean(abs(Observed_Values - predict_model))
MAE


#predict reduced model
predict_reduced<- predict(Reduced_Model, newdata=testset)
head(predict_reduced)

#calculate the the R_Squared Value and RMSE for Model 
SSE1<-sum((Observed_Values-predict_reduced)^2)
SST1<-sum((Observed_Values-mean(Observed_Values))^2)
r_squared1<-1-SSE1/SST1
r_squared1 

RMSE1<-sqrt(mean((Observed_Values-predict_reduced)^2))
RMSE1 

RSS1 <- sum((predict_reduced - Observed_Values)^2)
RSS1
MAE1 <- mean(abs(Observed_Values - predict_reduced))
MAE1

#install.packages("ISLR")
#require(caTools)
install.packages("ISLR")

library(ISLR)

fit_model <- lm(EP~., data = a3)
Step_AIC <- step(fit_model, direction = "backward")

#part2
#a
x_train<-model.matrix(EP~.,trainset)[,-1]
y_train<-trainset$EP

x_test<-model.matrix(EP~.,testset)[,-1]
y_test<-testset$EP

head(x_train)
head(y_train)
head(x_test)
head(y_test)
#b
install.packages("glmnet")
library(glmnet)

Lasso_model<-glmnet(x_train,y_train,alpha=1,nlambda = 100,lambda.min.ratio = 0.001)
plot(Lasso_model,xvar = "lambda")
Cross_Validation<-cv.glmnet(x_train,y_train,nlambda = 100,alpha=0.5,lambda.min.ratio=0.001)
plot(Cross_Validation)


best_lambda<-Cross_Validation$lambda.min
predict_y<-predict(Lasso_model, s = best_lambda, newx = x_test)

#c
Observe_y<-y_test
rss <- sum((predict_y - Observe_y) ^ 2)  ## residual sum of squares
tss <- sum((Observe_y - mean(Observe_y)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
rsq

#part 3
install.packages("pls")
library(pls)
set.seed (1000)

pcr_model <- pcr(EP~., data = a3, scale = TRUE, validation = "CV")
summary(pcr_model)

##RMSEP
validationplot(pcr_model,val.type = c("RMSEP"))
##MSEP
validationplot(pcr_model,val.type = c("MSEP"))
##R2
validationplot(pcr_model,val.type = c("R2"))



##5 comm 
pcr_model_train<-pcr(EP~.,data=trainset,scale=TRUE,validation='CV')
pcr_pred <- predict(pcr_model, testset, ncomp = 5)
mean((pcr_pred - testset$EP)^2)
summary(pcr_pred)
lrm <- lm(EP~., data = trainset)
lrm_pred <- predict(lrm, testset)
mean((lrm_pred - testset$EP)^2)







