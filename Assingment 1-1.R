#Stat 577: Home Work
#Feb 1, 2019  
#Missing Values
life.expectancy = read.csv("UNLifeExpectancy.csv",header=TRUE)
unlist(lapply(life.expectancy,function(x)sum(is.na(x))))
life.expectancy$ILLITERATE[which(is.na(life.expectancy$ILLITERATE))] = median(life.expectancy$ILLITERATE,na.rm=TRUE)
life.expectancy$POP[which(is.na(life.expectancy$POP))] = median(life.expectancy$POP,na.rm=TRUE)
life.expectancy$FERTILITY[which(is.na(life.expectancy$FERTILITY))] = median(life.expectancy$FERTILITY,na.rm=TRUE)
life.expectancy$PRIVATEHEALTH[which(is.na(life.expectancy$PRIVATEHEALTH))] = median(life.expectancy$PRIVATEHEALTH,na.rm=TRUE)
life.expectancy$PUBLICEDUCATION[which(is.na(life.expectancy$PUBLICEDUCATION))] = median(life.expectancy$PUBLICEDUCATION,na.rm=TRUE)
life.expectancy$HEALTHEXPEND [which(is.na(life.expectancy$HEALTHEXPEND ))] = median(life.expectancy$HEALTHEXPEND ,na.rm=TRUE)
life.expectancy$BIRTHATTEND[which(is.na(life.expectancy$BIRTHATTEND))] = median(life.expectancy$BIRTHATTEND,na.rm=TRUE)
life.expectancy$PHYSICIAN[which(is.na(life.expectancy$PHYSICIAN))] = median(life.expectancy$PHYSICIAN,na.rm=TRUE)
life.expectancy$GDP[which(is.na(life.expectancy$GDP))] = median(life.expectancy$GDP,na.rm=TRUE)
unlist(lapply(life.expectancy,function(x)sum(is.na(x))))
life.expectancy.refined = life.expectancy[,-c(1,2,12,13,15)]
#Problem 1: Linear Model, Interpretations, Diagonistic
#(a) Scatter Plot Matrix

head(life.expectancy.refined)
plot(life.expectancy.refined,pch=16,cex=.5)

# B and C) Fit Linear Model

fit = lm(LIFEEXP~.,data=life.expectancy.refined)
summary(fit) # obtain summary of the fitted model
residuals(fit)
mse = mean(residuals(fit)^2)
mse
rmse = sqrt(mse)
rmse

# D, E) residuals vs fitted values, VIF 
library(car)
par(mfrow=c(2,2)) # define a matrix of 2 by 2 of plots
plot(fit)
par(mfrow=c(2,2)) # define a matrix of 2 by 2 of plots
plot(fit)
# Variance Inflation Factor
vif(fit)
sqrt(vif(fit)) > 3
par(mfrow=c(1,1))
# F)Index Plot of The residuals
residuals = fit$residuals
plot(residuals,type='l',xlab="Observation",ylab = "Residual")
title("Are the residuals independent?")
ncvTest(fit)
# Durbin-Watson test
durbinWatsonTest(fit)

library(MASS)
library(leaps)   # package for best sub-set selection
library(glmnet)
 
#### Best Subset Selection

fitbsub = regsubsets(x=life.expectancy.refined[,2:10],y=life.expectancy.refined[,1],nbest = 1, nvmax = 9)
summary(fitbsub)  # summary of the results
summary(fitbsub,matrix.logical=TRUE) # summary of the results, more intuitive
par(mfrow=c(1,2))
plot(fitbsub) # summary of the results, where blocks show presence or absence of variable
plot (fitbsub, scale="Cp") # this uses Cp instead of BIC
bs_fit = lm(LIFEEXP~ FERTILITY+ HEALTHEXPEND+ BIRTHATTEND,data = life.expectancy.refined) 
summary(bs_fit)
# Forward Stepwise Model
fit0 = lm(LIFEEXP~1,data=life.expectancy.refined) # fit the model with only the intercept
fitf = stepAIC(fit0,scope=LIFEEXP~ILLITERATE+POP+FERTILITY+PRIVATEHEALTH+PUBLICEDUCATION+HEALTHEXPEND+BIRTHATTEND+PHYSICIAN+GDP,
               direction="forward",data=life.expectancy.refined,k=log(nrow(life.expectancy.refined)), trace=TRUE)
summary(fitf)
# Backward Stepwise
  
fit = lm(LIFEEXP~.,data=life.expectancy.refined)# fit model with all variables
fitb = stepAIC(fit,direction="backward",data=life.expectancy.refined,k=log(nrow(ozone)))
summary(fitb)
#Problem3
# OLS Regression with all Predictors
# ( 70/30)
set.seed(12345)
train_index = sample(1:nrow(life.expectancy.refined),.7*nrow(life.expectancy.refined)) # create a random training index
test_data = life.expectancy.refined[-train_index,] # create the desing matric of the test data
fit = lm(LIFEEXP~.,data = life.expectancy.refined[train_index,]) # fit the model on the training data
summary(fit) 
train_pred = fit$fitted.values# predicted values for training data
test_pred = predict(fit,newdata = test_data) # predicted values for test data

# Compute MSE
train_mse = mean((life.expectancy.refined[train_index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[-train_index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 

# ( 50/50)
set.seed(12345)
train_index = sample(1:nrow(life.expectancy.refined),.5*nrow(life.expectancy.refined)) # create a random training index
test_data = life.expectancy.refined[-train_index,] # create the desing matric of the test data
fit = lm(LIFEEXP~.,data = life.expectancy.refined[train_index,]) # fit the model on the training data
summary(fit) 
train_pred = fit$fitted.values# predicted values for training data
test_pred = predict(fit,newdata = test_data) # predicted values for test data

# Compute MSE
train_mse = mean((life.expectancy.refined[train_index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[-train_index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 

#Best Fit Model 
#(70/30)
set.seed(12345)
train_index = sample(1:nrow(life.expectancy.refined),.7*nrow(life.expectancy.refined)) # create a random training index
test_data = life.expectancy.refined[-train_index,] # create the desing matric of the test data
bs_fit = lm(LIFEEXP~ FERTILITY+ HEALTHEXPEND+ BIRTHATTEND,data = life.expectancy.refined[train_index,]) # fit the model on the training data
summary(bs_fit) 
train_pred = bs_fit$fitted.values# predicted values for training data
test_pred = predict(bs_fit,newdata = test_data) # predicted values for test data

# Compute MSE
train_mse = mean((life.expectancy.refined[train_index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[-train_index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 

#(50/50)
set.seed(12345)
train_index = sample(1:nrow(life.expectancy.refined),.5*nrow(life.expectancy.refined)) # create a random training index
test_data = life.expectancy.refined[-train_index,] # create the desing matric of the test data
bs_fit = lm(LIFEEXP~ FERTILITY+ HEALTHEXPEND+ BIRTHATTEND,data = life.expectancy.refined[train_index,]) # fit the model on the training data
summary(bs_fit) 
train_pred = bs_fit$fitted.values# predicted values for training data
test_pred = predict(bs_fit,newdata = test_data) # predicted values for test data

# Compute MSE
train_mse = mean((life.expectancy.refined[train_index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[-train_index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 

#Forwardstepwise
#(70/30)

set.seed(12345)
n = nrow(life.expectancy.refined) # the sample size
training.index = sample(1:n,0.7*n,replace = FALSE) # training set row index
test.index = setdiff(1:n,training.index) # test set row index
training.data = life.expectancy.refined[training.index,] # training data
test.data = life.expectancy.refined[test.index,] # test data
fitf = stepAIC(fit0,scope=LIFEEXP~ILLITERATE+POP+FERTILITY+PRIVATEHEALTH+PUBLICEDUCATION+HEALTHEXPEND+BIRTHATTEND+PHYSICIAN+GDP,
               direction="forward",data = life.expectancy.refined[training.index,],k=log(nrow(life.expectancy.refined[training.index,])), trace=TRUE)
summary(fitf) 
train_pred=predict(fitf)
#train_pred = fitf$fitted.values# predicted values for training data
test_pred = predict(fitf,newdata = test.data) # predicted values for test data

# Compute MSE
train_mse= mean(residuals(fitf)^2)
#train_mse = mean((life.expectancy.refined[training.index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[test.index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 
#(50/50)

set.seed(12345)
n = nrow(life.expectancy.refined) # the sample size
training.index = sample(1:n,0.5*n,replace = FALSE) # training set row index
test.index = setdiff(1:n,training.index) # test set row index
training.data = life.expectancy.refined[training.index,] # training data
test.data = life.expectancy.refined[test.index,] # test data
fitf = stepAIC(fit0,scope=LIFEEXP~ILLITERATE+POP+FERTILITY+PRIVATEHEALTH+PUBLICEDUCATION+HEALTHEXPEND+BIRTHATTEND+PHYSICIAN+GDP,
               direction="forward",data = life.expectancy.refined[training.index,],k=log(nrow(life.expectancy.refined[training.index,])), trace=TRUE)
summary(fitf) 
train_pred=predict(fitf)
#train_pred = fitf$fitted.values# predicted values for training data
test_pred = predict(fitf,newdata = test.data) # predicted values for test data

# Compute MSE
train_mse= mean(residuals(fitf)^2)
#train_mse = mean((life.expectancy.refined[training.index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[test.index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 

#Backward Stepwise
#(70/30)
set.seed(12345)
n = nrow(life.expectancy.refined) # the sample size
training.index = sample(1:n,0.7*n,replace = FALSE) # training set row index
test.index = setdiff(1:n,training.index) # test set row index
training.data = life.expectancy.refined[training.index,] # training data
test.data = life.expectancy.refined[test.index,] # test data
fit = lm(LIFEEXP~.,data = life.expectancy.refined[training.index,])# fit model with all variables
fitb = stepAIC(fit,direction="backward",data = life.expectancy.refined[training.index,],k=(nrow(life.expectancy.refined[training.index,])))
summary(fitb)
#train_pred=predict(fitb)
train_pred = fitb$fitted.values# predicted values for training data
test_pred = predict(fitb,newdata = test.data) # predicted values for test data

# Compute MSE
#train_mse= mean(residuals(fitb)^2)
train_mse = mean((life.expectancy.refined[training.index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[test.index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 
#(50/50)

set.seed(12345)
n = nrow(life.expectancy.refined) # the sample size
training.index = sample(1:n,0.5*n,replace = FALSE) # training set row index
test.index = setdiff(1:n,training.index) # test set row index
training.data = life.expectancy.refined[training.index,] # training data
test.data = life.expectancy.refined[test.index,] # test data
fit = lm(LIFEEXP~.,data = life.expectancy.refined[training.index,])# fit model with all variables
fitb = stepAIC(fit,direction="backward",data = life.expectancy.refined[training.index,],k=(nrow(life.expectancy.refined[training.index,])))
summary(fitb)
#train_pred=predict(fitb)
train_pred = fitf$fitted.values# predicted values for training data
test_pred = predict(fitb,newdata = test.data) # predicted values for test data

# Compute MSE
train_mse= mean(residuals(fitb)^2)
#train_mse = mean((life.expectancy.refined[training.index,1]-train_pred)^2)
test_mse = mean((life.expectancy.refined[test.index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 


