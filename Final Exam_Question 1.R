##Final Exam
##Question 1
##a)
##Linear Regression Model
usage = read.csv("Electricity1.csv",header=TRUE)
head(usage)
usage.refined = usage[,-c(14,15,16)]

train.index = sample(1:nrow(usage.refined),5000) # create a random training index
test.data = usage.refined[-train.index,] # create the desing matric of the test data
fit = lm(Usage~.,data = usage.refined[train.index,]) # fit the model on the training data
summary(fit) 
train.pred = fit$fitted.values# predicted values for training data
test.pred = predict(fit,newdata = test.data) # predicted values for test data


# Compute MSE
train.mse = mean((usage.refined[train.index,1]-train.pred)^2)
test.mse = mean((usage.refined[-train.index,1]-test.pred)^2)
c("Training MSE" = train.mse,"Test MSE"= test.mse) 

## Question 2 (b)
usage = read.csv("Electricity1.csv")
usage.refined = usage[,-c(14,15,16)]
YY = as.numeric(usage.refined[,1]); YY = YY - mean(YY)  # center the response
XX = as.matrix(usage.refined[,-1]); XX = scale(XX,center=T,scale=F) # center the predictors
n = nrow(XX)
train_index = sample(1:n,500,replace = FALSE) # randomly select the indices of the training set
X = XX[train_index,]  # trainining X
Y = YY[train_index]  # training Y
X_test = XX[-c(train_index),] # Test X
Y_test = YY[-c(train_index)] # Test Y
X = XX  # trainining X
Y = YY  # training Y
lam = 1 # specify lambda
#betar = solve(t(X)%*%X + diag(rep(lam/2*nrow(usage.refined),7)))%*%t(X)%*%Y # ridge
fitl0 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=0) # lasso
fitl1 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.1) # lasso
fitl2 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.2) # lasso
fitl3 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.3) # lasso
fitl4 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.4) # lasso
fitl5 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # lasso
fitl6 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.6) # lasso
fitl7 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.7) # lasso
fitl8 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.8) # lasso
fitl9 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.9) # lasso
fitl10 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1) # lasso
#fital = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1,penalty.factor=1/abs(betals)) # adaptive lasso
#fitenet = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # enet
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",lambda=lam) # scad
#fitmcp = ncvreg(X,Y,family="gaussian",penalty="MCP",lambda=lam) # mcp
coefs= cbind(as.matrix(fitl0$beta),as.matrix(fitl1$beta),as.matrix(fitl2$beta),as.matrix(fitl3$beta),as.matrix(fitl4$beta),as.matrix(fitl5$beta),as.matrix(fitl6$beta),as.matrix(fitl7$beta),as.matrix(fitl8$beta),as.matrix(fitl9$beta),as.matrix(fitl10$beta))
colnames(coefs) = c( "0",".1",".2",".3",".4",".5",".6",".7",".8",".9", "1" )
coefs





cvfitl0 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=0)
r.min = cvfitl0$lambda.min
predictedl0 = predict(cvfitl0, newx = X_test, s = r.min) # predicted values for 0
cvfitl1 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.1)
l.min = cvfitl1$lambda.min
predictedl1 = predict(cvfitl1, newx = X_test, s = l.min ) # predicted values for .1
cvfitl2 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.2)
l.min = cvfitl2$lambda.min
predictedl2 = predict(cvfitl2, newx = X_test, s = l.min ) # predicted values for .2
cvfitl3 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.3)
l.min = cvfitl3$lambda.min
predictedl3 = predict(cvfitl3, newx = X_test, s = l.min ) # predicted values for .3
cvfitl4 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.4)
l.min = cvfitl1$lambda.min
predictedl4 = predict(cvfitl4, newx = X_test, s = l.min ) # predicted values for .4
cvfitl5 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.5)
l.min = cvfitl5$lambda.min
predictedl5 = predict(cvfitl5, newx = X_test, s = l.min ) # predicted values for .5

cvfitl6 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.6)
l.min = cvfitl6$lambda.min
predictedl6 = predict(cvfitl6, newx = X_test, s = l.min ) # predicted values for .6
cvfitl7 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.7)
l.min = cvfitl1$lambda.min
predictedl7 = predict(cvfitl7, newx = X_test, s = l.min ) # predicted values for .7
cvfitl8 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.8)
l.min = cvfitl8$lambda.min
predictedl8 = predict(cvfitl8, newx = X_test, s = l.min ) # predicted values for .8
cvfitl9 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.9)
l.min = cvfitl9$lambda.min
predictedl9 = predict(cvfitl9, newx = X_test, s = l.min ) # predicted values for .9
cvfitl10 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=1)
l.min = cvfitl10$lambda.min
predictedl10 = predict(cvfitl9, newx = X_test, s = l.min ) # predicted values for 1

#cvfitscad = cv.ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
#scad.min = cvfitscad$lambda.min
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
#predictedscad = predict(fitscad, X=X_test, lambda = scad.min,type="link") # predicted values for SCAD

# plot the predicted values (for test observations)
predicted = data.frame(cbind(predictedl0,predictedl1,predictedl2, predictedl3, predictedl4, predictedl5, predictedl6, predictedl7, predictedl8, predictedl9, predictedl10))
colnames(predicted) = c("0",".1",".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")
plot(predicted)

# Test MSE for each method
#MSEls = mean((Y_test - predictedls)^2)

MSE0 = mean((Y_test - predictedl0)^2)
MSE1 = mean((Y_test - predictedl1)^2)
MSE2 = mean((Y_test - predictedl2)^2)
MSE3 = mean((Y_test - predictedl3)^2)
MSE4 = mean((Y_test - predictedl4)^2)
MSE5 = mean((Y_test - predictedl5)^2)
MSE6 = mean((Y_test - predictedl6)^2)
MSE7 = mean((Y_test - predictedl7)^2)
MSE8 = mean((Y_test - predictedl8)^2)
MSE9 = mean((Y_test - predictedl9)^2)
MSE10 = mean((Y_test - predictedl10)^2)

#MSEscad = mean((Y_test - predictedscad)^2)
MSE = cbind(MSE0,MSE1,MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10  )
colnames(MSE) = c("0",".1",".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")
MSE
# Train MSE for each method
usage = read.csv("Electricity1.csv")
usage.refined = usage[,-c(14,15,16)]
YY = as.numeric(usage.refined[,1]); YY = YY - mean(YY)  # center the response
XX = as.matrix(usage.refined[,-1]); XX = scale(XX,center=T,scale=F) # center the predictors
n = nrow(XX)
train_index = sample(1:n,50000,replace = FALSE) # randomly select the indices of the training set
X = XX[train_index,]  # trainining X
Y = YY[train_index]  # training Y
lam = 1 # specify lambda
#betar = solve(t(X)%*%X + diag(rep(lam/2*nrow(usage.refined),7)))%*%t(X)%*%Y # ridge
fitl0 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=0) # lasso
fitl1 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.1) # lasso
fitl2 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.2) # lasso
fitl3 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.3) # lasso
fitl4 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.4) # lasso
fitl5 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # lasso
fitl6 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.6) # lasso
fitl7 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.7) # lasso
fitl8 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.8) # lasso
fitl9 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.9) # lasso
fitl10 = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1) # lasso
#fital = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1,penalty.factor=1/abs(betals)) # adaptive lasso
#fitenet = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # enet
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",lambda=lam) # scad
#fitmcp = ncvreg(X,Y,family="gaussian",penalty="MCP",lambda=lam) # mcp
coefs= cbind(as.matrix(fitl0$beta),as.matrix(fitl1$beta),as.matrix(fitl2$beta),as.matrix(fitl3$beta),as.matrix(fitl4$beta),as.matrix(fitl5$beta),as.matrix(fitl6$beta),as.matrix(fitl7$beta),as.matrix(fitl8$beta),as.matrix(fitl9$beta),as.matrix(fitl10$beta))
colnames(coefs) = c( "0",".1",".2",".3",".4",".5",".6",".7",".8",".9", "1" )
coefs

cvfitl0 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=0)
r.min = cvfitl0$lambda.min
predictedl0 = predict(cvfitl0, newx = X, s = r.min) # predicted values for 0
cvfitl1 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.1)
l.min = cvfitl1$lambda.min
predictedl1 = predict(cvfitl1, newx = X, s = l.min ) # predicted values for .1
cvfitl2 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.2)
l.min = cvfitl2$lambda.min
predictedl2 = predict(cvfitl2, newx = X, s = l.min ) # predicted values for .2
cvfitl3 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.3)
l.min = cvfitl3$lambda.min
predictedl3 = predict(cvfitl3, newx = X, s = l.min ) # predicted values for .3
cvfitl4 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.4)
l.min = cvfitl1$lambda.min
predictedl4 = predict(cvfitl4, newx = X, s = l.min ) # predicted values for .4
cvfitl5 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.5)
l.min = cvfitl5$lambda.min
predictedl5 = predict(cvfitl5, newx = X, s = l.min ) # predicted values for .5

cvfitl6 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.6)
l.min = cvfitl6$lambda.min
predictedl6 = predict(cvfitl6, newx = X, s = l.min ) # predicted values for .6
cvfitl7 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.7)
l.min = cvfitl1$lambda.min
predictedl7 = predict(cvfitl7, newx = X, s = l.min ) # predicted values for .7
cvfitl8 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.8)
l.min = cvfitl8$lambda.min
predictedl8 = predict(cvfitl8, newx = X, s = l.min ) # predicted values for .8
cvfitl9 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.9)
l.min = cvfitl9$lambda.min
predictedl9 = predict(cvfitl9, newx = X, s = l.min ) # predicted values for .9
cvfitl10 = cv.glmnet(x=X,y=Y,family="gaussian",alpha=1)
l.min = cvfitl10$lambda.min
predictedl10 = predict(cvfitl9, newx = X, s = l.min ) # predicted values for 1

# plot the predicted values (for training observations)
predicted = data.frame(cbind(predictedl0,predictedl1,predictedl2, predictedl3, predictedl4, predictedl5, predictedl6, predictedl7, predictedl8, predictedl9, predictedl10))
colnames(predicted) = c("0",".1",".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")
plot(predicted)
# Train MSE for each method
#MSEls = mean((Y_test - predictedls)^2)
##train.mse = mean((usage.refined[train.index,1]-train.pred)^2)
MSE0 = mean((Y - predictedl0)^2)
MSE1 = mean((Y - predictedl1)^2)
MSE2 = mean((Y - predictedl2)^2)
MSE3 = mean((Y - predictedl3)^2)
MSE4 = mean((Y - predictedl4)^2)
MSE5 = mean((Y - predictedl5)^2)
MSE6 = mean((Y - predictedl6)^2)
MSE7 = mean((Y - predictedl7)^2)
MSE8 = mean((Y - predictedl8)^2)
MSE9 = mean((Y - predictedl9)^2)
MSE10 = mean((Y - predictedl10)^2)

MSE_Train = cbind(MSE0,MSE1,MSE2, MSE3, MSE4, MSE5, MSE6, MSE7, MSE8, MSE9, MSE10  )
colnames(MSE_Train) = c("0",".1",".2", ".3", ".4", ".5", ".6", ".7", ".8", ".9", "1")
MSE_Train

## Question 1 d) regression tree

set.seed(577)
usage=read.csv("Electricity1.csv")
usage.refined = usage[,-c(14,15,16)]

train=sample(1:nrow(usage.refined), 5000)
#train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.usage=tree(Usage~.,usage.refined,subset=train)

summary(tree.usage)
plot(tree.usage)
text(tree.usage,pretty=0)

cv.usage=cv.tree(tree.usage)
plot(cv.usage$size,cv.usage$dev,type='b', xlab="size",ylab="MSE")
cv.usage

prune.usage=prune.tree(tree.usage,best=5)
plot(prune.usage)
text(prune.usage,pretty=0)

pred.vals = predict(tree.usage,newdata=usage.refined[-train,])
usage.test = usage.refined[-train,"Usage"]
pred.vals1 = predict(tree.usage,newdata=usage.refined[train,])
usage.train = usage.refined[train,"Usage"]
#plot(pred.vals,boston.test,xlab="Predicted Median House Value",ylab="Actual Median House Value")
#abline(0,1)

train.MSE= mean((pred.vals1-usage.train)^2)
train.MSE
test.MSE = mean((pred.vals-usage.test)^2)
test.MSE

## Question 1: e) Random Forest Model
# Random Forest Model
library(randomForest)
set.seed(577)
usage.raw=read.csv("Electricity1.csv")
usage = usage.raw[,-c(14,15,16)]

train=sample(1:nrow(usage), 5000)
usage.test = usage[-train,"Usage"]
rf.usage=randomForest(Usage~.,data=usage,subset=train,mtry=1,
                      importance=TRUE)
rf.usage
varImpPlot(rf.usage)
pred.rf1 = predict(rf.usage,newdata=usage[train,])
usage.train = usage[train,"Usage"]
train.MSE = mean((pred.rf1-usage.train)^2)
train.MSE
pred.rf = predict(rf.usage,newdata=usage[-train,])
usage.test = usage[-train,"Usage"]
test.MSE = mean((pred.rf-usage.test)^2)
test.MSE


library(randomForest)
set.seed(577)
usage.raw=read.csv("Electricity1.csv")
usage = usage.raw[,-c(14,15,16)]

train=sample(1:nrow(usage), 50000)
usage.test = usage[-train,"Usage"]
rf.usage=randomForest(Usage~.,data=usage,subset=train,mtry=3,
                      importance=TRUE)
rf.usage
rf.usage
varImpPlot(rf.usage)
pred.rf1 = predict(rf.usage,newdata=usage[train,])
usage.train = usage[train,"Usage"]
train.MSE = mean((pred.rf1-usage.train)^2)
train.MSE
pred.rf = predict(rf.usage,newdata=usage[-train,])
usage.test = usage[-train,"Usage"]
test.MSE = mean((pred.rf-usage.test)^2)
test.MSE


library(randomForest)
set.seed(577)
usage.raw=read.csv("Electricity1.csv")
usage = usage.raw[,-c(14,15,16)]

train=sample(1:nrow(usage), 50000)
usage.test = usage[-train,"Usage"]
rf.usage=randomForest(Usage~.,data=usage,subset=train,mtry=8,
                      importance=TRUE)
rf.usage
pred.rf1 = predict(rf.usage,newdata=usage[train,])
usage.train = usage[train,"Usage"]
train.MSE = mean((pred.rf1-usage.train)^2)
train.MSE
pred.rf = predict(rf.usage,newdata=usage[-train,])
usage.test = usage[-train,"Usage"]
test.MSE = mean((pred.rf-usage.test)^2)
test.MSE

library(randomForest)
set.seed(577)
usage.raw=read.csv("Electricity1.csv")
usage = usage.raw[,-c(14,15,16)]

train=sample(1:nrow(usage), 50000)
usage.test = usage[-train,"Usage"]
rf.usage=randomForest(Usage~.,data=usage,subset=train,mtry=12,
                      importance=TRUE)
rf.usage
pred.rf1 = predict(rf.usage,newdata=usage[train,])
usage.train = usage[train,"Usage"]
train.MSE = mean((pred.rf1-usage.train)^2)
train.MSE
pred.rf = predict(rf.usage,newdata=usage[-train,])
usage.test = usage[-train,"Usage"]
test.MSE = mean((pred.rf-usage.test)^2)
test.MSE


##Question 1 f) Gradieant Boosted Tree Model
# Gradient Boosted Tree Model
set.seed(577)
usage.raw=read.csv("Electricity1.csv")
usage = usage.raw[,-c(14,15,16)]

train=sample(1:nrow(usage), 5000)
usage.test = usage[-train,"Usage"]
#train = sample(1:nrow(Boston), nrow(Boston)/2)#partition the data into training and test data
#boston.test = Boston[-train,14]  
boost.usage=gbm(Usage~.,data=usage[train,],distribution="gaussian",
                n.trees=208,interaction.depth=2, cv.folds = 5)
summary(boost.usage)   # Gives the variable importance
plot(boost.usage, i="mean")

# Select number of trees
set.seed(577);gbm.perf(boost.usage,plot.it = TRUE,oobag.curve = FALSE,overlay = TRUE,method="cv")
gbm.perf(boost.usage,
         plot.it = TRUE,
         oobag.curve = FALSE,
         overlay = TRUE,
         method="cv")

par(mfrow=c(1,2))

#plot(boost.heart,i="rm")  # Plot the additive function for variable rm
#plot(boost.heart,i="lstat") # Plot the additive function for variable lstat
pred.boost1 = predict(boost.usage,newdata=usage[train,],n.trees=208)
pred.boost = predict(boost.usage,newdata=usage[-train,],n.trees=208)
usage.test=usage$Usage[-train]
usage.train=usage$Usage[train]
#heart.test = heart[-train,"y"]
train.MSE = mean((pred.boost1-usage.train)^2)
train.MSE
test.MSE = mean((pred.boost-usage.test)^2)
test.MSE

##Question 1 g) neural network model
usage.raw=read.csv("Electricity1.csv")
ELECTRICITY = usage.raw[,-c(14,15,16)]

``{r}

set.seed(577)
library(MASS)


scaleddata=scale(ELECTRICITY)
normalize = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf = as.data.frame(lapply(ELECTRICITY, normalize))

trainNN = maxmindf[1:5000, ]
testNN = maxmindf[5001:54054, ]

#node 1 
library(neuralnet)
NN1 = neuralnet(Usage ~., data=trainNN, hidden=1, linear.output=FALSE, threshold=0.01)
plot(NN1)

test.usage = (testNN$Usage)
train.usage = (trainNN$Usage)

nnpreds1= predict(NN1, newdata=trainNN)
c(TrainMSE.NN1 = mean((train.usage - nnpreds1)^2))


nnpreds11= predict(NN1, newdata=testNN)
c(TestMSE.NN1 = mean((test.usage - nnpreds11)^2))

#node 2
NN2 = neuralnet(Usage ~., data=trainNN, hidden=2, linear.output=FALSE, threshold=0.01)
plot(NN2)

nnpreds1= predict(NN2, newdata=trainNN)
c(TrainMSE.NN2 = mean((train.usage - nnpreds1)^2))

nnpreds2= predict(NN2, newdata=testNN)
c(TestMSE.NN2 = mean((test.usage - nnpreds2)^2))

#node 3
NN3 = neuralnet(Usage ~., data=trainNN, hidden=3, linear.output=FALSE, threshold=0.01)
plot(NN3)

nnpreds1= predict(NN3, newdata=trainNN)
c(TrainMSE.NN3 = mean((train.usage - nnpreds1)^2))

nnpreds2= predict(NN3, newdata=testNN)
c(TestMSE.NN3 = mean((test.usage - nnpreds2)^2))

#node 4
NN4 = neuralnet(Usage ~., data=trainNN, hidden=4, linear.output=FALSE, threshold=0.01)
plot(NN4)

nnpreds1= predict(NN4, newdata=trainNN)
c(TrainMSE.NN4 = mean((train.usage - nnpreds1)^2))

nnpreds2= predict(NN4, newdata=testNN)
c(TestMSE.NN4 = mean((test.usage - nnpreds2)^2))

#node 5
NN5 = neuralnet(Usage ~., data=trainNN, hidden=5, linear.output=FALSE, threshold=0.01)
plot(NN5)

nnpreds1= predict(NN5, newdata=trainNN)
c(TrainMSE.NN5 = mean((train.usage - nnpreds1)^2))

nnpreds2= predict(NN5, newdata=testNN)
c(TestMSE.NN5 = mean((test.usage - nnpreds2)^2))

#node 6
NN6 = neuralnet(Usage ~., data=trainNN, hidden=6, linear.output=FALSE, threshold=0.01)
plot(NN6)

nnpreds1= predict(NN6, newdata=trainNN)
c(TrainMSE.NN6 = mean((train.usage - nnpreds1)^2))

nnpreds2= predict(NN6, newdata=testNN)
c(TestMSE.NN6 = mean((test.usage - nnpreds2)^2))
```