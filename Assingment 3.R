###############
# Assingment 3

# Problem 1
#Heart Data
#Logistic Regression
# Splliting the Data into training and Test data
heartt = read.table("heartt.csv",sep=",", header=TRUE)
heartt$ï..HD[heartt$ï..HD==-1] = 0 # response should be 1 and -1 (change 0 to -1)
set.seed(1)
for(i in 1:20)
{
  train.index = sample(1:270,135,replace = FALSE)
  heartt.train = heartt[train.index,]
  heartt.test = heartt[-train.index,]
  lr.fit = glm(ï..HD ~.,data=heartt.train,family="binomial",maxit=50)
  predicted.probs = predict(lr.fit,type="response",newdata = heartt.test[,-1])
  preds = rep("No",nrow(heartt.test))
  preds[predicted.probs > .5] = "Yes"}
table(preds,heartt.test$ï..HD )
# correct classification rate
(72 + 42)/135
# misclassifcation
(7 + 14)/135

##Regularized logistic regression
library(glmnet)
heartt = read.table("heartt.csv",sep=",", header=TRUE)
heartt$ï..HD[heartt$ï..HD==-1] = 0 # response should be 1 and -1 (change 0 to -1)
Y = as.numeric(heartt[,1]); Y = Y  
X = as.matrix(heartt[,-1]); X = X 
set.seed(1)
for(i in 1:20)
{
  train.index = sample(1:270,135,replace = FALSE)
  heartt.train = heartt[train.index,]
  heartt.test = heartt[-train.index,]

  lr.fit = glmnet(x=X,y=Y,family="binomial",alpha=1)
  plot(lr.fit,col=1:14,xvar = "lambda")
  cvfit = cv.glmnet(x=X,y=Y,family="binomial",alpha=1,type.measure = "class")
  plot(cvfit)
  predictedlr = predict(cvfit, newx = X, s = "lambda.1se",type="class")
  preds = rep("No",nrow(heartt.test))
  preds[predicted.probs > .5] = "Yes"}
  table(preds,heartt.train$ï..HD )
  lr_corcla = mean(predictedlr == heartt.test$ï..HD) # correct classification rate
  lr_miscla = mean(predictedlr != heartt.test$ï..HD) # misclassification rate
  
## Linear SVMs (SVC)
library(e1071)
heartt = read.table("heartt.csv", ",", header=TRUE)
#heartt=heart[,-1]
#heartt$ï..HD [heart$ï..HD ==0] = -1 # response should be 1 and -1 (change 0 to -1)
heartt$ï..HD  = as.factor(heartt$ï..HD ) # the response should be a factor
head(heartt) # see the first few rows of the data
  
  
set.seed(2)
for(i in 1:20)
{
 train.index = sample(1:270,135,replace=FALSE)
 xtrain = heartt[train.index, ]
 ytrain = heartt[train.index,14]
 xtest = heartt[-train.index, ]
 ytest = heartt[-train.index,14]
 traindat = data.frame(xtrain , ï..HD = as.factor(ytrain))
 testdat = data.frame (xtest , ï..HD = as.factor(ytest))
  
  
 tune.out.svc=tune(svc,ï..HD~., data=testdat,kernel="linear",decision.values=TRUE,probability=TRUE,
                    ranges = list(cost=c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1,1.5,2,5,10,100)))
 bestmod.svc = tune.out.svc$best.model
 pred.probs.svc = attributes(predict(bestmod.svc,testdat,decision.values =TRUE,probability=TRUE))$probabilities[,2]
 ypred.svc=predict(bestmod.svc) # prediction using the best model
 }
 table(predict=ypred.svc ,truth=testdat$ï..HD) # confusion matrix
 sum(ypred.svc!=testdat$ï..HD)/nrow(testdat) # misclassification rate
  
 ## Kernel SVMs (SVC)
 
 library(MASS)
 heartt = read.table("heartt.csv", ",", header=TRUE)
 #heartt=heart[,-1]
 #heartt$ï..HD [heart$ï..HD ==0] = -1 # response should be 1 and -1 (change 0 to -1)
 heartt$ï..HD  = as.factor(heartt$ï..HD ) # the response should be a factor
 head(heartt) # see the first few rows of the data
 set.seed(2)
 for(i in 1:20)
 {
   train.index = sample(1:270,135,replace=FALSE)
   xtrain = heartt[train.index,c(2:14) ]
   ytrain = heartt[train.index,1]
   xtest = heartt[-train.index,c(2:14)]
   ytest = heartt[-train.index,1]
   traindat = data.frame(xtrain , ï..HD = as.factor(ytrain))
   testdat = data.frame (xtest , ï..HD = as.factor(ytest))
   
 tune.out.svm=tune(svm,ï..HD~., data=testdat ,kernel="radial",decision.values=TRUE,probability=TRUE,
                   ranges = list(cost=c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1,1.5,2,5,10,100),
                                 gamma = c(0.1,0.5,1,2,3,4)))
 bestmod.svm = tune.out.svm$best.model}
 ypred.svm=predict(bestmod.svm)
 table(predict=ypred.svm ,truth=testdat$ï..HD) # confusion matrix
 sum(ypred.svm!=testdat$ï..HD)/nrow(testdat) # misclassification rate
  
## Question 2

flyer=read.csv("flyer1.csv")

#Logistic Regression
#flyer$Class[flyer$Class==-1] = 0 # response should be 1 and -1 (change 0 to -1)
set.seed(1)
for(i in 1:20)
{
  train.index = sample(1:3999,0.5*3999,replace = FALSE)
  flyer.train = flyer[train.index,]
  flyer.test = flyer[-train.index,]
  lr.fit = glm(Class ~.,data=flyer.train,family="binomial",maxit=50)
  predicted.probs = predict(lr.fit,type="response",newdata = flyer.test[,-8])
  preds = rep("No",nrow(flyer.test))
  preds[predicted.probs > .5] = "Yes"}
table(preds,flyer.test$Class)
# correct classification rate
(1395 + 602)/2000
# misclassifcation
(1+ 2)/2000

##Regularized Logistic Regression
library(glmnet)
flyer = read.table("flyer.csv",sep=",", header=TRUE)
#heartt$ï..HD[heartt$ï..HD==-1] = 0 # response should be 1 and -1 (change 0 to -1)
Y = as.numeric(flyer[,8]); Y = Y  
X = as.matrix(flyer[,-8]); X = X 
set.seed(1)
for(i in 1:20)
{
  train.index = sample(1:3999,0.5*3999,replace = FALSE)
  flyer.train = flyer[train.index,]
  flyer.test = flyer[-train.index,]
  
  lr.fit = glmnet(x=X,y=Y,family="binomial",alpha=1)
  plot(lr.fit,col=1:14,xvar = "lambda")
  cvfit = cv.glmnet(x=X,y=Y,family="binomial",alpha=1,type.measure = "class")
  plot(cvfit)
  predictedlr = predict(cvfit, newx = X, s = "lambda.1se",type="class")
  preds = rep("No",nrow(flyer.test))
  preds[predicted.probs > .5] = "Yes"}
# correct classification rate
(987 + 203)/2000
# misclassifcation
(408+ 402)/2000

##Linear SVM
library(e1071)
flyer = read.table("flyer1.csv", ",", header=TRUE)
#heartt=heart[,-1]
flyer$Class [flyer$Class ==0] = -1 # response should be 1 and -1 (change 0 to -1)
flyer$Class  = as.factor(flyer$Class ) # the response should be a factor
head(flyer) # see the first few rows of the data


set.seed(2)
for(i in 1:20)
{
  train.index = sample(1:3999,0.5*3999,replace=FALSE)
  xtrain = flyer[train.index,c(1:7) ]
  ytrain = flyer[train.index,8]
  xtest = flyer[-train.index,c(1:7)]
  ytest = flyer[-train.index,8]
  traindat = data.frame(xtrain , Class = as.factor(ytrain))
  testdat = data.frame (xtest , Class = as.factor(ytest))
  
  
  tune.out.svc=tune(svm,Class~., data=testdat,kernel="linear",decision.values=TRUE,probability=TRUE,
                    ranges = list(cost=c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1,1.5,2,5,10,100)))
  bestmod.svc = tune.out.svc$best.model
  pred.probs.svc = attributes(predict(bestmod.svc,testdat,decision.values =TRUE,probability=TRUE))$probabilities[,2]
  ypred.svc=predict(bestmod.svc) # prediction using the best model
}
table(predict=ypred.svc ,truth=testdat$Class) # confusion matrix
sum(ypred.svc!=testdat$Class)/nrow(testdat) # misclassification rate

table(preds,flyer.test$Class)

## Kernel SVM
library(MASS)
flyer = read.table("flyer1.csv", ",", header=TRUE)
#heartt=heart[,-1]
flyer$Class [flyer$Class ==0] = -1 # response should be 1 and -1 (change 0 to -1)
flyer$Class  = as.factor(flyer$Class ) # the response should be a factor
head(flyer) # see the first few rows of the data


set.seed(2)
  for(i in 1:20)
{
train.index = sample(1:3999,0.5*3999,replace=FALSE)
xtrain = flyer[train.index,c(1:7) ]
ytrain = flyer[train.index,8]
xtest = flyer[-train.index,c(1:7)]
ytest = flyer[-train.index,8]
traindat = data.frame(xtrain , Class = as.factor(ytrain))
testdat = data.frame (xtest , Class = as.factor(ytest))
  
tune.out.svm=tune(svm,Class~., data=testdat ,kernel="radial",decision.values=TRUE,probability=TRUE,
                    ranges = list(cost=c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1,1.5,2,5,10,100),
                                  gamma = c(0.1,0.5,1,2,3,4)))
bestmod.svm = tune.out.svm$best.model}
ypred.svm=predict(bestmod.svm)
table(predict=ypred.svm ,truth=testdat$Class) # confusion matrix
