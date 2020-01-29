---
  title: "Landslide Susceptibility"
author: "Yasin Rabby, Seda Kocaman, & Karessa Manning"
date: "`r format(Sys.time(), '%d %B, %Y')`"
output:
  word_document: default
pdf_document: default
html_document: default
---
  
LSlide = read.csv("All_Data_Landslides_Bangladesh_no_slope.csv",sep=",", header=TRUE)
head(LSlide)

predLabels <- c(names(LSlide[2]) <- 'Topographic Wetness',
                names(LSlide[3]) <- 'Standard Precipitation',
                names(LSlide[4]) <- 'Profile Curvature',
                names(LSlide[5]) <- 'Planform Curvature',
                names(LSlide[6]) <- 'Vegetative Cover',
                names(LSlide[7]) <- 'Elevation',
                names(LSlide[8]) <- 'Fault Distance',
                names(LSlide[9]) <- 'Distance to Road',
                names(LSlide[10]) <- 'Aspect',
                names(LSlide[11]) <- 'Land Use',
                names(LSlide[12]) <- 'Geology',
                names(LSlide[13]) <- 'Rainfall',
                names(LSlide[14]) <- 'Distance to River',
                names(LSlide[15]) <- 'Road Density')
predLabels
################## Models Considered for Classifying Data ##################

######################### Logistic Regression #########################

LSlide$Y[LSlide$Y..HD==-1] = 0
set.seed(1)
for(i in 1:20)
{
  train.index = sample(1:1612,1290,replace = FALSE)
  LSlide.train = LSlide[train.index,]
  LSlide.test = LSlide[-train.index,]
  lr.fit = glm(Y~.,data=LSlide.train,family="binomial",maxit=50)
  predicted.probs = predict(lr.fit,type="response",newdata = LSlide.test[,-1])
  preds = rep("No",nrow(LSlide.test))
  preds[predicted.probs > .5] = "Yes"
}
names(lr.fit$coefficients) <- c('Topographic Wetness','Standard Precipitation','Profile Curvature',
                               'Planform Curvature','Vegetative Cover','Elevation','Fault Distance','Distance',
                               'Aspect','Land Use','Geology','Rainfall','Stream Distance','Road Density')
table(preds,LSlide.test$Y)
# correct classification rate
(150 + 157)/322
# misclassifcation rate
(7 + 8)/322
summary(lr.fit)

######################### LASSO Regression #########################

library(glmnet)

Y = as.numeric(LSlide[,1]); Y = Y  
X = as.matrix(LSlide[,-1]); X = X 
set.seed(1)
for(i in 1:20)
{
  train.index = sample(1:1612,1290,replace = FALSE)
  LSlide.train = LSlide[train.index,]
  LSlide.test = LSlide[-train.index,]
  
  lr.fit = glmnet(x=X,y=Y,family="binomial",alpha=1)
  #plot(lr.fit,col=2:16,xvar = "lambda")
  cvfit = cv.glmnet(x=X,y=Y,family="binomial",alpha=1,type.measure = "class")
  #plot(cvfit)
  predictedlr = predict(cvfit, newx = X, s = "lambda.1se",type="class")
  preds = rep("No",nrow(LSlide.test))
  preds[predictedlr > .5] = "Yes"
}
table(preds,LSlide.test$Y)
lr_corcla = mean(predictedlr == LSlide$Y) # correct classification rate
lr_corcla
lr_miscla = mean(predictedlr != LSlide$Y) # misclassification rate
lr_miscla

######################### Linear SVCs Regression #########################

library(e1071)

set.seed(2)
for(i in 1:20)
{
  train.index = sample(1:1612,1290,replace=FALSE)
  xtrain = LSlide[train.index,c(2:16) ]
  ytrain = LSlide[train.index,1]
  xtest = LSlide[-train.index,c(2:16)]
  ytest = LSlide[-train.index,1]
  traindat = data.frame(xtrain , Y = as.factor(ytrain))
  testdat = data.frame (xtest , Y = as.factor(ytest))
  
  tune.out.svc=tune(svm,Y~., data=testdat,kernel="linear",decision.values=TRUE,probability=TRUE,
                    ranges = list(cost=c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1,1.5,2,5,10,100)))
  bestmod.svc = tune.out.svc$best.model
  pred.probs.svc = attributes(predict(bestmod.svc,testdat,decision.values =TRUE,probability=TRUE))$probabilities[,2]
  ypred.svc=predict(bestmod.svc) # prediction using the best model
}

table(predict=ypred.svc ,truth=testdat$Y) # confusion matrix
sum(ypred.svc!=testdat$Y)/nrow(testdat) # misclassification rate


######################### Kernel SVMs Regression #########################

library(MASS)

set.seed(2)
for(i in 1:20)
{
  train.index = sample(1:1612,1290,replace=FALSE)
  xtrain = LSlide[train.index,c(2:16) ]
  ytrain = LSlide[train.index,1]
  xtest = LSlide[-train.index,c(2:16)]
  ytest = LSlide[-train.index,1]
  traindat = data.frame(xtrain , Y = as.factor(ytrain))
  testdat = data.frame (xtest , Y = as.factor(ytest))
  
  tune.out.svm=tune(svm,Y~., data=testdat,kernel="linear",decision.values=TRUE,probability=TRUE,
                    ranges = list(cost=c(0.001,0.005,0.0075,0.01,0.05,0.075,0.1,0.5,0.75,1,1.5,2,5,10,100)))
  bestmod.svm = tune.out.svm$best.model
  ypred.svm=predict(bestmod.svm) # prediction using the best model
}

table(predict=ypred.svm ,truth=testdat$Y) # confusion matrix
sum(ypred.svm!=testdat$Y)/nrow(testdat) # misclassification rate

######################################## Classification Methods ########################################
set.seed(540) # set seed
train.rows = sample(1:nrow(LSlide),0.8*nrow(LSlide)) # training rows
TRAIN = LSlide[train.rows,]
TEST = LSlide[-train.rows,]

######################### Classification Tree #########################

library(mgcv)
install.packages("MASS")
install.packages("tree")
library(MASS)
library(tree)

LSlide_tree = tree(factor(Y)~.,data = TRAIN)
set.seed(577); LSlide_cv_tree = cv.tree(LSlide_tree,FUN=prune.misclass)
plot(LSlide_cv_tree$size,LSlide_cv_tree$dev,type="b",xlab="size",ylab="Mis-Classification")

LSlide_prune = prune.misclass(LSlide_tree,best=2)
plot(LSlide_prune)
text(LSlide_prune,pretty=0)
pred_tree=predict(LSlide_prune,newdata=TEST[,-20],type="class")
c(misclassification_rate_tree = mean(pred_tree!=TEST$Y))

######################### Bagged Classification Tree Model #########################

install.packages("randomForest")
library(randomForest)

set.seed(577)
LSlide_bag = randomForest(factor(Y)~.,data=TRAIN,mtry=ncol(TRAIN)-1,importance=TRUE)
varImpPlot(LSlide_bag, main='Best Landslide Predictors', type=1)#, labels=predLabels)
pred_bag=predict(LSlide_bag,newdata=TEST,type="class")
c(misclassification_rate_bag = mean(pred_bag!=TEST$Y))

######################### Random Forest Model #########################


set.seed(577)
LSlide_rf = randomForest(factor(Y)~.,data=TRAIN,mtry=5,importance=TRUE)
varImpPlot(LSlide_rf, main='Best Landslide Predictors', type=1, labels=predLabels)
pred_rf = predict(LSlide_rf,newdata=TEST[,-20],type="class")
c(misclassification_rate_rf = mean(pred_rf!=TEST$Y))
summary(LSlide_rf)


######################### Gradient Boost Model #########################

install.packages("gbm")
library(gbm)

set.seed(577)
LSlide_gbm=gbm(Y~.,data=TRAIN,distribution="bernoulli",n.trees=5000,interaction.depth=2,cv.folds = 5)
gbm.perf(LSlide_gbm,plot.it = TRUE,oobag.curve = FALSE,overlay = TRUE,method="cv")
LSlide_gbm=gbm(Y~.,data=TRAIN,distribution="bernoulli",n.trees=65,interaction.depth=2,cv.folds = 5)
summary(LSlide_gbm, cBars = 3, main="Best Landslide Predictors")
pred_gbm = round(predict(LSlide_gbm,newdata=TEST[,-20],type="response",n.trees = 20))
c(misclassification_rate_gbm = mean(pred_gbm!=TEST$Y))

##

misclassification_rate_tree = mean(pred_tree!=TEST$Y)
misclassification_rate_rf = mean(pred_rf!=TEST$Y)
misclassification_rate_gbm = mean(pred_gbm!=TEST$Y)


##
library(gplots)
scaled_LSlide = scale(LSlide[,-20])
heatmap.2(cor(scaled_LSlide), Rowv = FALSE, density.info = "none")