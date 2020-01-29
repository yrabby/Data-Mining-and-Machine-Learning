###########################
# STAT 577- Home Work 2: Yasin Wahid Rabby
# Energy

#libraries required
library(glmnet)  # for LASSO, Ridge, and Elastic Net
#library(ncvreg)  # for SCAD and MCP 
#install.packages("ncvreg")
#### Questuin (a)
# Ridge Regression
energy = read.csv("energy.csv")
Y = as.numeric(energy[,1]); Y = Y - mean(Y)  # center the response
X = as.matrix(energy[,-1]); X = scale(X,center=T,scale=F)  # center the predictors
# with given lambda
lam = 1
fitr = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=0)
betar = as.matrix(fitr$beta)
betar
# Ridge solution paths  
fitr = glmnet(x=X,y=Y,family="gaussian",alpha=0)
plot(fitr,col=1:8) # plots paths with respect to L1 Norm (default)
legend(0,15,legend=names(energy)[2:9],col=1:8,lty=rep(1,8),cex=.8)
plot(fitr,xvar = "lambda",col=1:8) #ridge paths  -- with respect to log lambda
legend("topright",legend=names(energy)[2:9],col=1:8,lty=rep(1,8),cex=.8)

# Lasso Regression
energy = read.csv("energy.csv")
Y = as.numeric(energy[,1]); Y = Y - mean(Y)  # center the response
X = as.matrix(energy[,-1]); X = scale(X,center=T,scale=F)  # center the predictors
lam = 1
fitl = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1)
betal = as.matrix(fitl$beta)
betal

#lasso paths  -- with respect to L1 norm

fitl = glmnet(x=X,y=Y,family="gaussian",alpha=1)
plot(fitl,col=1:8)
legend(0,19,legend=names(energy)[2:9],col=1:8,lty=rep(1,8),cex=.8)

plot(fitl,xvar = "lambda",col=1:8) #lasso paths  -- with respect to log lambda
legend("bottom",legend=names(energy)[2:9],col=1:8,lty=rep(1,8),cex=.5)

# elastic net regression
energy = read.csv("energy.csv")
Y = as.numeric(energy[,1]); Y = Y - mean(Y)  # center the response
X = as.matrix(energy[,-1]); X = scale(X,center=T,scale=F)  # center the predictors
fitenet = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # enet
betanet = as.matrix(fitenet$beta)
betanet

#Elasticnet paths  -- with respect to L1 norm

fitnet = glmnet(x=X,y=Y,family="gaussian",alpha=.5)
plot(fitnet,col=1:8)
legend(0,19,legend=names(energy)[2:9],col=1:8,lty=rep(1,8),cex=.8)

plot(fitnet,xvar = "lambda",col=1:8) #elasticnet  -- with respect to log lambda
legend("bottom",legend=names(energy)[2:9],col=1:8,lty=rep(1,8),cex=.5)

library(glmnet)  # for LASSO, Ridge, and Elastic Net
library(ncvreg)  # for SCAD and MCP 

# Load in ozone data and set predictor matrix and response vector
energy = read.csv("energy.csv")
Y = as.numeric(energy[,1]); Y = Y - mean(Y)  # center the response
X = as.matrix(energy[,-1]); X = scale(X,center=T,scale=F) # center the predictors
#n = nrow(XX)
#train_index = sample(1:n,230,replace = FALSE) # randomly select the indices of the training set
#X = XX[train_index,]  # trainining X
#Y = YY[train_index]  # training Y
#X_test = XX[-c(train_index),] # Test X
#Y_test = YY[-c(train_index)] # Test Y

####################################################################
#least squares, lasso, adaptive lasso, SCAD, ridge, elastic net, MC+
####################################################################

#X = XX  # Independent Variables
#Y = YY  #  Dependent Variable
lam = 1 # specify lambda

#betals = solve(t(X)%*%X)%*%t(X)%*%Y # ols
betar = solve(t(X)%*%X + diag(rep(lam/2*nrow(ozone),7)))%*%t(X)%*%Y # ridge
fitl = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1) # lasso
#fital = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1,penalty.factor=1/abs(betals)) # adaptive lasso
fitenet = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # enet
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",lambda=lam) # scad
#fitmcp = ncvreg(X,Y,family="gaussian",penalty="MCP",lambda=lam) # mcp
coefs= cbind(betar,as.matrix(fitl$beta),as.matrix(fitenet$beta))
colnames(coefs) = c("Ridge","Lasso", "ENET")
coefs


##############################################################################
# compare ridge, lasso regualrization paths
##############################################################################


par(mfrow=c(2,3)) # partition the plot screen into a 2 x 3 matrix

fitr = glmnet(x=X,y=Y,family="gaussian",alpha=0) # ridge fit
plot(fitr,col=1:8,main="Ridge",xvar = "lambda") # ridge solution paths
legend("topright",legend=names(energy)[2:9],col=1:8,lty=rep(1,8),cex=.3)


fitl = glmnet(x=X,y=Y,family="gaussian",alpha=1) # lasso fit
plot(fitl,col=1:8,main="Lasso",xvar = "lambda") # lasso solution paths
legend("bottomright",legend=names(ozone)[2:9],col=1:8,lty=rep(1,8),cex=.2)

fitenet = glmnet(x=X,y=Y,family="gaussian",alpha=.5) # enet fit
plot(fitenet,col=1:8,main="ENET alpha=.5",xvar = "lambda") # enet solution paths
legend("bottomright",legend=names(ozone)[2:9],col=1:8,lty=rep(1,8),cex=.2)

### Question: ( b)
#70% training and 30% test
# Load in energy data and set predictor matrix and response vector
energy = read.csv("energy.csv")
YY = as.numeric(energy[,1]); YY = YY - mean(YY)  # center the response
XX = as.matrix(energy[,-1]); XX = scale(XX,center=T,scale=F) # center the predictors
n = nrow(XX)
train_index = sample(1:n,538,replace = FALSE) # randomly select the indices of the training set
X = XX[train_index,]  # trainining X
Y = YY[train_index]  # training Y
X_test = XX[-c(train_index),] # Test X
Y_test = YY[-c(train_index)] # Test Y
X = XX  # trainining X
Y = YY  # training Y
lam = 1 # specify lambda

#betals = solve(t(X)%*%X)%*%t(X)%*%Y # ols
betar = solve(t(X)%*%X + diag(rep(lam/2*nrow(energy),7)))%*%t(X)%*%Y # ridge
fitl = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1) # lasso
#fital = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1,penalty.factor=1/abs(betals)) # adaptive lasso
fitenet = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # enet
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",lambda=lam) # scad
#fitmcp = ncvreg(X,Y,family="gaussian",penalty="MCP",lambda=lam) # mcp
coefs= cbind(betar,as.matrix(fitl$beta),as.matrix(fitenet$beta))
colnames(coefs) = c("Ridge","Lasso","ENET")
coefs


########################################################################
# Prediction Accuracy Comparisons
########################################################################
#fitls = lm(Y~X-1)
#predictedls = X_test%*%fitls$coef 
cvfitl = cv.glmnet(x=X,y=Y,family="gaussian",alpha=1)
l.min = cvfitl$lambda.min
predictedl = predict(cvfitl, newx = X_test, s = l.min ) # predicted values for lasso
cvfitr = cv.glmnet(x=X,y=Y,family="gaussian",alpha=0)
r.min = cvfitr$lambda.min
predictedr = predict(cvfitr, newx = X_test, s = r.min) # predicted values for ridge
cvfitenet = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.5)
enet.min = cvfitenet$lambda.min
predictedenet = predict(cvfitenet, newx = X_test, s = enet.min) # predicted values for ENET
#cvfitscad = cv.ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
#scad.min = cvfitscad$lambda.min
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
#predictedscad = predict(fitscad, X=X_test, lambda = scad.min,type="link") # predicted values for SCAD

# plot the predicted values (for test observations)
predicted = data.frame(cbind(predictedl,predictedr,predictedenet))
colnames(predicted) = c("LASSO","Ridge","ENET")
plot(predicted)

# Test MSE for each method
#MSEls = mean((Y_test - predictedls)^2)

MSEl = mean((Y_test - predictedl)^2)
MSEr = mean((Y_test - predictedr)^2)
MSEenet = mean((Y_test - predictedenet)^2)

#MSEscad = mean((Y_test - predictedscad)^2)
MSE = cbind(MSEl,MSEr,MSEenet)
colnames(MSE) = c("LASSO","Ridge","ENET")
MSE


# You can also obtain the cvfit plot for SCAD and MCP just as you did for Lasso
# for instance for the above scad cv fit

plot(cvfitscad)

#### 20 Independent Iteration######

# 70% Training and 30% Test
XX = as.matrix(energy[,-1]); XX = scale(XX,center=T,scale=F)
YY = as.numeric(energy[,1]); YY = YY - mean(YY) 

MSE_Ts = matrix(0,20,3) 
for(i in 1:20)
  
{
  train_index = sample(1:768,538,replace=FALSE) # randomly select the indices of the training set
  X = XX[train_index,]  # trainining X
  Y = YY[train_index]  # training Y
  X_test = XX[-c(train_index),] # Test X
  Y_test = YY[-c(train_index)] # Test Y 
  cvfitl = cv.glmnet(x=X,y=Y,family="gaussian",alpha=1)
  l.min = cvfitl$lambda.min
  predictedl = predict(cvfitl, newx = X_test, s = l.min ) # predicted values for lasso
  cvfitr = cv.glmnet(x=X,y=Y,family="gaussian",alpha=0)
  r.min = cvfitr$lambda.min
  predictedr = predict(cvfitr, newx = X_test, s = r.min) # predicted values for ridge
  cvfitenet = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.5)
  enet.min = cvfitenet$lambda.min
  predictedenet = predict(cvfitenet, newx = X_test, s = enet.min) # predicted values for ENET
  #cvfitscad = cv.ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
  #scad.min = cvfitscad$lambda.min
  #fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
  #predictedscad = predict(fitscad, X=X_test, lambda = scad.min,type="link") # predicted values for SCAD
  
  
  
  # plot the predicted values (for test observations)
  predicted = data.frame(cbind(predictedl,predictedr,predictedenet))
  colnames(predicted) = c("LASSO","Ridge","ENET")
  plot(predicted)
  
  # Test MSE for each method
  #MSEls = mean((Y_test - predictedls)^2)
  
  MSEl = mean((Y_test - predictedl)^2)
  MSEr = mean((Y_test - predictedr)^2)
  MSEenet = mean((Y_test - predictedenet)^2)
  
  #MSEscad = mean((Y_test - predictedscad)^2)
  MSE_Ts[i,] = c( MSEl, MSEr, MSEenet)
}
MSEs = apply(MSE_Ts,2,mean) # compute the mean MSE from 20 replications
names(MSEs) = c("LASSO","Ridge","ENET")
MSEs


####50% training and 50% test
# Load in energy data and set predictor matrix and response vector
energy = read.csv("energy.csv")
YY = as.numeric(energy[,1]); YY = YY - mean(YY)  # center the response
XX = as.matrix(energy[,-1]); XX = scale(XX,center=T,scale=F) # center the predictors
n = nrow(XX)
train_index = sample(1:n,384,replace = FALSE) # randomly select the indices of the training set
X = XX[train_index,]  # trainining X
Y = YY[train_index]  # training Y
X_test = XX[-c(train_index),] # Test X
Y_test = YY[-c(train_index)] # Test Y

X = XX  # trainining X
Y = YY  # training Y
lam = 1 # specify lambda

#betals = solve(t(X)%*%X)%*%t(X)%*%Y # ols
betar = solve(t(X)%*%X + diag(rep(lam/2*nrow(energy),7)))%*%t(X)%*%Y # ridge
fitl = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1) # lasso
#fital = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=1,penalty.factor=1/abs(betals)) # adaptive lasso
fitenet = glmnet(x=X,y=Y,family="gaussian",lambda=lam,alpha=.5) # enet
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",lambda=lam) # scad
#fitmcp = ncvreg(X,Y,family="gaussian",penalty="MCP",lambda=lam) # mcp
coefs= cbind(betar,as.matrix(fitl$beta),as.matrix(fitenet$beta))
colnames(coefs) = c("Ridge","Lasso","ENET")
coefs

########################################################################
# Prediction Accuracy Comparisons
########################################################################
#fitls = lm(Y~X-1)
#predictedls = X_test%*%fitls$coef 
cvfitl = cv.glmnet(x=X,y=Y,family="gaussian",alpha=1)
l.min = cvfitl$lambda.min
predictedl = predict(cvfitl, newx = X_test, s = l.min ) # predicted values for lasso
cvfitr = cv.glmnet(x=X,y=Y,family="gaussian",alpha=0)
r.min = cvfitr$lambda.min
predictedr = predict(cvfitr, newx = X_test, s = r.min) # predicted values for ridge
cvfitenet = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.5)
enet.min = cvfitenet$lambda.min
predictedenet = predict(cvfitenet, newx = X_test, s = enet.min) # predicted values for ENET
#cvfitscad = cv.ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
#scad.min = cvfitscad$lambda.min
#fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
#predictedscad = predict(fitscad, X=X_test, lambda = scad.min,type="link") # predicted values for SCAD

# plot the predicted values (for test observations)
predicted = data.frame(cbind(predictedl,predictedr,predictedenet))
colnames(predicted) = c("LASSO","Ridge","ENET")
plot(predicted)

# Test MSE for each method
#MSEls = mean((Y_test - predictedls)^2)

MSEl = mean((Y_test - predictedl)^2)
MSEr = mean((Y_test - predictedr)^2)
MSEenet = mean((Y_test - predictedenet)^2)

#MSEscad = mean((Y_test - predictedscad)^2)
MSE = cbind(MSEl,MSEr,MSEenet)
colnames(MSE) = c("LASSO","Ridge","ENET")
MSE

# ###20 Independent Replication##

# 50% Training and 50% Test
XX = as.matrix(energy[,-1]); XX = scale(XX,center=T,scale=F)
YY = as.numeric(energy[,1]); YY = YY - mean(YY) 

MSE_Ts = matrix(0,20,3) 
for(i in 1:20)
  
{
  train_index = sample(1:768,384,replace=FALSE) # randomly select the indices of the training set
  X = XX[train_index,]  # trainining X
  Y = YY[train_index]  # training Y
  X_test = XX[-c(train_index),] # Test X
  Y_test = YY[-c(train_index)] # Test Y 
  cvfitl = cv.glmnet(x=X,y=Y,family="gaussian",alpha=1)
  l.min = cvfitl$lambda.min
  predictedl = predict(cvfitl, newx = X_test, s = l.min ) # predicted values for lasso
  cvfitr = cv.glmnet(x=X,y=Y,family="gaussian",alpha=0)
  r.min = cvfitr$lambda.min
  predictedr = predict(cvfitr, newx = X_test, s = r.min) # predicted values for ridge
  cvfitenet = cv.glmnet(x=X,y=Y,family="gaussian",alpha=.5)
  enet.min = cvfitenet$lambda.min
  predictedenet = predict(cvfitenet, newx = X_test, s = enet.min) # predicted values for ENET
  #cvfitscad = cv.ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
  #scad.min = cvfitscad$lambda.min
  #fitscad = ncvreg(X,Y,family="gaussian",penalty="SCAD",alpha=1)
  #predictedscad = predict(fitscad, X=X_test, lambda = scad.min,type="link") # predicted values for SCAD
  
  
  
  # plot the predicted values (for test observations)
  predicted = data.frame(cbind(predictedl,predictedr,predictedenet))
  colnames(predicted) = c("LASSO","Ridge","ENET")
  plot(predicted)
  
  # Test MSE for each method
  #MSEls = mean((Y_test - predictedls)^2)
  
  MSEl = mean((Y_test - predictedl)^2)
  MSEr = mean((Y_test - predictedr)^2)
  MSEenet = mean((Y_test - predictedenet)^2)
  
  #MSEscad = mean((Y_test - predictedscad)^2)
  MSE_Ts[i,] = c( MSEl, MSEr, MSEenet)
}
MSEs = apply(MSE_Ts,2,mean) # compute the mean MSE from 20 replications
names(MSEs) = c("LASSO","Ridge","ENET")
MSEs

#####Question (c)####
energy = read.csv("energy.csv")

# Set X and Y: center both
Y = as.numeric(energy[,1]); Y = Y - mean(Y)  # center the response
X = as.matrix(energy[,-1]); X = scale(X,center=T,scale=F)  # center the predictors

############################################################
# PCR and PLS using the PLS package
#############################################################
# PCR
library(pls)
set.seed(1)
pcr.fit = pcr(Heating~., data=energy ,scale=TRUE,validation ="CV")
summary(pcr.fit)
ncomp.pcr = pcr.fit$ncomp # Number of components
ncomp.pcr
coefplot(pcr.fit, ncomp = 1:ncomp.pcr, legendpos="bottom" ) # coefficient plots
loadings.pcr = coef(pcr.fit, ncomp = ncomp.pcr,intercept = FALSE)
loadings.pcr
validationplot(pcr.fit,val.type="MSEP") # plot the CV results

plot(pcr.fit, ncomp =2 , asp = 1, line = TRUE,main = "Actual Vs Predicted - PCR") # plots predicted V actual
plot(pcr.fit, plottype = "scores", comps = 1:3,main="Matrix Plot of the PCs") # a matrix plot of the PCs
predicted.pcr = predict(pcr.fit, ncomp = 2, newdata = X) # get predicted values
MSE.pcr = mean((predicted.pcr - Y)^2) # Training MSE 
MSE.pcr

# PLSR
energy = read.csv("energy.csv")

# Set X and Y: center both
Y = as.numeric(energy[,1]); Y = Y - mean(Y)  # center the response
X = as.matrix(energy[,-1]); X = scale(X,center=T,scale=F)  # center the predictors

set.seed(1)
pls.fit=plsr(Heating~., data=energy ,scale=TRUE ,validation ="CV")
summary(pls.fit)
ncomp.pls = pls.fit$ncomp
ncomp.pls
coefplot(pls.fit, ncomp = 1:ncomp.pls, legendpos = "bottom") # coefficient plots
loadings.pls = coef(pls.fit, ncomp = ncomp.pls,intercept = FALSE)
loadings.pls
validationplot(pls.fit,val.type="MSEP") # plot the CV results

plot(pls.fit, ncomp = 2, asp = 1, line = TRUE,main = "Actual V Predicted - PLSR") # plots predicted V actual
plot(pls.fit, plottype = "scores", comps = 1:2,main = "PLS") # a matrix plot of the PCs
predicted.pls = predict(pls.fit, ncomp = 2, newdata = X)
MSE.pls = mean((predicted.pls - Y)^2) # Training MSE 
MSE.pls

# Compare the two methods 
#par(mfrow=c(3,2))
#validationplot(pcr.fit,val.type="MSEP",main="PCR") # plot the CV results
#validationplot(pls.fit,val.type="MSEP",main="PLSR") # plot the CV results
#coefplot(pcr.fit, ncomp = 1:ncomp.pcr,main="PCR")# coefficient plots
#coefplot(pls.fit, ncomp = 1:ncomp.pls,main="PLSR") # coefficient plots
plot(predicted.pcr,predicted.pls,main="Predicted: PCR v PLSR") # predicted values
windows()

# ###Question (d)###
XX = as.matrix(energy[,-1]); XX = scale(XX,center=T,scale=F)
YY = as.numeric(energy[,1]); YY = YY - mean(YY) 

#70% Traing and 30% Test
MSE_Ts = matrix(0,100,2) 
for(i in 1:100)
{
  train_index = sample(1:768,538,replace=FALSE) # select training index randomly
  X = XX[c(train_index),] # training X
  X_test = XX[-c(train_index),] # test X
  Y = YY[c(train_index)] # Training Y
  Y_test = YY[-c(train_index)] # Test Y
  pcr.fit = pcr(Y~X,scale=TRUE,ncomp = 2)  # Fit pcr to the training data
  predicted.pcr = predict(pcr.fit, ncomp = 2, newdata = X_test) # get predicted values to the test data
  pls.fit = plsr(Y~X,scale=TRUE,ncomp = 2)  # Fit plsr to to the training data
  predicted.pls = predict(pls.fit, ncomp = 2, newdata = X_test) # Get predicted values to the test data
  MSE.pcr = mean((predicted.pcr - Y_test)^2) # test mse forpcr
  MSE.plsr = mean((predicted.pls - Y_test)^2) # test mse for plsr
  MSE_Ts[i,] = c( MSE.pcr, MSE.plsr)
}

MSEs = apply(MSE_Ts,2,mean) # compute the mean MSE from 100 replications
names(MSEs) = c("PCR","PLSR")
MSEs


#50% Traing and 50% Test
XX = as.matrix(energy[,-1]); XX = scale(XX,center=T,scale=F)
YY = as.numeric(energy[,1]); YY = YY - mean(YY) 

MSE_Ts = matrix(0,100,2) 
for(i in 1:100)
{
  train_index = sample(1:768,384,replace=FALSE) # select training index randomly
  X = XX[c(train_index),] # training X
  X_test = XX[-c(train_index),] # test X
  Y = YY[c(train_index)] # Training Y
  Y_test = YY[-c(train_index)] # Test Y
  pcr.fit = pcr(Y~X,scale=TRUE,ncomp = 2)  # Fit pcr to the training data
  predicted.pcr = predict(pcr.fit, ncomp = 2, newdata = X_test) # get predicted values to the test data
  pls.fit = plsr(Y~X,scale=TRUE,ncomp = 2)  # Fit plsr to to the training data
  predicted.pls = predict(pls.fit, ncomp = 2, newdata = X_test) # Get predicted values to the test data
  MSE.pcr = mean((predicted.pcr - Y_test)^2) # test mse forpcr
  MSE.plsr = mean((predicted.pls - Y_test)^2) # test mse for plsr
  MSE_Ts[i,] = c( MSE.pcr, MSE.plsr)
}

MSEs = apply(MSE_Ts,2,mean) # compute the mean MSE from 100 replications
names(MSEs) = c("PCR","PLSR")
MSEs