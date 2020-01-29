#Generalized Additive Model
library(mgcv) # uses cross-validation
heart = read.csv("nba.csv",header=TRUE)
head(heart,3)
set.seed(577)
head(heart)
train=sample(1:nrow(heart), 811)
CHD.test=heart$y[-train]

#train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.heart=tree(y~.,heart,subset=train)
set.seed(577)
train_index = sample(1:nrow(calif),.7*nrow(calif)) # create a random training index
test_data = calif[-train_index,] # create the desing matric of the test data
fit = gam(y ~ s(GamesPlayed)+s(FG.)+s(BLK)+s(TOV)
          +s(STL)+s(AST)+s(REB)+s(DREB)+s(OREB)+s(MIN)+s(PTS)+s(FGM)+s(FGA)+s(ThreePM)+s(ThreePA)+s(ThreeP.)+s(FTM)+s(FTA)+s(FT.),family="binomial", data = heart, subset = train) # fit the model on the training data
summary(fit) 
pred.vals = predict(fit,data=heart[-train,])
heart.test = heart[-train,"y"]
test.MSE = mean((pred.vals-CHD.test)^2)
test.MSE

#plot(pred.vals,boston.test,xlab="Predicted Median House Value",ylab="Actual Median House Value")
#abline(0,1)
test.MSE = mean((pred.vals-heart.test)^2)
test.MSE

train_pred = fit$fitted.values# predicted values for training data
test_pred = predict(fit,newdata = test_data) # predicted values for test data
# Compute MSE
train_mse = mean((calif[train_index,1]-train_pred)^2)
test_mse = mean((calif[-train_index,1]-test_pred)^2)
c("Training MSE" = train_mse,"Test MSE"= test_mse) 
test.MSE = mean((train_pred-test_pred)^2)
test.MSE

#Plot

gam1=lm(y~ns(MIN,4)+ns(PTS,5)+BLK,data=calif) # fit with lm-  using natural splines
gam.m3=gam(y~ s(MIN)+s(FGM)+s(FGA)+s(ThreePM)+s(ThreePA)+s(ThreeP.)+s(FTM)+s(FTA)+s(FT.)+s(TOV)+s(DREB)+s(PTS)+s(BLK),data=calif) # fit with gam using smoothing spliines
par(mfrow=c(2,3))
plot(gam.m3, se=TRUE,col="blue")
plot.gam(gam1, se=TRUE, col="red") # fit with lm
summary(fit)
predictions = predict(fit,se.fit=TRUE)
plot(calif$y,exp(predictions$fit),cex=0.1,
     xlab="Actual price",ylab="Predicted")
segments(calif$y,exp(predictions$fit-2*predictions$se.fit),
         calif$y,exp(predictions$fit+2*predictions$se.fit),
         col="grey")
abline(a=0,b=1,lty=2)

# Classification Tree
install.packages("MASS")
install.packages("tree")
library(MASS)
library(tree)
heart = read.csv(file="nba.csv")
head(heart)
tree.heart=tree(y~.,heart) # Fit a Classification tree
summary(tree.heart)
plot(tree.heart) # Plot the classification tree
text(tree.heart,pretty=0)
tree.heart


#-----------------------------------------------------------------------
# Look at test error
#-----------------------------------------------------------------------
set.seed(577)
train=sample(1:nrow(heart), 811)
heart.test=heart[-train,]
CHD.test=heart$y[-train]
tree.heart=tree(y~.,heart,subset=train)
summary(tree.heart)
plot(tree.heart) # Plot the classification tree
text(tree.heart,pretty=0)
tree.heart
tree.pred=predict(tree.heart,heart.test,type="vector")
conf.mat = table(tree.pred,CHD.test)
conf.mat
corr.clas = (conf.mat[1,1] + conf.mat[2,2] )/sum(conf.mat)
corr.clas
# Now consider if Pruning improves the misclassication rate
#------------------------------------------------------------
set.seed(577)
train=sample(1:nrow(heart), 811)
heart.test=heart[-train,]
CHD.test=heart$y[-train]
tree.heart=tree(y~.,heart,subset=train)
summary(tree.heart)
cv.heart=cv.tree(tree.heart,FUN=prune.tree)
names(cv.heart)
cv.heart
dev.off()
prune.heart=prune.tree(tree.heart,best=6)
plot(prune.heart)
text(prune.heart,pretty=0)

tree.pred=predict(prune.heart,heart.test,type="vector")
conf.mat = table(tree.pred,CHD.test)
corr.clas = (conf.mat[1,1] + conf.mat[2,2] )/sum(conf.mat)
corr.clas
set.seed(577)
heart=read.csv("nba.csv")
head(heart)
train=sample(1:nrow(heart), 811)
#train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.heart=tree(y~.,heart,subset=train)

summary(tree.heart)
plot(tree.heart)
text(tree.boston,pretty=0)

cv.tree=cv.tree(tree.heart)
plot(cv.heart$size,cv.heart$dev,type='b', xlab="size",ylab="MSE")
cv.heart

prune.heart=prune.tree(tree.heart,best=6)
plot(prune.heart)
text(prune.heart,pretty=0)

pred.vals = predict(tree.heart,newdata=heart[-train,])
heart.test = heart[-train,"y"]
#plot(pred.vals,boston.test,xlab="Predicted Median House Value",ylab="Actual Median House Value")
#abline(0,1)
test.MSE = mean((pred.vals-heart.test)^2)
test.MSE

#Bagged Classification Tree Model

install.packages("randomForest")
library(randomForest)
set.seed(577)
heart=read.csv("nba.csv")
head(heart)
train=sample(1:nrow(heart), 811)
heart.test = heart[-train,"y"]


bag.heart = randomForest(y~.,data=heart,subset=train,mtry=19,
                         ntree=500,importance=TRUE)
bag.heart  
importance(bag.heart)  # variable importance measures
varImpPlot(bag.heart)  # variable importance plot

pred.bag = predict(bag.heart,newdata=heart[-train,])#obtain predicted values
heart.test = heart[-train,"y"]
#plot(pred.bag,heart.test,xlab="Predicted Median House Value",
     #ylab="Actual Median House Value",
     #main = "Bagging Predictions for Test Data") 
#abline(0,1)
test.MSE = mean((heart.test)^2)
test.MSE
# Random Forest Model
set.seed(577)
heart=read.csv("nba.csv")
head(heart)
train=sample(1:nrow(heart), 811)
heart.test = heart[-train,"y"]
rf.heart=randomForest(y~.,data=heart,subset=train,mtry=5,
                      importance=TRUE)
rf.heart
pred.rf = predict(rf.heart,newdata=heart[-train,])
heart.test = heart[-train,"y"]
test.MSE = mean((pred.rf-heart.test)^2)
test.MSE
#plot(pred.rf,boston.test,xlab="Predicted Median House Value",
     #ylab="Actual Median House Value",
     #main = "Random Forest Predictions for Test Data") 
#abline(0,1)

importance(rf.heart)
varImpPlot(rf.heart)
# Gradient Boosted Tree Model
set.seed(577)
heart=read.csv("nba.csv")
head(heart)
train=sample(1:nrow(heart), 811)
heart.test = heart[-train,"y"]
#train = sample(1:nrow(Boston), nrow(Boston)/2)#partition the data into training and test data
#boston.test = Boston[-train,14]  
boost.heart=gbm(y~.,data=heart[train,],distribution="bernoulli",
                n.trees=73,interaction.depth=1)
summary(boost.heart)   # Gives the variable importance

# Select number of trees
gbm.perf(boost.heart,
         plot.it = TRUE,
         oobag.curve = FALSE,
         overlay = TRUE,
         method="OOB")

par(mfrow=c(1,2))
#plot(boost.heart,i="rm")  # Plot the additive function for variable rm
#plot(boost.heart,i="lstat") # Plot the additive function for variable lstat
pred.boost = predict(boost.heart,newdata=heart[-train,],n.trees=73)
CHD.test=heart$y[-train]
#heart.test = heart[-train,"y"]
test.MSE = mean((pred.boost-CHD.test)^2)
test.MSE

