## Assingment 5
library(MASS)
library(leaps)
install.packages("car")
library(car)
## ----wholesale-----------------------------------------------------
wholesale = read.csv("wholesale.csv",header=TRUE)
summary(wholesale)
head(wholesale,3)
unlist(lapply(wholesale,function(x)sum(is.na(x))))
wholesale.refined = wholesale[,-c(1,2)]
unlist(lapply(wholesale.refined,function(x)sum(is.na(x))))
head(wholesale.refined)
## ----wholesale data cleaning---------------------------------------
ORIG.wholesale = wholesale.refined #make a copy of the original
wholesale.refined[,1:6] <- log10(wholesale.refined[,1:6]+1)  # add one so we aren't taking log of 0
wholesale.refined <- as.data.frame(scale(wholesale.refined))
hist(FLYER$TotalMiles)
##Problem 1: K-means
#a)
## ----wholesale choosing K------------------------------------------
WCSS <- c()
possible.k <- 1:15
set.seed(577)  #important for reproducibility since kmeans uses random initial centers
for (i in possible.k) { 
  best.SS <- kmeans(wholesale.refined,center=i,iter.max=75,nstart=25)$tot.withinss #25 restarts with 50 iterations max (overkill)
  WCSS[i] <- best.SS }
plot(WCSS~possible.k,pch=20,cex=2)
#b)

## ----wholesale - descriptive analytics with 3 clusters-------------
set.seed(577); CLUSTER <- kmeans(wholesale.refined,center=3,iter.max=75,nstart=50)
round( CLUSTER$centers,digits=1 )
round( CLUSTER$size,digits=1 )
##c)
## ----wholesale - descriptive analytics with 4 clusters-------------
set.seed(577); CLUSTER <- kmeans(wholesale.refined,center=4,iter.max=75,nstart=50)
round( CLUSTER$centers,digits=1 )
round( CLUSTER$size,digits=1 )

## ----wholesale - descriptive analytics with 5 clusters-------------
set.seed(577); CLUSTER <- kmeans(wholesale.refined,center=5,iter.max=75,nstart=50)
round( CLUSTER$centers,digits=1 )
round( CLUSTER$size,digits=1 )

## Problem 2: Hirerchical Clustering
#####################################
###Hierarchical Clustering ##########
#####################################
##a)
## ----wholesale,echo=FALSE---------------------------------------------------
HC <- hclust( dist(wholesale.refined[,1:6]),method="complete")  #Only 1st 7 columns has characteristics of fliers
plot(HC)
round(ORIG.wholesale[2997,],digits=1)
wholesale.refined [76 , ]
wholesale.refined [339 , ]
summary(wholesale.refined)
wholesale [76 , ]

wholesale [339 , ]
summary(wholesale)

##b)
HC <- hclust( dist(wholesale.refined[,1:6]),method="ward.D2")  #Only 1st 7 columns has characteristics of fliers
plot(HC)
round(ORIG.wholesale[2997,],digits=1)
(abline(h=11))

## ----wholesale Cut,echo=FALSE-----------------------------------------------
##c)
wholesale.refined$clusterIDhc <- cutree(HC,k=4) #4 because 1 cluster is an outlier by itself
table(wholesale.refined$clusterIDhc)
round( aggregate(.~clusterIDhc,data= wholesale.refined,FUN=mean),digits=1)
ORIG.wholesale$clusterIDhc <- cutree(HC,k=4) #5 because 1 cluster is an outlier by itself
round( aggregate(.~clusterIDhc,data= ORIG.wholesale,FUN=median),digits=1)
##d)

wholesale.refined$clusterIDhc5 <- cutree(HC,k=5) #5 because 1 cluster is an outlier by itself
table(wholesale.refined$clusterIDhc5)
round( aggregate(.~clusterIDhc5,data= wholesale.refined,FUN=mean),digits=1)




