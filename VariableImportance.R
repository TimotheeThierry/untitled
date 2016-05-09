setwd("~/Documents/Impala/Scrapping Onisep")
tab=read.csv("table.csv")
metier = tab["Métier"]

n = nrow(tab)
p = ncol(tab)

##############
# K-means
##############

#Unlike hierarchical clustering, K-means clustering requires that the number of clusters 
#to extract be specified in advance. A plot of the total within-groups sums of squares 
#against the number of clusters in a K-means solution can be helpful. A bend in the 
#graph can suggest the appropriate number of clusters. The graph can be produced by 
#the following function.

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#1 standardize data : since the variables vary in range, they are standardized prior 
#to clustering
df <- scale(tab[2:26]) 

#2 determine number of clusters using NbClust : the number of clusters is determined 
#using the wwsplot() and NbClust()functions
wssplot(df,150)

#3 K-Means analysis
km <- kmeans(df,5,nstart=25)
km <- kmeans(df,40,nstart=25)
tab2 = cbind(metier, km$cluster)
c_metier = tab2[order(km$cluster),]
write.csv(c_metier, "/home/timothee/Documents/Impala/cluster_metier2")



###################
# Arbre de décision
###################
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

cluster = km$cluster
df=as.data.frame(cbind(tab[2:26], cluster))
metier_prédit <- rpart(cluster ~ . , data=df, method = "class")

rpart.plot(metier_prédit, fallen.leaves = F, varlen = 30, branch = 1)
str(metier_predit)
tab2 =cbind(metier, cluster, metier_prédit$where)

###################
# Random Forest
###################
library(randomForest)

set.seed(123)
samp <- sample(n, 0.6 * n)
train <- df[samp, ]
test <- df[-samp, ]

model <- randomForest(cluster ~ . , data = train, type )
model$importance

plot( importance(model), lty=2, pch=16)
impvar = rownames(model$importance)[order(model$importance, decreasing = T)]

#We can use ntree and mtry to specify the total number of trees to build (default = 500), 
#and the number of predictors to randomly sample at each split respectively. Let’s take a 
#look at the model.
