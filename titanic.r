##############################################################################
# Title:        Titanic.r
# Date:         02/28/2018
# Author:       Siddhartha S Bhatnagar
# Description:  Practise set on Titanic data
##############################################################################

library(rpart)
library(rattle)
# install.packages('RGtk2')
# install.packages('rpart.Plot ')
# install.packages('RColorBrewer ')
rm(list=ls(all=TRUE))

titanic <- read.csv('titanic.csv')

dim(titanic)
View(titanic)

#adding age range for easy categorisation
meanAge <- sum(na.omit(titanic$Age))/length(na.omit(titanic$Age))
titanic$Age[is.na(titanic$Age)] <- meanAge
titanic$Age <- round(titanic$Age)

titanic$AgeCat[titanic$Age>=0 & titanic$Age<=16] <- "0-16"
titanic$AgeCat[titanic$Age>=17 & titanic$Age<=32] <- "17-32"
titanic$AgeCat[titanic$Age>=33 & titanic$Age<=48] <- "33-48"
titanic$AgeCat[titanic$Age>=49 & titanic$Age<=64] <- "49-64"
titanic$AgeCat[titanic$Age>=65]  <- "65 and Above"

#changing suvived column to labels
titanic$Survived[titanic$Survived==0] <- 'Not Survived'
titanic$Survived[titanic$Survived==1] <- 'Survived'

#changing to factors for statistical modelling
titanic$Pclass <- factor(titanic$Pclass)
titanic$AgeCat <- factor(titanic$AgeCat)
titanic$Survived <- factor(titanic$Survived)
titanic$Embarked <- as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked == 'S'] <- "Southampton"
titanic$Embarked[titanic$Embarked == 'C'] <- "Cherbourg"
titanic$Embarked[titanic$Embarked == 'Q'] <- "Queenstown"
titanic$Embarked <- factor(titanic$Embarked)

#I believe this is not important
titanic=titanic[c(-9,-11)]

View(titanic)

#write.csv(titanic,file="E:/BA with R/Titanic project with R/titanicNew.csv")
decision_tree <- titanic
SibSpCat <- ifelse(decision_tree$SibSp >=3,">=3","<3")
decision_tree <- data.frame(decision_tree,SibSpCat)
decision_tree$SibSpCat <- as.factor(decision_tree$SibSpCat)
ParchCat = ifelse(decision_tree$Parch >=3,">=3","<3" )
decision_tree <- data.frame(decision_tree,ParchCat)
decision_tree$ParchCat <- as.factor(decision_tree$ParchCat)

set.seed(1)
test=sample(1:nrow(decision_tree),nrow(decision_tree)/3)
train=-test
training_data = decision_tree[train,]
testing_data = decision_tree[test,]
testing_survived = decision_tree$Survived[test]


tree_model=rpart(Survived ~ Pclass + Sex + AgeCat + Embarked + SibSpCat + ParchCat, 
                 data = training_data, method='class',
                 control = rpart.control(minsplit = 10,cp = 0.00))
fancyRpartPlot(tree_model,sub="decision_tree")
tree_predict=predict(tree_model,testing_data,type="class")
mean(tree_predict != testing_survived)

#Selecting important variables from decision tree
#Sex #Embarked #Survived

titanicUpdated<-titanic
SurvivedNum<-ifelse(titanicUpdated$Survived=="Not Survived",0,1)
titanicUpdated <-data.frame(titanicUpdated,SurvivedNum)

SexN<-ifelse(titanicUpdated $Sex=="male",1,0)
titanicUpdated <-data.frame(titanicUpdated, SexN)

EmbarkedN<-ifelse(titanicUpdated$Embarked=="Southampton",1,ifelse(titanicUpdated $Embarked=="Cherbourg",2,0))
titanicUpdated <-data.frame(titanicUpdated, EmbarkedN)

write.csv(titanicUpdated,file = "E:/BA with R/Titanic project with R/titanicUpdated.csv")

titanic.scaled <- scale(data.frame(titanic$Age,titanic$Parch,titanic$SibSp,titanic$Fare))
colnames(titanic.scaled)
totwss <- vector()
btwss <- vector()
for(i in 2:15)
{
  set.seed(1234)
  temp=kmeans(titanic.scaled,centers=i)
  totwss[i] <- temp$tot.withinss
  btwss[i] <- temp$betweenss
}

plot(totwss,xlab="Number of cluster",type="b",
     ylab="Total within sum of square")

plot(btwss,xlab="Number of Cluster",type="b",
     ylab="Total between sum of squares")

#Integrating the visulization in Tableau
install.packages("Rserve")
library(Rserve)
Rserve()
