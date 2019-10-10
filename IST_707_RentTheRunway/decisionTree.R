library(jsonlite)
library(tidyr)
library(lubridate)
library(class)
library(ggplot2)
library(lattice)
library(naivebayes)
library(e1071)
library(rpart)
library(caret)
library(caTools)
library(tidyr)
library(plyr)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(dplyr)
library(randomForest)
library(cluster)
library(doMC)
library(caret)
library(h2o)

setwd("~/GIT/SyracuseProjects/IST707_Project")

#setwd("C:\\Users\\Vince\\Documents\\GitHub\\SyracuseProjects\\IST707_Project")

rentTheRunwayPath <- "data/cleanRR.csv"
print("Reading in Data")
CleanRR.df<-read.csv(rentTheRunwayPath)
print("Cleaning Data")
df<-CleanRR.df
df$chestsize <-as.numeric(gsub("[^0-9.-]", "", df$`bust.size`))
df$cupsize <- gsub("[0-9.-]", "", df$`bust.size`)

df$size<-as.numeric(df$size)
df$weight<-as.numeric(df$weight)
df$height<-as.numeric(df$height)
df$item_id<-as.factor(df$item_id)
df$user_id<-as.factor(df$user_id)
df$cupsize <- as.factor(df$cupsize)



#rrsample.df$fit<-as.character(df$fit)

#rrsample.df$fit<-gsub("fit", 0, df$fit)
#rrsample.df$fit<-gsub("large", 1, df$fit)
#rrsample.df$fit<-gsub("small", -1, df$fit)
#rrsample.df$fit<-as.numeric(.df$fit)
print("Sampling Train Data")
set.seed(1250)
sample = sample.split(df, SplitRatio = .8)
train.df = subset(df, sample == TRUE)

print("Sampling Test Data")
#set.seed(600)
#sample = sample.split(df$user_id, SplitRatio = .50)
test.df = subset(df, sample == FALSE)


train.df<-train.df[,c("cupsize","age","weight", "height", "chestsize","fit")]
test.df<-test.df[,c("cupsize","age","weight", "height", "chestsize","fit")]

#train.df<-train.df[,c(-1,-3, -7,-8, -9,  -11, -12, -16, -17)]
#test.df<-test.df[,c(-1,-3, -7,-8, -9,  -11, -12, -16, -17)]
#train.df<-train.df[,-5]   # remove body.type
#test.df<-test.df[,-5]     # remove body.type
#testlbl<-test.df[,c(1)]
#testnolbl<-test.df[,c(-1)]
#train.df <- train.df[-1]

#column 1 is the fit
#column 2 is bust.size
#column 3 is weight
#column 4 is body.type
#column 5 is height
#column 6 is size
#column 7 is age


print("Running H20")
h2o.no_progress()
h2o.init()


train.h2o <- as.h2o(train.df)
y <- "fit"
x <- setdiff(names(train.h2o),y)

print("Running Decision Tree Model")
dt.model.two <- h2o.randomForest(x=x,y=y,training_frame = train.h2o,nfold=10,ntrees = 1)
summary(dt.model.two)
dt.predict<-h2o.predict(object = dt.model.two, newdata = as.h2o(test.df))
summary(dt.predict)

#Without cupsize
train.df<-df[,c("age","weight", "height", "chestsize","body.type")]
test.df<-df[,c("age","weight", "height", "chestsize","body.type")]


train.h2o <- as.h2o(train.df)
y <- "body.type"
x <- setdiff(names(train.h2o),y)

print("Running Decision Tree Model")
dt.model.two <- h2o.randomForest(x=x,y=y,training_frame = train.h2o,nfold=10,ntrees = 1)
summary(dt.model.two)
dt.predict<-h2o.predict(object = dt.model.two, newdata = as.h2o(test.df))
summary(dt.predict)

str(df)

