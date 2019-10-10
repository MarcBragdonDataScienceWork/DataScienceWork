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


setwd("C:\\Users\\Vince\\Documents\\GitHub\\SyracuseProjects\\IST707_Project")

rentTheRunwayPath <- "data\\cleanRR.csv"

CleanRR.df<-read.csv(rentTheRunwayPath)

df<-CleanRR.df
df$chestsize<-as.numeric(gsub("[^0-9.-]", "", df$`bust.size`))
df$size<-as.numeric(df$size)
df$weight<-as.numeric(df$weight)
df$height<-as.numeric(df$height)
df$item_id<-as.factor(df$item_id)
df$user_id<-as.factor(df$user_id)

#rrsample.df$fit<-as.character(df$fit)

#rrsample.df$fit<-gsub("fit", 0, df$fit)
#rrsample.df$fit<-gsub("large", 1, df$fit)
#rrsample.df$fit<-gsub("small", -1, df$fit)
#rrsample.df$fit<-as.numeric(.df$fit)

set.seed(1250)
sample = sample.split(df$user_id, SplitRatio = .75)
train.df = subset(df, sample == TRUE)
test.df = subset(df, sample == FALSE)

train.df<-train.df[,c("item_id", "weight", "height", "chestsize")]
train.df$item_id<-as.character(train.df$item_id)
train.df$item_id<-as.factor(train.df$item_id)


test.df<-test.df[,c("item_id", "weight", "height", "chestsize")]

test.nolbl.df<-test.df[,-1]
test.lbl.df<-test.df[,1]




#train.df<-train.df[,c(-1,-3, -5, -7,-8, -9,  -11, -12, -16, -17)]
#test.df<-test.df[,c(-1,-3, -5, -7,-8, -9,  -11, -12, -16, -17)]

####rpart model single fold
#dtree.model <- rpart(body.type~bust.size+weight+height+size,data=train.df ,method="class",minsplit=10)
#summary(dtree.model)
#plot(dtree.model)
#fancyRpartPlot(dtree.model)



####################################################################
####caret
train_control<-trainControl(method = "cv", number = 3)
caret.dtree.model<- train(item_id~.,data=train.df ,method="rpart",minsplit=1000, trControl=train_control)


caret.dtree.prediction <- predict(caret.dtree.model, test.nolbl.df, type="raw")
dt.df<-data.frame(caret.dtree.prediction,test.lbl.df)


dtTable<-table(caret.dtree.prediction,test.lbl.df)
fancyRpartPlot(caret.dtree.model)
plot(caret.dtree.model)

conf<-confusionMatrix(dtTable, test.lbl.df, positive = NULL,
                      dnn = c("Prediction", "Reference"), prevalence = NULL,
                      mode = "sens_spec")