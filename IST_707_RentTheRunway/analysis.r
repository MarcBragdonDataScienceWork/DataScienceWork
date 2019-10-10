# #install.packages("jsonlite")
# #install.packages("tidyr")
# #install.packages("lubridate")
# install.packages("tidyr")
# install.packages("lubridate")
# install.packages("class")
# install.packages("ggplot2")
# install.packages("lattice")
# install.packages("naivebayes")
# install.packages("e1071")
# install.packages("rpart")
# install.packages("caret")
# install.packages("caTools")
# install.packages("plyr")
# install.packages("rpart.plot")
# install.packages("rattle")
# install.packages("RColorBrewer")
# install.packages("dplyr")
# install.packages("randomForest")
# install.packages("plotly")
library(e1071)

library(plotly)
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
library(dplyr)

##### INIT #########
# Set up working directory 
# Set data paths

setwd("C:\\Users\\Vince\\Documents\\GitHub\\SyracuseProjects\\IST707_Project")

rentTheRunwayPath <- "data/renttherunway_final_data.json"

##### Data Loading ##########
# stream in json file 
# push to dataframe


#stream in from json
rentrunwaystream <- stream_in(file(rentTheRunwayPath))
str(rentrunwaystream)

#saves us from restreaming
rentrunwaydf <- rentrunwaystream

#df with all values for exploration
RTRdf <- rentrunwaystream

#explore data
#visual age range
RTRdf$age <- as.numeric(RTRdf$age)
hist(RTRdf$age)


######Data Cleaning ######################
# 1) Changed fit to factor
# 2) Change category to factor
# 3) Converted Heigh to inches
# 4) Removed lbs from weight and made numeric
# 5) Changed age to numeric
# 6) Removed anyone younger than 16 or older than 89
# 7) Remove anyone sized greater than 22
# 8) Body type to factor
# 9) Bust size to factor
#10) Rating is changed to ordered factor

head(rentrunwaydf)


#convert fit and category to factors
rentrunwaydf$fit <- factor(rentrunwaydf$fit)
rentrunwaydf$category <- factor(rentrunwaydf$category)

#convert the factor height to inches. 
#split the string height on the inches symbol and grab both numbers
rentrunwaydf$height <- sapply(strsplit(as.character(rentrunwaydf$height),"'|\""),
                              function(height){12*as.numeric(height[1]) + as.numeric(height[2])})

#remove lbs from weight and make it numeric
rentrunwaydf$weight <- as.numeric(lapply(rentrunwaydf$weight, gsub, pattern='lbs', replacement=''))
rentrunwaydf <- subset(rentrunwaydf,rentrunwaydf$weight >= 75)

#age is a string in the data, make it numeric
rentrunwaydf$age <- as.numeric(rentrunwaydf$age)
rentrunwaydf <- subset(rentrunwaydf,rentrunwaydf$age>15)
rentrunwaydf <- subset(rentrunwaydf,rentrunwaydf$age < 90)
#sizes go out to 58 we are going to cap the max size offered  22
rentrunwaydf <- subset(rentrunwaydf,rentrunwaydf$size <= 22)



#body type is a factor too
rentrunwaydf$`body type` <- factor(rentrunwaydf$`body type`)
rentrunwaydf$`bust size` <- factor(rentrunwaydf$`bust size`)
rentrunwaydf$item_id <- factor(rentrunwaydf$item_id)
rentrunwaydf$user_id <- factor(rentrunwaydf$user_id)

###  Remove party:  Cocktail
rentrunwaydf$'rented for'[rentrunwaydf$'rented for' == "party: cocktail"] <- "party"
rentrunwaydf$`rented for` <- as.character(rentrunwaydf$`rented for`)
rentrunwaydf$`rented for` <- factor(rentrunwaydf$`rented for`)


#############################    Create Boxplots of the data 
#############################
#############################
tempDF<-rentrunwaydf[, c("weight","size", "height", "age")]
boxplot(tempDF$weight, xlab = "Weight", ylab = "Pounds", main= "Weight Boxplot")
boxplot(tempDF$age, xlab = "age", ylab = "Years Old", main= "Age Boxplot")
boxplot(tempDF$height, xlab = "Height", ylab = "Inches", main= "Height Boxplot")
boxplot(tempDF$size, xlab = "Size", ylab = "Womens Sizes", main= "Size Boxplot")
remove(tempDF)


##############
##############  We can add weight bins
##############
#summary(rentrunwaydf$weight)
rentrunwaydf$WeightBin<-cut(rentrunwaydf$weight, breaks = c(50, 100, 150, 200, 250, Inf), labels = c("Tiny","Small", "Medium", "Large", "x-Large"))


##############
##############  Should we convert user_id and item_id into a factor?
##############

#rating is numeric
#rentrunwaydf$rating <- factor(rentrunwaydf$rating, ordered=TRUE)
rentrunwaydf$rating <- factor(rentrunwaydf$rating)
str(rentrunwaydf)

sum(is.na(rentrunwaydf))


##### Data Cleaning Part Two Create Features ##########
#   Create feature set, by relacing missing values with
#   True/False in new varaibles
#
#------------------------------------------------
rrFeatureSet<-rentrunwaydf
rrFeatureSet$providedWeight<-!is.na(rrFeatureSet$weight)
rrFeatureSet$providedBodytype<-!is.na(rrFeatureSet$`body type`)
rrFeatureSet$providedBustsize<-!is.na(rrFeatureSet$`bust size`)
#   remove the weight, body type and bust size columns
rrFeatureSet<-rrFeatureSet[,-c(3, 5, 9)]
#   see result of featureset
print(NAsByColumn(rrFeatureSet))
str(rrFeatureSet)






###Data Cleaning Part Three Removing NAs##############
#
# Part 1) Count and identfy the number of NAs
# Part 2) Remove the values that aren't complete cae
#
###############################################
#   function to return the NAs for all variables in
#   the passed dataframe
#
#------------------------------------------------
NAsByColumn<-function  (x){
  na_count <-sapply(x, function(y) sum(length(which(is.na(y)))))
  na_count <- data.frame(na_count)
  return(na_count)
}

ColVariableType<-function (x) {
  dataTypes <-sapply(x, function(y) typeof(y))
  dataTypes <- data.frame(dataTypes)
  return(dataTypes)
}



###############################################
#   Call our function to return all of the columns that have NAs
#
#------------------------------------------------
print(NAsByColumn(rentrunwaydf))


###############################################
#   Call our function to return each variables type
#
#------------------------------------------------
print(ColVariableType(rentrunwaydf))

#Created a new data frame with only complete cases.
CleanRR.DF<-rentrunwaydf



#Check to see if there are any NAs in the dataframe
if (sum(is.na(rentrunwaydf))>0){
  #replace withonly complete cases in the dataframes
  CleanRR.DF<-rentrunwaydf[complete.cases(rentrunwaydf), ]  
}
###remove(CleanRR.DF)   #Remove this 
#install.packages("lubridate")

CleanRR.DF$review_date<-mdy(CleanRR.DF$review_date)
str(CleanRR.DF)
unique(rentrunwaydf$category)
unique(rentrunwaydf$'rented for')

################### END OF DATA CLEANING##########################
#CleanRR.DF$user_id<-as.factor(CleanRR.DF$user_id)
#CleanRR.DF$item_id<-as.factor(CleanRR.DF$item_id)
summary(CleanRR.DF)

count.data <-  CleanRR.DF 

count.users <- count.data %>% count(user_id, sort = TRUE, name="count") %>% subset(count > 73)

subset.top.renters <- subset(CleanRR.DF, CleanRR.DF$user_id %in% count.users$user_id)

top.renter <- subset(subset.top.renters, subset.top.renters$user_id %in% c(691468))


nrow(subset.top.renters)

user.freq.plot <- plot_ly(
  
  x=count.users$user_id,
  y=count.users$count,
  name="Top Users Count",
  type="bar"
  
)
user.freq.plot




#explore data
#Find min and max for age
min(CleanRR.DF$age)
max(CleanRR.DF$age)
str(CleanRR.DF)
range(CleanRR.DF$age)
range(CleanRR.DF$size)
hist(CleanRR.DF$age)

hist(CleanRR.DF$`rented for`)
CleanRR.DF$`rented for` <- factor(CleanRR.DF$`rented for`)

#visualize the data and explore variables

#visualize the rented for totals
barchart(CleanRR.DF$`rented for`)

#visualize the category
barchart(CleanRR.DF$category)

#visualize the rating totals in a pie chart
rating = CleanRR.DF$rating
rating.freq = table(rating)
colors = c("yellow","green", "blue", "hot pink", "cyan") 
pie(rating.freq, col = colors, main = "Rating Totals")

#visualize the size totals in a pie chart
size = CleanRR.DF$size
size.freq = table(size)
colors = c("yellow","green", "blue", "hot pink", "cyan", "lavender", "orange", "purple", "pink", "dark blue") 
pie(size.freq, col = colors, main = "Size Totals")

#find the min, max, mean, median for size
min(CleanRR.DF$size)
max(CleanRR.DF$size)
mean(CleanRR.DF$size)
median(CleanRR.DF$size)

#find the mean and median for age - already removed under 16 and above 88
mean(CleanRR.DF$age)
median(CleanRR.DF$age)
#########\
testdf<-as.factor(CleanRR.DF$size)
table(testdf)
barplot(table(testdf), xlab = "Size", ylab = "Count", main = "Customer Size Breakdown", col=brewer.pal(8,'Pastel2'))
remove(testdf)

testdf<-as.factor(CleanRR.DF$'body type')
table(testdf)
barplot(table(testdf), xlab = "Size", ylab = "Count", main = "Customer Bodytype Breakdown", col=brewer.pal(8,'Pastel2'))
remove(testdf)


##################    Rent the runway charts
##################
##################
##################
##################

table(CleanRR.DF$fit)
barplot(table(CleanRR.DF$fit), xlab = "Size", ylab = "Count", main = "Fit Breakdown", col=brewer.pal(8,'Pastel2'))
barplot(table(CleanRR.DF$rating), xlab = "Rating", ylab = "Count", main = "Rating Breakdown", col=brewer.pal(8,'Pastel2'))
barplot(table(CleanRR.DF$'rented for'), xlab = "Rented For", ylab = "Count", main = "Rented For Breakdown", col=brewer.pal(8,'Pastel2'))




#-----------------------------------------------------------------------------------------------
###############################################
#   Effort to create a market basket/transaction
#   
#
#------------------------------------------------
library(plyr)
#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
transactionData <- ddply(CleanRR.DF,c("user_id","review_date"),
                         function(df1)paste(df1$item_id,
                                            collapse = ","))

transactionData <- subset(transactionData,grepl(",",transactionData$V1))

grepl("*a*","Marc")
grepl(",",transactionData$V1)

###############################################
#   Install arules packages
#
#------------------------------------------------
#install.packages("arules")
#library(arules)
#install.packages("arulesViz")
#library(arulesViz)
transactionData$user_id<-as.factor(transactionData$user_id)
transactionData$user_id<-NULL
transactionData$review_date<-NULL
colnames(transactionData)<-c("items")

###############################################
#   Save the file so we can read it back in as a transaction
#
#------------------------------------------------
#write.csv(transactionData,"C:/Users/Vince/Documents/GitHub/SyracuseProjects/IST707_Project/market_basket_transactions.csv", quote = FALSE, row.names = TRUE)
#remove(tr)
###  If reading from the file use the following
tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

###convert the transactionData to a transaction
#tr<-as(transactionData, "transactions")

###You can obtain the numbner of rows and columns
###and density is the matrix, the most frequent
###items rented
summary(tr)

#install.packages("RColorBrewer")
library(RColorBrewer)
###Absolute Item frequency
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

###Relative frequency
itemFrequencyPlot(tr,topN=10,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")

###############################################
#   Generate association rules
#
#------------------------------------------------
association.rules <- apriori(tr)
summary(association.rules)
#End of association rule mining
#-----------------------------------------------------------------------------------------------


###############################################
#   Arules 2, looking at all of the data across
#   the fields
#   1.  Fit
#   2.  Bust size
#   3.  rented for
#   4.  body type
#------------------------------------------------
library(sqldf)
arules.DF<-CleanRR.DF
arules.DF<-arules.DF[,c(1,3, 7, 9)]
arules.DF$`rented for`<-as.factor(arules.DF$'rented for')
arules.DF$`rented for`<-as.factor((arules.DF$`rented for`))
write.csv(arules.DF, file = "arulesdata.csv")
tr2 <- read.transactions('arulesdata.csv', format = 'basket', sep=',')
summary(tr2)
tr2 <- as(tr2, "transactions")
dim(tr2)
itemLabels(tr2)
itemFrequencyPlot(tr2,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Frequency Plot")

rules <- apriori(tr2, parameter = list(supp = 0.001, conf = 0.5, minlen=2, maxlen=5000))

######################
#####
#####   Playing arouund with arules
#####

#rules <- apriori(tr2, parameter = list(supp = 0.001, conf = 0.8, minlen=2, maxlen=5000))

#rules<-apriori(data=tr2, parameter=list(supp=0.001,conf = 0.1, minlen=2), 
#                   appearance = list(default="rhs",lhs="hourglass"),
#                   control = list(verbose=T))
#rulesSort<-sort(rules, decreasing=TRUE,by="confidence")
#inspect(rulesSort)




options(digits = 2)
#sort rules so that we can view the most relevant
rules <- sort(rules, by="confidence", decreasing=TRUE)
inspect(rules)
summary(rules)



###############################################
#   Create a feature Set that contains only review data
#   
#
#------------------------------------------------
review.DF<-CleanRR.DF[,c("user_id", "item_id", "review_text", "review_summary", "review_date")]

#----------------------------------------------------------------------------------------------
#Some of our ages are of range we need to clean these out when age is a factor
#length(CleanRR.DF[CleanRR.DF$age < 15,]$age)

###############################################
#   createa a small dataset for testing
#   
#
#------------------------------------------------
sample = sample.split(CleanRR.DF$user_id, SplitRatio = .1)
rrsample.df = subset(CleanRR.DF, sample == TRUE)



#####################################################
#
# KMeans Clustering in H2O
#
#
####################################################

install.packages("h2o")
library(h2o)
h2o.init()


bodytype.DF <- CleanRR.DF[,c("height","weight","bust size","body type","age")]
summary(bodytype.DF)

h2o.Kmeans.DF <- as.h2o(bodytype.DF)

num.body.types <- length(unique(h2o.Kmeans.DF$`body type`))

x <- c("height","weight","bust size","body type")

kmeans.model <- h2o.kmeans(Kmeans.DF,x=x, k=100)
summary(kmeans.model)
centers <- as.data.frame(kmeans.model@model$centers)
print(centers)

h2o.rf.DF <- as.h2o(bodytype.DF)

#set your x's and y's for training
y <- "body type"
x <- setdiff(names(h2o.rf.DF),y)
rf.model <- h2o.randomForest(x=x,y=y,training_frame = h2o.rf.DF,nfold=10,ntrees=30)
summary(rf.model)
plot(rf.model)
############################################################################
#create sample of RR
# Now Selecting 85% of data as sample from total 'n' rows of the data  
rrsample = sample.split(CleanRR.DF, SplitRatio = .99)
rrtrainsample = subset(CleanRR.DF, rrsample == TRUE)
rrtestsample  = subset(CleanRR.DF, rrsample == FALSE)

install.packages("doMC")
library(foreach)
library(doMC)
library(h2o)
h2o.init()

registerDoMC(24)
#remove labels from testing data


rrtestsample <- CleanRR.DF[,c("category","age","rented for","item_id")]
rrtestsample$`rented for` <- as.factor(rrtestsample$`rented for`)
rrtestsample$item_id <- as.factor(rrtestsample$item_id)


str(rrtestsample)

head(CleanRR.DF)
head(rrtestsample_nolabel)

h2o.dt.df <- as.h2o(rrtestsample)

y <- "item_id"
x <- setdiff(names(h2o.dt.df),y)

h2o.rf.model <- h2o.randomForest(x,y,h2o.dt.df,nfold=5,ntrees=30)

plot(h2o.rf.model)





########################################################################

library(doMC)

print("Registering Cores for DOMC Parallel Processing")
registerDoMC(cores=24)


random.forest.data <- CleanRR.DF[,c("height","weight","size","rating","age")]

train.data <- random.forest.data[sample(nrow(random.forest.data),50000, replace=FALSE),]
test.data <- random.forest.data[sample(nrow(random.forest.data),80000, replace=FALSE),]

train.x <- subset(train.data, select = -rating)
train.y <- train.data$rating

test.x <- subset(test.data, select = -rating)
test.y <- test.data$rating


train.control <- trainControl(method="cv",
                              number = 5, allowParallel = TRUE)

#set up model selection method
mtry <- sqrt(ncol(train.x))
#grid.rf <- expand.grid(.mtry = c(2, 4, 8, 16))
grid.rf <- expand.grid(.mtry = mtry)


print("Training Model Using Random Forest")
rf.model <- train(train.x,train.y, ntrees=10, trControl=train.control,tuneGrid=grid.rf)
summary(rf.model)





print("Making Prediction with RF Model")
rf.predictions <- predict(rf.model, test.x)


hist(count(test.data$rating))

print("Summary of RF Model")
print(rf.model)
summary(rf.model)



print("Table of RF Predictions")
table(rf.predictions,test.y)

rf.value.table <- merge(rf.predictions,test.y)

confusionMatrix(rf.predictions,test.y)


ggplot(data = rf.value.table, aes(x=rf.predictions, y=test.y, fill=value)) + 
  geom_tile()
###################################################################################

random.forest.data <- CleanRR.DF[,c("height","weight","size","rating","age")]

train.data <- random.forest.data[sample(nrow(random.forest.data),50000, replace=FALSE),]
test.data <- random.forest.data[sample(nrow(random.forest.data),80000, replace=FALSE),]

train.x <- subset(train.data, select = -rating)
train.y <- train.data$rating

test.x <- subset(test.data, select = -rating)
test.y <- test.data$rating


train.control <- trainControl(method="cv",
                              number = 5, allowParallel = TRUE)

#set up model selection method
mtry <- sqrt(ncol(train.x))
#grid.rf <- expand.grid(.mtry = c(2, 4, 8, 16))
grid.rf <- expand.grid(.mtry = mtry)


print("Training Model Using Random Forest")
rf.model <- train(train.x,train.y, ntrees=10, trControl=train.control,tuneGrid=grid.rf)
summary(rf.model)

print("Making Prediction with RF Model")
rf.predictions <- predict(rf.model, test.x)


hist(count(test.data$rating))

print("Summary of RF Model")
print(rf.model)
summary(rf.model)



print("Table of RF Predictions")
table(rf.predictions,test.y)

rf.value.table <- merge(rf.predictions,test.y)

confusionMatrix(rf.predictions,test.y)


ggplot(data = rf.value.table, aes(x=rf.predictions, y=test.y, fill=value)) + 
  geom_tile()

#################################################################################


random.forest.data <- CleanRR.DF[,c("height","weight","size","body type","age")]
colnames(random.forest.data )[colnames(random.forest.data)=="old_name"] <- "new_name"

train.data <- random.forest.data[sample(nrow(random.forest.data),50000, replace=FALSE),]
test.data <- random.forest.data[sample(nrow(random.forest.data),80000, replace=FALSE),]

train.x <- subset(train.data, select = -"body type")
train.y <- train.data$'body type'

test.x <- subset(test.data, select = -"body type")
test.y <- test.data$'body type'


train.control <- trainControl(method="cv",
                              number = 5, allowParallel = TRUE)

#set up model selection method
mtry <- sqrt(ncol(train.x))
#grid.rf <- expand.grid(.mtry = c(2, 4, 8, 16))
grid.rf <- expand.grid(.mtry = mtry)


print("Training Model Using Random Forest")
rf.model <- train(train.x,train.y, ntrees=10, trControl=train.control,tuneGrid=grid.rf)
summary(rf.model)





print("Making Prediction with RF Model")
rf.predictions <- predict(rf.model, test.x)


hist(count(test.data$rating))

print("Summary of RF Model")
print(rf.model)
summary(rf.model)



print("Table of RF Predictions")
table(rf.predictions,test.y)

rf.value.table <- merge(rf.predictions,test.y)

confusionMatrix(rf.predictions,test.y)




#################################
################################
#SVM



train.control <- trainControl(method="cv",
                              number = 10, allowParallel = T) 
print("Train Model")
svm.model <- train(train.x,train.y, method="svmLinear", trControl = train.control)

print("Make Predictions with SVM Model")
svm.predictions <- predict(svm.model, test.x)



print("Summary of SVM Model")
summary(svm.model)


print("Table of SVM Predictions")
svm.confusion.matrix <- confusionMatrix(svm.predictions,test.x)
svm.confusion.matrix
plot(svm.model)

mosaic(svm.confusion.matrix$table)


########################## Naive Bayes model#################################################
#create new sample with rented for, age, item predicting rented for 
rrNB2 <- CleanRR.DF[c("rented for","age","item_id")]
rrNB2$age <-as.factor(rrNB2$age)
rrNB2$item_id <- as.factor(rrNB2$item_id)
colnames(rrNB2) <- c("rented_for", "age","item_id")

# Now Selecting 90% of data as sample from total 'n' rows of the data  
rrsampleNB2 = sample.split(rrNB2, SplitRatio = .90)
rrtrainsampleNB2 = subset(rrNB2, rrsampleNB2 == TRUE)
rrtestsampleNB2  = subset(rrNB2, rrsampleNB2 == FALSE)
colnames(rrtestsampleNB2) <- c("rented_for", "age","item_id")
colnames(rrtrainsampleNB2) <- c("rented_for", "age","item_id")

#remove "rented for" from testing data
rrtestsampleNB2_nolabel<-rrtestsampleNB2[-c(1)]
rrtestsampleNB2_justlabel<-rrtestsampleNB2$rented_for
head(rrtestsampleNB2_nolabel)

#naive bayes model
NBtrainsample2 <- naiveBayes(rented_for~., data=rrtrainsampleNB2, na.action =na.pass)
NB_pred2 <- predict(NBtrainsample2, rrtestsampleNB2_nolabel)
table(NB_pred2, rrtestsampleNB2_justlabel)
NB_table2 <- table(NB_pred2, rrtestsampleNB2_justlabel)
NB_table2

#NB confusion matrix 
confusionMatrix(NB_table2, rrtestsampleNB2$rented_for, positive = NULL, dnn = c("Prediction", "Reference"), prevalence = NULL, mode = "prec_recall")
##only a 36% accuracy testing again with differentvariables

##############testing with category instead of item id for predicting rented for ##########################
#create new sample with rented for, age, category
rrNB3 <- CleanRR.DF[c("rented for","age","category")]
rrNB3$age <-as.factor(rrNB3$age)
rrNB3$item_id <- as.factor(rrNB3$item_id)
colnames(rrNB3) <- c("rented_for", "age","category")

# Now Selecting 90% of data as sample from total 'n' rows of the data  
rrsampleNB3 = sample.split(rrNB3, SplitRatio = .90)
rrtrainsampleNB3 = subset(rrNB3, rrsampleNB3 == TRUE)
rrtestsampleNB3  = subset(rrNB3, rrsampleNB3 == FALSE)
colnames(rrtestsampleNB3) <- c("rented_for", "age","category")
colnames(rrtrainsampleNB3) <- c("rented_for", "age","category")

#remove "rented for" from testing data
rrtestsampleNB3_nolabel<-rrtestsampleNB3[-c(1)]
rrtestsampleNB3_justlabel<-rrtestsampleNB3$rented_for
head(rrtestsampleNB3_nolabel)

#naive bayes model 
NBtrainsample3<- naiveBayes(rented_for~., data=rrtrainsampleNB3, na.action =na.pass)
NB_pred3 <- predict(NBtrainsample3, rrtestsampleNB3_nolabel)
table(NB_pred3, rrtestsampleNB3_justlabel)
NB_table3 <- table(NB_pred3, rrtestsampleNB3_justlabel)
NB_table3

#NB confusion matrix 
confusionMatrix(NB_table3, rrtestsampleNB3$rented_for, positive = NULL, dnn = c("Prediction", "Reference"), prevalence = NULL, mode = "prec_recall")
#####accuracy is better at 42% with category instead of item id

##Visualize
plot(NB_table3, xlab = "Prediction", ylab = "Label", main = "Naive Bayes Model for Rented For")


##############new naive bayes with age, weight, height, body type, predicting size###############
# rrNB4 <- CleanRR.DF[c("size","age", "weight", "height", "body type" )]
# rrNB4$size <-as.factor(rrNB4$size)
# rrNB4$weight <-as.factor(rrNB4$weight)
# rrNB4$height<-as.factor(rrNB4$height)
# rrNB4$age <-as.factor(rrNB4$age)
# 
# # Now Selecting 90% of data as sample from total 'n' rows of the data  
# rrsampleNB4 = sample.split(rrNB4, SplitRatio = .90)
# rrtrainsampleNB4 = subset(rrNB4, rrsampleNB4 == TRUE)
# rrtestsampleNB4 = subset(rrNB4, rrsampleNB4 == FALSE)
# 
# #remove "rented for" from testing data
# rrtestsampleNB4_nolabel<-rrtestsampleNB4[-c(1)]
# rrtestsampleNB4_justlabel<-rrtestsampleNB4$size
# head(rrtestsampleNB4_nolabel)
# 
# #naive bayes model for size
# NBtrainsample4<- naiveBayes(size~., data=rrtrainsampleNB4, na.action =na.pass)
# NB_pred4 <- predict(NBtrainsample4, rrtestsampleNB4_nolabel)
# table(NB_pred4, rrtestsampleNB4_justlabel)
# NB_table4 <- table(NB_pred4, rrtestsampleNB4_justlabel)
# NB_table4
# 
# #NB confusion matrix 
# confusionMatrix(NB_table4, rrtestsampleNB4$size, positive = NULL, dnn = c("Prediction", "Reference"), prevalence = NULL, mode = "prec_recall")
# #####accuracy is better at 37% with category instead of item id
# 
# ##Visualize
# plot(NB_table4, xlab = "Prediction", ylab = "Label", main = "Naive Bayes Model for Size")

##############new naive bayes to predict fit on item - item id, fit, weight, height, body type, size
rrNB5 <- CleanRR.DF[c("fit", "size","weight", "height", "body type", "item_id" )]
rrNB5$size <-as.factor(rrNB5$size)
rrNB5$weight <-as.factor(rrNB5$weight)
rrNB5$height<-as.factor(rrNB5$height)
rrNB5$item_id <-as.factor(rrNB5$item_id)

# Now Selecting 90% of data as sample from total 'n' rows of the data  
rrsampleNB5 = sample.split(rrNB5, SplitRatio = .90)
rrtrainsampleNB5 = subset(rrNB5, rrsampleNB5 == TRUE)
rrtestsampleNB5 = subset(rrNB5, rrsampleNB5 == FALSE)

#remove "fit" from testing data
rrtestsampleNB5_nolabel<-rrtestsampleNB5[-c(1)]
rrtestsampleNB5_justlabel<-rrtestsampleNB5$fit
head(rrtestsampleNB5_nolabel)

#naive bayes model for fit
NBtrainsample5<- naiveBayes(fit~., data=rrtrainsampleNB5, na.action =na.pass)
NB_pred5 <- predict(NBtrainsample5, rrtestsampleNB5_nolabel)
table(NB_pred5, rrtestsampleNB5_justlabel)
NB_table5 <- table(NB_pred5, rrtestsampleNB5_justlabel)
NB_table5

#NB confusion matrix 
confusionMatrix(NB_table5, rrtestsampleNB5$fit, positive = NULL, dnn = c("Prediction", "Reference"), prevalence = NULL, mode = "prec_recall")
#####accuracy is 62% predicting the fit of the item with only 77% precision for fit, and 18% 

##Visualize
plot(NB_table5, xlab = "Prediction", ylab = "Label", main = "Naive Bayes Model for Fit")

####################################################knn model ##############################################
#knn model for rented for, age, category
#create new sample with rented for, age, category
# rrknn3 <- CleanRR.DF[c("rented for","age","category")]
# rrknn3$age <-as.factor(rrknn3$age)
# rrknn3$item_id <- as.factor(rrknn3$item_id)
# colnames(rrknn3) <- c("rented_for", "age","category")
# 
# # Now Selecting 90% of data as sample from total 'n' rows of the data  
# rrsampleknn3 = sample.split(rrknn3, SplitRatio = .90)
# rrtrainsampleknn3 = subset(rrNB3, rrsampleknn3 == TRUE)
# rrtestsampleknn3  = subset(rrNB3, rrsampleknn3 == FALSE)
# colnames(rrtestsampleknn3) <- c("rented_for", "age","category")
# colnames(rrtrainsampleknn3) <- c("rented_for", "age","category")
# 
# #need to normalize the data create norm function
# norm <- function(x) { (x - min(x)) / (max(x) - min(x)) }
# 
# #
# normalize(rrknn3$age, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
# 
# TestSet_numonly <- testsample[,-1]
# TestSet_labels <- testsample[,1]
# TrainSet_numonly <- trainsample[,-1]
# TrainSet_labels <- trainsample[,1]
# 
# kNNModel1 <- class::knn(train=TrainSet_numonly, test=TestSet_numonly,
#                         cl=TrainSet_labels, prob=TRUE)
# print(kNNModel1)
# 
# knntable <-table(kNNModel1, TestSet_labels)
# 
# #knn confusion matrix
# confusionMatrix(kNNModel1, testsample$label)

#####################
######################
#######NEW KNN###########
#######################knn modelwith item id, size, height, weight predicting fit#######################################
#knn model to predict fit
rrknn2 <- CleanRR.DF[c("fit", "item_id","weight", "height", "size")]
rrknn2$size <- as.numeric(rrknn2$size)
rrknn2$item_id <- as.numeric(rrknn2$item_id)
rrknn2$fit <- as.character(rrknn2$fit)

# Now Selecting 90% of data as sample from total 'n' rows of the data  
rrsampleknn2 = sample.split(rrknn2, SplitRatio = .90)
rrtrainsampleknn2 = subset(rrknn2, rrsampleknn2 == TRUE)
rrtestsampleknn2 = subset(rrknn2, rrsampleknn2 == FALSE)

TestSet_numonly <- rrtestsampleknn2[,-1]
TestSet_labels <- rrtestsampleknn2[,1]
TrainSet_numonly <- rrtrainsampleknn2[,-1]
TrainSet_labels <- rrtrainsampleknn2[,1]

kNNModel2 <- class::knn(train=TrainSet_numonly, test=TestSet_numonly,
                        cl=TrainSet_labels,prob=TRUE)
print(kNNModel2)

knntable2 <-table(kNNModel2, TestSet_labels)
knntable2

#knn confusion matrix for fit
confusionMatrix(knntable2, rrtestsampleknn2$fit)
#visualize the tables
plot(knntable2, xlab = "Fit", ylab = "Prediction", main = "kNN Model for Fit")
#### 66 percent accuracy in predicting fit

#######################knn model AGAIN with item id, size, height, weight to predict body type#######################################
#knn model with same variables to predict body type
rrknn3 <- CleanRR.DF[c("body type", "item_id","weight", "height", "size")]
rrknn3$size <- as.numeric(rrknn3$size)
rrknn3$`body type`<- as.character(rrknn3$`body type`)

# Now Selecting 90% of data as sample from total 'n' rows of the data  
rrsampleknn3 = sample.split(rrknn3, SplitRatio = .90)
rrtrainsampleknn3 = subset(rrknn3, rrsampleknn3 == TRUE)
rrtestsampleknn3 = subset(rrknn3, rrsampleknn3 == FALSE)

TestSet_numonly <- rrtestsampleknn3[,-1]
TestSet_labels <- rrtestsampleknn3[,1]
TrainSet_numonly <- rrtrainsampleknn3[,-1]
TrainSet_labels <- rrtrainsampleknn3[,1]

kNNModel3 <- class::knn(train=TrainSet_numonly, test=TestSet_numonly,
                        cl=TrainSet_labels, prob=TRUE)
print(kNNModel3)

knntable3 <-table(kNNModel3, TestSet_labels)
knntable3

#knn confusion matrix
confusionMatrix(knntable3, rrtestsampleknn3$`body type`)
####only a 28% accuracy to predict body type

#visualize the knn for body type
plot(knntable3, xlab = "Body Type", ylab = "Prediction", main = "kNN Model for Body Type")





