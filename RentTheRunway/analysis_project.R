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
#  install.packages("doMC",
#                   repos = "http://cran.r-project.org")


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
library(doMC)


##### INIT #########
# Set up working directory 
# Set data paths

setwd("~/GIT/SyracuseProjects/IST707_Project")

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

###  Remove party:  Cocktail
rentrunwaydf$'rented for'[rentrunwaydf$'rented for' == "party: cocktail"] <- "party"
rentrunwaydf$`rented for` <- as.character(rentrunwaydf$`rented for`)
rentrunwaydf$`rented for` <- factor(rentrunwaydf$`rented for`)


#############################    Create Boxplots of the data 
#############################
#############################
#tempDF<-rentrunwaydf[, c("weight","size", "height", "age")]
#boxplot(tempDF$weight, xlab = "Weight", ylab = "Pounds", main= "Weight Boxplot")
#boxplot(tempDF$age, xlab = "age", ylab = "Years Old", main= "Age Boxplot")
#boxplot(tempDF$height, xlab = "Height", ylab = "Inches", main= "Height Boxplot")
#boxplot(tempDF$size, xlab = "Size", ylab = "Womens Sizes", main= "Size Boxplot")
#remove(tempDF)


##############
##############  We can add weight bins
##############
#summary(rentrunwaydf$weight)
#rentrunwaydf$WeightBin<-cut(rentrunwaydf$weight, breaks = c(50, 100, 150, 200, 250, Inf), labels = c("Tiny","Small", "Medium", "Large", "x-Large"))


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

summary(CleanRR.DF)
write.csv(CleanRR.DF,"data/cleanRR.csv", quote = TRUE)


################### END OF DATA CLEANING##########################

summary(CleanRR.DF)


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
#########

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

#install.packages("h2o")
library(h2o)
h2o.init()
unique(CleanRR.DF$fit)
rrsample.df<-subset(CleanRR.DF, is.element(CleanRR.DF$item_id, top.items))
#sample = sample.split(CleanRR.DF$user_id, SplitRatio = .5)
#rrsample.df = subset(CleanRR.DF, sample == TRUE)
rrsample.df$chestsize<-as.numeric(gsub("[^0-9.-]", "", rrsample.df$`bust size`))
rrsample.df$size<-as.numeric(rrsample.df$size)
rrsample.df$fit<-as.character(rrsample.df$fit)

rrsample.df$fit<-gsub("fit", 0, rrsample.df$fit)
rrsample.df$fit<-gsub("large", 1, rrsample.df$fit)
rrsample.df$fit<-gsub("small", -1, rrsample.df$fit)


rrsample.df$fit<-as.numeric(rrsample.df$fit)
rrkMeans.sample.df<-rrsample.df[,c("fit", "chestsize", "size", "height")]    #, "body type")]


summary(rrsample.df)

#rrmodel<-kmeans(rrkMeans.sample.df, 10)


h2o.rrkMeans.sample.df <- as.h2o(rrkMeans.sample.df)

x <- c("fit","chestsize","size","height")    #,"body type")

h2o.rrkMeans.model <- h2o.kmeans(h2o.rrkMeans.sample.df, x=x, k=10)

summary(h2o.rrkMeans.model)
centers <- as.data.frame(h2o.rrkMeans.model@model$centers)
print(centers)

plot(centers)
#install.packages("plotly")
library(plotly)

centChart<-plot_ly(x=centers$fit, y=centers$chestsize, z=centers$height, type="scatter3d", mode="markers")
centChart

centChart2<-plot_ly(x=centers$fit, y=centers$chestsize, z=centers$size, type="scatter3d", mode="markers")
centChart2

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

y <- "rented for"
x <- setdiff(names(h2o.dt.df),y)

h2o.rf.model <- h2o.randomForest(x,y,h2o.dt.df,nfold=5,ntrees=30)

plot(h2o.rf.model)


rrtree <- rpart(rrtrainsample$`rented for` ~ ., data =rrtrainsample, method = "class")
summary(rrtree)
predicted= predict(rrtree, rrtestsample, type="class")

rpart.plot(rrtree)
fancyRpartPlot(rrtree)


###############knn model #################
#create new sample with age, category, rented for, item id

rrtestsampleKNN <- CleanRR.DF[c("rented for","age","category")]
#rrtestsampleKNN <-rrtestsampleKNN[complete.cases(rrtestsampleKNN),]
#rrtestsampleKNN$age <-as.factor(rrtestsampleKNN$age)
rrtestsampleKNN$item_id <-as.factor(rrtestsampleKNN$item_id)
#colnames(rrtestsampleKNN) <- c("rented_for", "age", "category")



# Now Selecting 99% of data as sample from total 'n' rows of the data  
rrsampleKNN = sample.split(rrtestsampleKNN, SplitRatio = .99)
rrtrainsampleKNN1 = subset(rrtestsampleKNN, rrsampleKNN == TRUE)
rrtestsampleKNN1  = subset(rrtestsampleKNN, rrsampleKNN == FALSE)
#colnames(rrtestsampleKNN1) <- c("rented_for", "age", "category")
#colnames(rrtrainsampleKNN1) <- c("rented_for", "age", "category")


#remove "rented for" from testing data
#rrtestsampleKNN_nolabel<-rrtestsampleKNN[-c(1)]
#rrresttsampleKNN_justlabel<-rrtestsampleKNN$`rented for`
head(rrtestsampleKNN_nolabel)

#knn model
# TestSet_numonly <- rrtestsampleKNN1[,-1]
# TestSet_labels <- rrtestsampleKNN1[,1]
# TrainSet_numonly <- rrtrainsampleKNN1[,-1]
# TrainSet_labels <- rrtrainsampleKNN1[,1]

str(rrtestsampleKNN1)

res <- model.matrix(~., data = rrtestsampleKNN1)



x <- subset(rrtestsampleKNN1, select = -rented_for)
y <- as.factor(rrtestsampleKNN1$rented_for)

#kNNModel1 <- knn(train=TrainSet_numonly, test=TestSet_numonly,
#                        cl=TrainSet_labels,k = 5, prob=TRUE)
registerDoMC(cores=24)

train.control <- trainControl(method="repeatedcv",
                              number = 10, 
                              repeats = 3,
                              allowParallel = T) 

knn.model <- train(x, y, method="knn", trControl=train.control, preProcess =c("center", "scale"))





kNNModel <- train()


#check for NAs since model is not working
(sum(is.na(TrainSet_numonly)))  #there are no NAs...

print(kNNModel1)

knntable <-table(kNNModel1, TestSet_labels)

#knn confusion matrix
confusionMatrix(kNNModel1, testsample$label)

#visualize the tables
plot(knntable)

###################naive bayes model#######################################

#naive bayes model using e1701 with 99% sample with 4 samples
NBtrainsample <- naiveBayes(rented_for~., data=rrtrainsampleKNN1, na.action =na.pass)
NB_pred <- predict(NBtrainsample, rrtestsampleKNN_nolabel)
table(NB_pred, rrresttsampleKNN_justlabel)
NB_table <- table(NB_pred, rrresttsampleKNN_justlabel)
NB_table

#NB confusion matrix 
confusionMatrix(NB_table, rrtestsampleKNN1$rented_for, positive = NULL, dnn = c("Prediction", "Reference"), prevalence = NULL, mode = "prec_recall")

#create new sample with rented for, age, item, category
rrNB2 <- CleanRR.DF[c("rented for","age","category","item_id")]
rrNB2 <-rrNB2[complete.cases(rrNB2),]
rrNB2$age <-as.factor(rrNB2$age)
colnames(rrNB2) <- c("rented_for", "age","category","item_id")

# Now Selecting 90% of data as sample from total 'n' rows of the data  
rrsampleNB2 = sample.split(rrNB2, SplitRatio = .90)
rrtrainsampleNB2 = subset(rrNB2, rrsampleNB2 == TRUE)
rrtestsampleNB2  = subset(rrNB2, rrsampleNB2 == FALSE)
colnames(rrtestsampleNB2) <- c("rented_for", "age","category","item_id")
colnames(rrtrainsampleNB2) <- c("rented_for", "age","category","item_id")


#remove "rented for" from testing data
rrtestsampleNB2_nolabel<-rrtestsampleNB2[-c(1)]
rrtestsampleNB2_justlabel<-rrtestsampleNB2_nolabel$rented_for
head(rrtestsampleNB2_nolabel)

#naive bayes model using e1701 with 99% sample wiht just age and rented for
NBtrainsample2 <- naiveBayes(rented_for~., data=rrtrainsampleNB2, na.action =na.pass)
NB_pred2 <- predict(NBtrainsample2, rrtestsampleNB2_nolabel)
table(NB_pred2, rrtestsampleNB2_justlabel)
NB_table2 <- table(NB_pred2, rrtestsampleNB2_justlabel)
NB_table2

#NB confusion matrix 
confusionMatrix(NB_table2, rrtestsampleNB2$rented_for, positive = NULL, dnn = c("Prediction", "Reference"), prevalence = NULL, mode = "prec_recall")




