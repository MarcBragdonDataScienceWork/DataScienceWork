# install.packages("jsonlite")
# install.packages("tidyr")
# install.packages("caret")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("plotly")
# install.packages("RColorBrewer")
# install.packages("sqldf")
# install.packages("arules")
# install.packages("caTools")
# install.packages("vcd")
# install.packages("tm")
library(tm)
library(vcd)
library(arules)
library(RColorBrewer)
library(lubridate)
library(jsonlite)
library(tidyr)
library(plyr)
library(caret)
library(dplyr)
library(doMC)
library(plotly)
library(sqldf)
library(caTools)
library(vcd)
library(tidytext)
print("Registering Cores for DOMC Parallel Processing")
registerDoMC(cores=24)

##### INIT #########
# Set up working directory 
# Set data paths
init <- function(){
  print("Set Data Path and Data File Location")
  #setwd("~/GIT/SyracuseProjects/IST707_Project")
  setwd("~/Documents/SyracuseProjects/IST707_Project")
}
##### Data Loading ##########
# stream in json file 
# push to dataframe

print("Stream In Data from JSON File")
#stream in from json

pull.the.data <- function(rentTheRunwayPath){
  
  rentrunwaystream <- stream_in(file(rentTheRunwayPath))

}

#saves us from restreaming
#rentrunwaydf <- rentrunwaystream[sample(nrow(rentrunwaystream),100),]
################################################################################################################
clean.the.data <- function(input.data.frame){
  
  print("Convert Height to Inches")
  input.data.frame$height <- sapply(strsplit(as.character(input.data.frame$height),"'|\""),
                                function(height){12*as.numeric(height[1]) + as.numeric(height[2])})
  
  print("Clean up Pounds (Remove lbs)")
  #remove lbs from weight and make it numeric
  input.data.frame$weight <- as.numeric(lapply(input.data.frame$weight, gsub, pattern='lbs', replacement=''))
  input.data.frame <- subset(input.data.frame,input.data.frame$weight >= 75)
  
  print("Remove Obvious Outliers from Age and Size")
  #age is a string in the data, make it numeric
  input.data.frame$age <- as.numeric(input.data.frame$age)
  input.data.frame <- subset(input.data.frame,input.data.frame$age>15)
  input.data.frame <- subset(input.data.frame,input.data.frame$age < 90)
  #sizes go out to 58 we are going to cap the max size offered  22
  input.data.frame <- subset(input.data.frame,input.data.frame$size <= 22)
  
  print("Change Review Date to a Date")
  input.data.frame$review_date<-mdy(input.data.frame$review_date)
  
  print("Convert User ID, Item ID, Rating to Factors")
  input.data.frame$user_id <- as.factor(input.data.frame$user_id)
  input.data.frame$item_id <- as.factor(input.data.frame$item_id)
  input.data.frame$rating <- as.factor(input.data.frame$rating)
  

  print("Separate Band and Cup Size")
  input.data.frame$chestsize <-as.numeric(gsub("[^0-9.-]", "", input.data.frame$`bust size`))
  input.data.frame$cupsize <- gsub("[0-9.-]", "", input.data.frame$`bust size`)
  
  print("Collapsing Cocktail Party into Party because it was a Single Data point")
  input.data.frame$'rented for'[input.data.frame$'rented for' == "party: cocktail"] <- "party"
  input.data.frame$`rented for` <- as.character(input.data.frame$`rented for`)
  input.data.frame$`rented for` <- factor(input.data.frame$`rented for`)
  
  print("Remove the InComplete Cases")
  input.data.frame<-input.data.frame[complete.cases(input.data.frame), ]  
  
  return(input.data.frame)
}

##############################################################################################################
make.descriptive.visualizations <- function(input.data.frame){
  
  tempDF<-input.data.frame[, c("weight","size", "height", "age")]
  boxplot(tempDF$weight, xlab = "Weight", ylab = "Pounds", main= "Weight Boxplot")
  boxplot(tempDF$age, xlab = "age", ylab = "Years Old", main= "Age Boxplot")
  boxplot(tempDF$height, xlab = "Height", ylab = "Inches", main= "Height Boxplot")
  boxplot(tempDF$size, xlab = "Size", ylab = "Womens Sizes", main= "Size Boxplot")
  remove(tempDF)
  min(input.data.frame$age)
  max(input.data.frame$age)
  range(input.data.frame$age)
  range(input.data.frame$size)
  hist(input.data.frame$age)
  #TODO Find out why this doesn't work
  #hist(input.data.frame$`rented for`)
  barchart(input.data.frame$category)
  rating = input.data.frame$rating
  rating.freq = table(rating)
  colors = c("yellow","green", "blue", "hot pink", "cyan") 
  pie(rating.freq, col = colors, main = "Rating Totals")
  min(input.data.frame$size)
  max(input.data.frame$size)
  mean(input.data.frame$size)
  median(input.data.frame$size)
  
  #find the mean and median for age - already removed under 16 and above 88
  mean(input.data.frame$age)
  median(input.data.frame$age)
  #########
  
  testdf<-as.factor(input.data.frame$size)
  table(testdf)
  barplot(table(testdf), xlab = "Size", ylab = "Count", main = "Customer Size Breakdown", col=brewer.pal(8,'Pastel2'))
  remove(testdf)
  
  testdf<-as.factor(input.data.frame$'body type')
  table(testdf)
  barplot(table(testdf), xlab = "Size", ylab = "Count", main = "Customer Bodytype Breakdown", col=brewer.pal(8,'Pastel2'))
  remove(testdf)
  
  table(input.data.frame$fit)
  barplot(table(input.data.frame$fit), xlab = "Size", ylab = "Count", main = "Fit Breakdown", col=brewer.pal(8,'Pastel2'))
  barplot(table(input.data.frame$rating), xlab = "Rating", ylab = "Count", main = "Rating Breakdown", col=brewer.pal(8,'Pastel2'))
  barplot(table(input.data.frame$'rented for'), xlab = "Rented For", ylab = "Count", main = "Rented For Breakdown", col=brewer.pal(8,'Pastel2'))
  
  
}
#########################################################################################################
make.user.frequency.plot <- function(input.data.frame){
  
  #str(input.data.frame)
  count.data <-  input.data.frame 
  count.users <- count.data %>% count(user_id, sort = TRUE, name="count") %>% subset(count > 73)

  subset.top.renters <- subset(input.data.frame, input.data.frame$user_id %in% count.users$user_id)
  
  top.renter <- subset(subset.top.renters, subset.top.renters$user_id %in% c(691468))
  print("Number of Top Renters")
  print(nrow(subset.top.renters))
  
  user.freq.plot <- plot_ly(
    
    x=count.users$user_id,
    y=count.users$count,
    name="Top Users Count",
    type="bar"
    
  )
  user.freq.plot
}

##########################################################################################################
run.association.rules <- function(input.data.frame){
  
  transactionData <- ddply(input.data.frame,c("user_id","review_date"),
                           function(df1)paste(df1$item_id,
                                              collapse = ","))
  
  transactionData <- subset(transactionData,grepl(",",transactionData$V1))
  transactionData$user_id<-as.factor(transactionData$user_id)
  transactionData$user_id<-NULL
  transactionData$review_date<-NULL
  colnames(transactionData)<-c("items")
  
  tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')
  itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
  itemFrequencyPlot(tr,topN=10,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative Item Frequency Plot")
  
  arules.DF<-input.data.frame
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
  options(digits = 2)
  #sort rules so that we can view the most relevant
  rules <- sort(rules, by="confidence", decreasing=TRUE)
  inspect(rules)
  summary(rules)
  
}
########################################################################

run.mine.text <- function(input.data.frame){
  
  
  input.data.frame <- input.data.frame[sample(nrow(clean.data),2000, replace=FALSE),]
  
  review.td <- subset(input.data.frame,select = c("user_id", "rating","review_summary"))
  
  ratings.td <- subset(input.data.frame,select = "rating")
  
  corpus <- Corpus(VectorSource(review.td$review_summary))
  tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, 
                                         removeNumbers = TRUE,removestopwords=TRUE,tolower=TRUE))
  
  train_set <- as.matrix(tdm)
  distanceMatrix <- dist(tdm, method="euclidean")
  

  train_set <- data.frame(ratings.td, as.matrix(distanceMatrix))
  

  return(train_set)
  
}
########################################################################
run.decision.tree <- function(input.data.frame){
  
  df<-input.data.frame
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
  
  print(conf)
}
########################################################################
run.svm <- function(input.data.frame){
  
  
  svm.df <- svm.train.set
  
  print("Pull Fields we Need")
  #svm.df <- input.data.frame
  
  svm.df$rating <- as.factor(svm.df$rating)
    
  svm.sample = sample.split(svm.df, SplitRatio = .99)
  svm.train = subset(svm.df, svm.sample == TRUE)
  svm.test  = subset(svm.df, svm.sample == FALSE)
  
  train.x = subset(svm.train,select=-rating)
  train.y = as.factor(svm.train$rating)
  
  test.x = subset(svm.test,select=-rating)
  test.y = as.factor(svm.test$rating)

  print("Set Train Control")
  train.control <- trainControl(method="cv",
                                number = 10, allowParallel = T) 
  print("Train Model")
  svm.model <- train(train.x, train.y, method="svmLinear", trControl = train.control)
  
  print("Make Predictions with SVM Model")
  svm.predictions <- predict(svm.model, test.x)
  
  
  
  print("Summary of SVM Model")
  summary(svm.model)
  print("Table of SVM Predictions")
  svm.confusion.matrix <- confusionMatrix(svm.predictions,test.y)
  plot(svm.model)
  
  mosaic(svm.confusion.matrix$table)
  
  
  
}



#######################################################################
#init must be run first
init()
rentTheRunwayPath <- "data/renttherunway_final_data.json"
rentrunwaystream <- pull.the.data(rentTheRunwayPath)

print("Clean the Data")
clean.data <- clean.the.data(rentrunwaystream)
str(clean.data)
print("Make Descriptive Visualizations")
#### Comment about all the visualizations that should be made TODO
make.descriptive.visualizations(clean.data)
#Tells us the top users 
print("Make User Frequency Plot")
make.user.frequency.plot(clean.data)
#run associaton rule mining
print("Run Association Rules")
#run.association.rules(clean.data)
print("Run SVM")

run.decision.tree(clean.data)


svm.train.set <- run.mine.text(clean.data)
run.svm(svm.train.set)


