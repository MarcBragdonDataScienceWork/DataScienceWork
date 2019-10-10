# install.packages("jsonlite")
# install.packages("tidytext")
# install.packages("tidyr")
# install.packages("dplyr")
#  install.packages("kernlab",
#                   repos = "http://cran.r-project.org")
# install.packages("ggplot2",
#                  repos = "http://cran.r-project.org")
# 
# install.packages("dplyr",
#                   repos = "http://cran.r-project.org")
# 
#  install.packages("vcd",
#                   repos = "http://cran.r-project.org")
#install.packages("wordcloud", repos ="http://cran-r.projects.org")
library(vcd)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(doMC)
library(randomForest)
library(parallelSVM)
library(caret)
library(tidyr)
library(tidytext)
install.packages("wordcloud")
library(wordcloud)


print("Registering Cores for DOMC Parallel Processing")
registerDoMC(cores=4)
##### INIT #########
# Set up working directory 
# Set data paths
print("Setting Data and Directories")
setwd("~/GIT/SyracuseProjects/IST707_Project")
rentTheRunwayPath <- "data/renttherunway_final_data.json"


print("Loading the Data Set from the JSON")
rentrunwaystream <- stream_in(file(rentTheRunwayPath))


print("Pulling only Rating and Review")
rentrunwayreview <- subset(rentrunwaystream,select = c("rating","review_summary"))

print("Change everything to Lower Case")
rentrunwayreview$review_summary <- sapply(rentrunwayreview$review_summary, tolower)

print("Pulling in and Removing Stop Words")
data("stop_words")



review_tibble <- as_tibble(rentrunwayreview) 

head(review_tibble)


print("Unnesting Tokens")
words <- review_tibble %>% unnest_tokens(word, review_summary) %>% anti_join(stop_words)

countwords <- words %>% count(word, sort = TRUE)
head(countwords,10)

countwords %>% with(wordcloud(word,n,random.order=FALSE))



svm.model.data <- review_tibble %>% unnest_tokens(word, review_summary) %>% anti_join(stop_words)
svm.model.data<-svm.model.data[-grep("\\b\\d+\\b", svm.model.data$word),]
svm.model.data$word <- gsub("\\s+","",svm.model.data$word)
#svm.model.data <- as.data.frame(svm.model.data)

svm.model.data$word <- as.factor(svm.model.data$word)

output <- unique(svm.model.data$word)

write.csv(output, "words.csv")

svm.model.data$rating <- as.factor(svm.model.data$rating)
#svm.model.data$user_id <- as.factor(svm.model.data$user_id)
svm.model.data$rating
str(svm.model.data)

sample.train.data <- svm.model.data[sample(nrow(svm.model.data),200, replace=FALSE),]
sample.test.data <- svm.model.data[sample(nrow(svm.model.data),500, replace=FALSE),]

str(sample.train.data)

# library(h2o)
# 
# h2o.init()
# h2o.train.data <- as.h2o(sample.train.data)
# 
# 
x <- subset(sample.train.data, select = -rating)
y <- sample.train.data$rating
# 
# h2o.randomForest(x,y,training_frame = h2o.train.data,nfolds=10,ntrees=10)
# 
# summary(h2o.train.data)

train.control <- trainControl(method="cv",
                              number = 10, allowParallel = TRUE)

#set up model selection method
mtry <- sqrt(ncol(x))
#grid.rf <- expand.grid(.mtry = c(2, 4, 8, 16))
grid.rf <- expand.grid(.mtry = mtry)


print("Training Model Using Random Forest")
rf.model <- train(x,y, trControl=train.control,tuneGrid=grid.rf)

print("Making Prediction with RF Model")
rf.predictions <- predict(rf.model, test.x)


hist(count(test.x$label))

print("Summary of RF Model")
print(rf.model)
summary(rf.model)

print("Table of RF Predictions")
table(rf.predictions,test.y)

rf.value.table <- merge(rf.predictions,test.y)

############################### Random Forest ###################################

#set up train control with ten fold cross validation repeated 3 times
train.control <- trainControl(method="cv",
                              number = 10,allowParallel = T) 
#set up model selection method



print("Training Model Using Random Forest")
rf.model <- train(x,y, trControl=train.control)

print("Making Prediction with RF Model")
rf.predictions <- predict(rf.model, test.x)

hist(count(test.x$label))

print("Summary of RF Model")
print(rf.model)
summary(rf.model)

rf.model$
  
  print("Table of RF Predictions")
table(rf.predictions,test.y)

rf.confusion.matrix <- confusionMatrix(rf.predictions,test.y)

mosaic(rf.confusion.matrix$table)
