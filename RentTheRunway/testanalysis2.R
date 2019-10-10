# install.packages("jsonlite")
# install.packages("tidytext")
# install.packages("tidyr")
# install.packages("dplyr")
#  install.packages("kernlab",
#                   repos = "http://cran.r-project.org")
# install.packages("ggplot2",
#                  repos = "http://cran.r-project.org")

# install.packages("dplyr",
#                   repos = "http://cran.r-project.org")

#  install.packages("vcd",
#                   repos = "http://cran.r-project.org")
# install.packages("doMC")

# install.packages("randomforest")

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
library(kernlab)

#print("Registering Cores for DOMC Parallel Processing")
registerDoMC(cores=24)
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

print("Unnesting Tokens")
words <- review_tibble %>% unnest_tokens(word, review_summary) %>% anti_join(stop_words)

countwords <- words %>% count(word, sort = TRUE)
head(countwords,10)


svm.model.data <- review_tibble %>% unnest_tokens(word, review_summary) %>% anti_join(stop_words)

svm.model.data<-svm.model.data[-grep("\\b\\d+\\b", svm.model.data$word),]
svm.model.data$word <- gsub("\\s+","",svm.model.data$word)



svm.model.data <- as.data.frame(svm.model.data)

svm.model.data$word <- as.factor(svm.model.data$word)
svm.model.data$rating <- as.factor(svm.model.data$rating)
#svm.model.data$user_id <- as.factor(svm.model.data$user_id)

###################### CARET SVM #######################
print("Training SVM in Caret")

sample.train.data <- svm.model.data[sample(nrow(svm.model.data),200, replace=FALSE),]
sample.test.data <- svm.model.data[sample(nrow(svm.model.data),500, replace=FALSE),]

str(sample.train.data)

x <- subset(sample.train.data, select = -rating)
y <- as.factor(sample.train.data$rating)


#Train control for SVM Model
train.control <- trainControl(method="cv",
                              number = 3) 
#train.control <- trainControl(method="cv",
#                              number = 10, allowParallel = T) 

svm.model <- train(x, y, method="lssvmPoly")
#svm.model <- train(x, y, method="svmRadial", trControl = train.control)



print("Make Predictions with SVM Model")
svm.predictions <- predict(svm.model, test.x)

print("Summary of SVM Model")
summary(svm.model)
print("Table of SVM Predictions")
svm.confusion.matrix <- confusionMatrix(svm.predictions,test.y)
plot(svm.model)

mosaic(svm.confusion.matrix$table)
