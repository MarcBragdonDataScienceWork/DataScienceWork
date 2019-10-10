# install.packages("jsonlite")
# install.packages("tidyr")
# install.packages("caret")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("plotly")

library(lubridate)
library(jsonlite)
library(tidyr)
library(plyr)
library(caret)
library(dplyr)
library(doMC)
library(plotly)
print("Registering Cores for DOMC Parallel Processing")
registerDoMC(cores=24)

##### INIT #########
# Set up working directory 
# Set data paths
print("Set Data Path and Data File Location")
setwd("~/GIT/SyracuseProjects/IST707_Project")

rentTheRunwayPath <- "data/renttherunway_final_data.json"

##### Data Loading ##########
# stream in json file 
# push to dataframe

print("Stream In Data from JSON File")
#stream in from json
rentrunwaystream <- stream_in(file(rentTheRunwayPath))

#saves us from restreaming
#rentrunwaydf <- rentrunwaystream[sample(nrow(rentrunwaystream),100),]

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
}

make.user.frequency.plot <- function(input.data.frame){
  
  str(input.data.frame)
  
  input.data.frame <- clean.data
    
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

init()
print("Clean the Data")
clean.data <- clean.the.data(rentrunwaystream)
str(clean.data)
print("Make Descriptive Visualizations")
#### Comment about all the visualizations that should be made TODO
make.descriptive.visualizations(clean.data)
#Tells us the top users 
make.user.frequency.plot(clean.data)


