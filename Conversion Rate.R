## Loading the data
data <- read.csv("conversion_data.csv", header = T)

## Viewing the first six rows of the data
head(data)

## Having a look at the structure
str(data)

## Looking at the summary of data
summary(data)

#### Assessing data quality

## Sorting the age
sort(unique(data$age), decreasing = T)

## Checking the number of rows with age greater than 79
subset(data, age>79)

## Removing the outliers 
data <- subset(data, age<80)

## Loading the required packages
library(dplyr)
library(magrittr)

## Looking at the conversion rate by country
data_country <- data %>% 
  group_by(country) %>% 
  summarise(conversion_rate = mean(converted))

## Loading the required library
library(ggplot2)

## Visualizing conversion rate by country
ggplot(data = data_country, aes(x = country, y = conversion_rate)) + 
  geom_bar(stat = "identity", aes(fill = country))

## Looking at the conversion rate by the number of pages visited
data_pages <- data %>%
  group_by(total_pages_visited) %>%
  summarise(conversion_rate = mean(converted))

## Visualizing conversion rate by number of pages visited
qplot(total_pages_visited, conversion_rate, data = data_pages, geom = "line")

## Converting the variable converted to categorical variable
data$converted = as.factor(data$converted)

## Converting the new_user variable to categorical variable
data$new_user = as.factor(data$new_user)

## Renaming Germany to DE 
levels(data$country)[levels(data$country)=="Germany"]="DE"

## Taking 66% data as training data
train_sample = sample(nrow(data), size = nrow(data)*0.66)

## Training data
train_data = data[train_sample,]

## Test data
test_data = data[-train_sample,]

## Loading the library
library(randomForest)

## Random Forest model
rf = randomForest(y = train_data$converted, x = train_data[, -ncol(train_data)],
                  ytest = test_data$converted, xtest = test_data[, -ncol(test_data)],
                  ntree = 100, mtry = 3, keep.forest = TRUE)

## Looking at the model
rf

## Variable importance plot
varImpPlot(rf,type=2)

## Random Forest model after removing total_pages_visited
rf = randomForest(y=train_data$converted, x = train_data[, -c(5, ncol(train_data))],
                  ytest = test_data$converted, xtest = test_data[, -c(5, ncol(train_data))],
                  ntree = 100, mtry = 3, keep.forest = TRUE, classwt = c(0.7,0.3))

## Looking at the model
rf

## Variable importance plot
varImpPlot(rf,type=2)

## Partial dependence plots
partialPlot(rf, train_data, country, 1)
partialPlot(rf, train_data, age, 1)
partialPlot(rf, train_data, source, 1)
partialPlot(rf, train_data, new_user, 1)

## Loading the library rpart
library(rpart)

## Decision tree
tree = rpart(data$converted ~ ., data[, -c(5,ncol(data))],
             control = rpart.control(maxdepth = 3),
             parms = list(prior = c(0.7, 0.3))
             )

## Looking at the tree
tree
