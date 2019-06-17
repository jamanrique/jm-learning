# Library Load

library(tidyverse)
library(caret)
library(e1071)
library(corrplot)

# Environment cleaning and seeding setting
rm(list=ls())
set.seed(140)
setwd("C:/Users/jaman/Documents/GitHub/jm-learning/data-science-cyo/data")

## Data loading 
w_red <- read_delim("winequality-red.csv", ";", escape_double = FALSE, trim_ws = TRUE)

colnames(w_red) <- make.names(colnames(w_red))

## Creating partition
index = createDataPartition(w_red$quality,times=1,p=0.75, list = F)
w_red_train = w_red[index,]
w_red_test = w_red[-index,]

# Exploratory Data Analysis
glimpse(w_red_train)
summary(w_red_train)


# Summarizing information
w_red_train %>% group_by(quality) %>% summarise(count=n(),fsd = mean(free.sulfur.dioxide),tsd = mean(total.sulfur.dioxide),d=mean(density),ca=mean(citric.acid))

# Understanding relationships between variables

m = cor(w_red_train[,1:12])
corrplot(m,method="number",type="upper")

# Defining our RMSE function

RMSE <- function(true_ratings,predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}

# defining our cross-validation technique

cv = trainControl(method='repeatedcv',number = 4,repeats=2)

# CART Model

cartmodel = train(quality ~ citric.acid + volatile.acidity + alcohol + sulphates + density, tuneLength=4,trControl=cv, data=w_red_train,method='rpart')

cart_pr = predict(cartmodel,newdata = w_red_test)

RMSE(cart_pr, w_red_test$quality)

plot(cartmodel$finalModel)
text(cartmodel$finalModel)

# KNN

knn = train(quality~citric.acid + volatile.acidity + alcohol + sulphates + density, w_red_train,method='knn')
knn_pr = predict(knn,newdata=w_red_test)

RMSE(knn_pr, w_red_test$quality)

plot(knn)


z# Random Forest

rf = train(quality~ citric.acid + volatile.acidity + alcohol + sulphates + density, w_red_train,method='rf',tuneLength=4,trControl=cv)
ggplot(rf,highlight = TRUE)

rf_pr = predict(rf, newdata = w_red_test)

RMSE(rf_pr, w_red_test$quality)

## Understanding our final model

varImpPlot(rf$finalModel)
