library(dslabs)
library(caret)
library(tidyverse)

rm(list=ls())

setwd("C:/Users/Justo.Manrique/Documents/Capstone-HarvardX")
edx <- readRDS("edx.rds")
validation <- readRDS("validation.rds")

## Understanding our data

head(edx)
summary(edx)

hist(edx$rating)

## User analysis

usr_summary <- edx %>% group_by(userId) %>% summarise(rating = mean(rating),ratingscount=n())
summary(usr_summary)
hist(usr_summary$rating)

## Movie analysis

mov_summary <- edx %>% group_by(movieId, title) %>% summarise(n_ratings = n(),avg_ratings = mean(rating), index = mean(rating)/n(), min = min(rating), max = max(rating), median = mode(median))

mov_summary <- mov_summary %>% arrange(avg_ratings,n_ratings)
hist(mov_summary$index[mov_summary$index>1])

#### Modelling ####

## RMSE function
RMSE <- function(true_ratings,predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}

## Movie effects

mean <- mean(edx$rating)
mov_effect <- edx %>% group_by(movieId) %>% summarise(b_i = mean(rating-mean))
mov_effect <- cbind(mov_effect,mean) %>% mutate(prd = b_i + mean) %>% select(-b_i,-mean)
validation <-  validation %>%  left_join(mov_effect,by='movieId')

# RMSE
mov_rmse <- RMSE(validation$rating,validation$prd)
rmse_table <- data.frame(method = 'Movie effect', RMSE=mov_rmse)

## User and movie effects

usr_effect <- edx %>% group_by(userId) %>% summarise(u_i = mean(rating-mean))
validation <- validation %>% left_join(usr_effect,by='userId') 
validation <- validation %>% mutate(prd_2 = prd + u_i) %>% select(-u_i)

# RMSE
movusr_rmse <- RMSE(validation$rating,validation$prd_2)
rmse_table <- bind_rows(rmse_table,data.frame(method = 'Movie and user effect', RMSE=movusr_rmse))
rmse_table

## Start regularization

lambda <- seq(0,10,0.10)
rmses_u_i <- sapply(lambda,function(l){
  mean_f <- mean(edx$rating)
  b_i <- edx %>% group_by(movieId) %>% summarise(b_i=sum(rating-mean_f)/(n()+l))
  b_u <- edx %>% left_join(b_i, by='movieId') %>% group_by(userId) %>% summarise(b_u=sum(rating- b_i - mean_f)/(n()+l))
  
  prd_r <- validation %>% left_join(b_i, by='movieId') %>% left_join(b_u,by='userId') %>% mutate(prd_r=mean_f+b_i+b_u)
  
  return(RMSE(prd_r$prd_r,prd_r$rating))
}) 

lambda[which.min(rmses_u_i)]

## User and movie effects, regularized
reg_l <- lambda[which.min(rmses_u_i)]

mov_reg_effect = edx %>% group_by(movieId) %>% summarise(b_i = sum(rating-mean)/(n()+reg_l))
mov_usr_reg_effect = edx %>% left_join(mov_reg_effect, by='movieId') %>% group_by(userId) %>% summarise(b_u = sum(rating-b_i-mean)/(n()+reg_l))

## User and movie effect, regularized prediction
validation <- validation %>% left_join(mov_reg_effect,by='movieId') %>% left_join(mov_usr_reg_effect,by='userId')
validation <- validation %>% mutate(prd_reg = mean + b_i + b_u) 
validation <- validation %>% select(-b_i,-b_u)

RMSE(validation$prd_reg,validation$rating)

mov_usr_reg_rmse <- RMSE(validation$rating,validation$prd_reg)
rmse_table <- bind_rows(rmse_table,data.frame(method = 'Movie effect and user effect, regularized', RMSE=mov_usr_reg_rmse))
rmse_table
##
