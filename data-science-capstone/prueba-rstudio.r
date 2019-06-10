## carga de librerías ##
library(caret)
library(dslabs)
library(tidyverse)
library(HistData)
library(MASS)

set.seed(-32326)

## carga de datos ##
galton_heights <- GaltonFamilies %>% filter(childNum==1 & gender=="male") %>% select(father, childHeight) %>% rename(son=childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times=1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
mean((avg - test_set$son)^2) #RMSE

fit <- lm(son~father,data=train_set)
fit$coef

y_hat <- fit$coefficients[1] + fit$coefficients[2]*test_set$father ## primera forma de predecir
mean((y_hat - test_set$son)^2)

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

#primera pregunta
rm(list = ls())

set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
n <- 100
set.seed(1)
rep <- replicate(n, {
    y <- dat$y
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  train_dat <- dat %>% slice(-test_index)
  test_dat <- dat %>% slice(test_index)
  fit <- lm(y~x, data=train_dat)
  y_hat <- predict(fit,test_dat)
  sqrt(mean((y_hat-test_dat$y)^2))
  })

# respuesta primera pregunta
mean(rep)
sd(rep)

# segunda pregunta
rm(list=ls())
n <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
for (i in 1:length(n)) {
    functionRmses <- function(x){
    Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
    dat <- MASS::mvrnorm(n = n[i], c(69, 69), Sigma) %>%
      data.frame() %>% setNames(c("x", "y"))
    
    rep <- replicate(100, {
      y <- dat$y
      test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
      train_dat <- dat %>% slice(-test_index)
      test_dat <- dat %>% slice(test_index)
      fit <- lm(y~x, data=train_dat)
      y_hat <- predict(fit,test_dat)
      sqrt(mean((y_hat-test_dat$y)^2))
    })
    }
  nombre <- paste("rep",i,sep="")
  assign(nombre,sapply(n[i],functionRmses))
}

 
mean(rep1)
sd(rep1)
mean(rep2)
sd(rep2)
mean(rep3)
sd(rep3)
mean(rep4)
sd(rep4)
mean(rep5)
sd(rep5)

## trabajando con matrices
library(dslabs)
mnist <- read_mnist()
mean(mnist$train$images >= 50 & mnist$train$images <= 205)
rm(list=ls())
library(caret)
library(tidyverse)
data("heights")

ks <- seq(1, 101, 3)
set.seed(1)
Accuracy <- map_df(ks, function(k)
{
  y <- heights$sex
  index <- createDataPartition(y,times = 1,p = 0.5,list = F)
  knn_train <- heights[index,]
  knn_test <- heights[-index,]
  knn_fit <- knn3(sex~.,data=heights)
  y_hat <- predict(object = knn_fit,newdata = knn_test,type = "class")
  F_1 <- F_meas(data = y_hat, reference = factor(knn_test$sex))
  list(k=k, F_1 =F_1)
})

Accuracy %>% slice(which.max(F_1))

data("tissue_gene_expression")

library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

tt <- colttests(x, y)
sum(tt$p.value<0.01)

x_subset <- x[,which(tt$p.value<0.01)]

fit_sig <- train(x_subset, y, method = "glm")
ggplot(fit)

library(dslabs)
library(caret)
data("tissue_gene_expression")
data("mnist_27")
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

sum(indexes$Resample01==3) +sum(indexes$Resample02==3) + sum(indexes$Resample03==3) + sum(indexes$Resample04==3) + sum(indexes$Resample05==3) + sum(indexes$Resample06==3) + sum(indexes$Resample07==3) +  sum(indexes$Resample08==3) + sum(indexes$Resample09==3) + sum(indexes$Resample10==3)

set.seed(1)
y <- rnorm(100, 0, 1)
qnorm(0.75)

quantile(y,0.75)

B <- 10000
set.seed(1)
Q_stars <- replicate(B, {
  y_star <- sample(y,replace=T)
  Q_star <- quantile(y_star, 0.75)
})
mean(Q_stars)
sd(Q_stars)

rm(list=ls())
set.seed(1993)
data("tissue_gene_expression")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_x <- train(x,y,method="lda",data = x)
train_qx <- train(x,y,method="qda",data = x)
train_qx_2 <- train(x,y,method="qda",data = x,preProcessing="center")

rm(list=ls())
set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
train_x <- train(x,y,method="lda",data = x)
confusionMatrix(train_x)

rm(list=ls())
library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
library(tidyverse)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
    
library(dslabs)
library(rpart)    
library(tidyverse)    
library(caret)
data("tissue_gene_expression")    
set.seed(1991)
class_tree <- train(tissue_gene_expression$x, tissue_gene_expression$y,
                    method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))


set.seed(1991)
class_tree <- train(tissue_gene_expression$x, tissue_gene_expression$y,
                    method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),control = rpart.control(minsplit = 0))
confusionMatrix(class_tree)
set.seed(1991)
fit_rpart <- train(tissue_gene_expression$x,tissue_gene_expression$y,method = "rf",nodesize=1,tuneGrid = data.frame(mtry=seq(50,200,25)))
varImp(fit_rpart)

## pregunta mnist

rm(list=ls())
data <- read_mnist()
models <- c("glm", "lda",  "naive_bayes",  "svmLinear", 
            "gamboost",  "gamLoess", "qda", 
            "knn", "kknn", "loclda", "gam",
            "rf", "ranger",  "wsrf", "Rborist", 
            "avNNet", "mlp", "monmlp",
            "adaboost", "gbm",
            "svmRadial", "svmRadialCost", "svmRadialSigma")

library(caret)
library(dslabs)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

m <- predict(object = fits,newdata = mnist_27$test)

length(m)
a <- list ()
b <- c()
for (i in 1:length(m))
{
a[[i]] <-confusionMatrix(m[[i]],reference = mnist_27$test$y)
b[i] <- a[[i]]$overall[1]
}

library(dslabs)
library(tidyverse)
data("movielens")
plot(movielens$year,count(rating))
