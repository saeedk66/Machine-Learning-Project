setwd("C:/Users/S.kh/Desktop/Coursera/Practical machine learning")
training <- read.csv("./pml-training.csv", na.strings = c("","#DIV/0!","NA"))
testing <- read.csv("./pml-testing.csv")

############ cleaning
training <- training[,colMeans(is.na(training)) <= .75]
testing <- testing[,colMeans(is.na(testing)) <= .75]
str(training)
training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]
library(caret)
library(ggplot2)
set.seed(2020)
intrain <- createDataPartition(y= training$classe, p = 0.7 , list = F)
trainingset <- training[intrain,]
testingset <- training[-intrain,]

######## Maching learning algorithms

fit1 <- train(classe~. , data = trainingset, method = "rpart")
library(rattle)
fancyRpartPlot(fit1$finalModel)

fit2 <- train(classe~., data = trainingset, method = "rf", ntree = 200, trControl = trainControl(method = "cv",5))
