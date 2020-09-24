# Credit-Scoring-Model
This repository contain some statistical method for determine performing loan.
#LDA
library(readxl)
data <- read_excel("C:/Users/Administrator/Downloads/Data Bank.xlsx") #importdata
View(data)
summary(data)
data = as.data.frame(data)
n      = dim(data)[1]
Y = as.matrix(data[,6])
#discriminantanalysis
library(lda)
library(MASS)
library(caret)
model=lda( data[,6] ~ ., data[,1:5])
model
fit <- lda(data[,6] ~ ., data[,1:5], CV=TRUE)
compare = confusionMatrix(table(data[,6],fit$class))
compare
#CART
library(rpart)
library(rpart.plot)
model1 = rpart(data[,6] ~ ., data[,1:5], method = "class")
# Plot the trees
rpart.plot(model1)
fit = predict(model1, data[,1:5], type = 'class')
compare2 = confusionMatrix(table(data[,6],fit))
compare2
