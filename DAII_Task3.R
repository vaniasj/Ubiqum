# Ubiqum Module 2 Task 3
# Vania Sao Jose
# September 2019


# loading libraries
library(caret)
library(corrplot)
library(lattice)
library(ggplot2)
library(e1071)
library(dplyr)
library(openxlsx)

#loading data
setwd("/Users/vaniasaojose/Desktop/Ubiqum/DA/DAII/Task3")
ExistProd <- read.csv("existingproductattributes2017.csv")

# Preprocessing data
ExistProd$ProductType <- as.factor(ExistProd$ProductType)

names(ExistProd)[names(ExistProd) == "ProductType"] <- "prod"
names(ExistProd)[names(ExistProd) == "ProductNum"] <- "id"
names(ExistProd)[names(ExistProd) == "Price"] <- "price"
names(ExistProd)[names(ExistProd) == "x5StarReviews"] <- "x_5*"
names(ExistProd)[names(ExistProd) == "x4StarReviews"] <- "x_4*"
names(ExistProd)[names(ExistProd) == "x3StarReviews"] <- "x_3*"
names(ExistProd)[names(ExistProd) == "x2StarReviews"] <- "x_2*"
names(ExistProd)[names(ExistProd) == "x1StarReviews"] <- "x_1*"
names(ExistProd)[names(ExistProd) == "PositiveServiceReview"] <- "pReview"
names(ExistProd)[names(ExistProd) == "NegativeServiceReview"] <- "nReview"
names(ExistProd)[names(ExistProd) == "Recommendproduct"] <- "reco"
names(ExistProd)[names(ExistProd) == "BestSellersRank"] <- "rank"
names(ExistProd)[names(ExistProd) == "ShippingWeight"] <- "weight"
names(ExistProd)[names(ExistProd) == "ProductDepth"] <- "depth"
names(ExistProd)[names(ExistProd) == "ProductWidth"] <- "width"
names(ExistProd)[names(ExistProd) == "ProductHeight"] <- "height"
names(ExistProd)[names(ExistProd) == "ProfitMargin"] <- "margin"
names(ExistProd)[names(ExistProd) == "Volume"] <- "volume"


# dummify the data
ExistProd2 <- dummyVars(" ~ .", data = ExistProd)
ExistProd2 <- data.frame(predict(ExistProd2, newdata = ExistProd))

#checking data types
str(ExistProd2)
summary(ExistProd2)

#checking missing data
ExistProd2$rank <- NULL

#creating a correlation matrix
corrData <- cor(ExistProd2)
corrData
corrplot(corrData)

#creating a model
set.seed(123)
inTraining <- createDataPartition(ExistProd2$volume, p = .75, list = FALSE)
training <- ExistProd2[inTraining, ]
testing <- ExistProd2[-inTraining, ]


#modelling - RF1
fitControl1 <- trainControl(method = "repeatedcv", number = 10, repeats = 1) 
rfFit1 <- train(volume~., data = training, method = "rf", trControl=fitControl, tuneLength = 2)


# Checking the model RF1
rfFit1
testing$predictions <- predict(rfFit1, testing)


# ---- 

  #SVM Model  
  #Fit a model. svm using the caret package
svmFit <- train(volume ~ ., 
                data = training, 
                method = "svmLinear", 
                trControl = fitControl, 
                tuneLength = 2)

# checking the performance of your model
svmFit #performance on the training set
testing$svmPred <- predict(svmFit, newdata = testing) #predictions on testing set
postResample(testing$svmPred, testing$volume) #performance on testing set


# Loading data New products
setwd("/Users/vaniasaojose/Desktop/Ubiqum/DA/DAII/Task3")
NewProd <- read.csv("newproductattributes2017.csv")

# Preprocessing data NewProd
NewProd$ProductType <- as.factor(NewProd$ProductType)

names(NewProd)[names(NewProd) == "ProductType"] <- "prod"
names(NewProd)[names(NewProd) == "ProductNum"] <- "id"
names(NewProd)[names(NewProd) == "Price"] <- "price"
names(NewProd)[names(NewProd) == "x5StarReviews"] <- "x_5*"
names(NewProd)[names(NewProd) == "x4StarReviews"] <- "x_4*"
names(NewProd)[names(NewProd) == "x3StarReviews"] <- "x_3*"
names(NewProd)[names(NewProd) == "x2StarReviews"] <- "x_2*"
names(NewProd)[names(NewProd) == "x1StarReviews"] <- "x_1*"
names(NewProd)[names(NewProd) == "PositiveServiceReview"] <- "pReview"
names(NewProd)[names(NewProd) == "NegativeServiceReview"] <- "nReview"
names(NewProd)[names(NewProd) == "Recommendproduct"] <- "reco"
names(NewProd)[names(NewProd) == "BestSellersRank"] <- "rank"
names(NewProd)[names(NewProd) == "ShippingWeight"] <- "weight"
names(NewProd)[names(NewProd) == "ProductDepth"] <- "depth"
names(NewProd)[names(NewProd) == "ProductWidth"] <- "width"
names(NewProd)[names(NewProd) == "ProductHeight"] <- "height"
names(NewProd)[names(NewProd) == "ProfitMargin"] <- "margin"
names(NewProd)[names(NewProd) == "Volume"] <- "volume"

# dummify the data
NewProd2 <- dummyVars(" ~ .", data = NewProd)
NewProd2 <- data.frame(predict(NewProd2, newdata = NewProd))

#checking data types
str(NewProd2)
summary(NewProd2)

#checking missing data
NewProd2$rank <- NULL

# Applying the model to predict at the Volume
NewProd2$volume <- predict(svmFit, NewProd2)


#----

#creat a csv
wb <- createWorkbook()

addWorksheet(wb, "EU")

addWorksheet(wb, "NOTEU")

writeDataTable(wb, 1, NewProd2, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium18")

writeDataTable(wb, 2, NewProd2, startRow = 1, startCol = 1, tableStyle = "TableStyleMedium18")

saveWorkbook(wb, "Tables_with_Formatting_SVM.xlsx", overwrite = TRUE)


