
# Local directory - use your own!!!!!!
setwd("/home/cris/mrcrstnherediagmez@gmail.com/Intro_Ciencia_de_datos/EjerciciosClasificacion")

# Load data
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
# Examine the structure of the wbcd data frame
str(wbcd)
wbcd

# Drop the id feature
wbcd <- wbcd[,-1]

# Table of diagnosis
table(wbcd$diagnosis)

# Recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
table(wbcd$diagnosis)
wbcd$diagnosis

# Table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# Summarize three numeric features
summary(wbcd[,c("radius_mean", "area_mean", "smoothness_mean")])

# Normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[,2:31], scale, center = TRUE, scale = TRUE))
# Confirm that normalization worked
summary(wbcd_n[,c("radius_mean", "area_mean", "smoothness_mean")])
boxplot(wbcd_n[,c("radius_mean", "area_mean", "smoothness_mean")])

plot(wbcd[,2:5])

plot(wbcd_n[,1:4], col=wbcd[,1])

cor(wbcd[,2:5])

cor(wbcd_n[,1:4])

# Create training and test data
shuffle_ds <- sample(dim(wbcd_n)[1])
eightypct <- (dim(wbcd_n)[1] * 80) %/% 100
wbcd_train <- wbcd_n[shuffle_ds[1:eightypct], ]
wbcd_test <- wbcd_n[shuffle_ds[(eightypct+1):dim(wbcd_n)[1]], ]

# Create labels for training and test data
wbcd_train_labels <- wbcd[shuffle_ds[1:eightypct], 1]
wbcd_test_labels <- wbcd[shuffle_ds[(eightypct+1):dim(wbcd_n)[1]], 1]

# Load the "class" library
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
wbcd_test_pred

# Evaluating model performance
table(wbcd_test_pred,wbcd_test_labels)

require(caret)
knnModel <- train(x = wbcd_train, y = wbcd_train_labels, method = "knn")
class(knnModel)
knnModel

knnModel <- train(wbcd_train, wbcd_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:15))
plot(knnModel)

knnPred <- predict(knnModel, newdata = wbcd[shuffle_ds[(eightypct+1):dim(wbcd_n)[1]], -1])
knnPred

postResample(pred = knnPred, obs = wbcd[shuffle_ds[(eightypct+1):dim(wbcd_n)[1]], 1])

#Exercise 1¶

#Try with different k choices and do a quick comparison. You can draw a plot to show the results.

require(caret)
knnModel <- train(wbcd_train, wbcd_train_labels, method="knn", metric="Accuracy", tuneGrid = data.frame(.k=1:25))
knnModel
plot(knnModel)

# evaluating model in Test
knnPred<-predict(knnModel,wbcd_test)
knnPred
table(knnPred,wbcd_test_labels)
