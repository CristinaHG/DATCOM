
library(MASS)
library(ISLR)

# First check LDA assumtions!

# The observations are a random sample: we will assume there are...
# Each predictor variable is normally distributed
shapiro.test(Smarket$Lag1)
shapiro.test(Smarket$Lag2)

qqnorm(y = Smarket$Lag1)
qqline(y = Smarket$Lag1)

# Predictors have a common variance
boxplot(Smarket[,2:3])

var(Smarket$Lag1)
var(Smarket$Lag2)

# Linear Discriminant Analysis
lda.fit <- lda(Direction~Lag1+Lag2,data=Smarket, subset=Year<2005)
lda.fit

plot(lda.fit, type="both")

Smarket.2005 <- subset(Smarket,Year==2005)
lda.pred <- predict(lda.fit,Smarket.2005)
class(lda.pred)
lda.pred

data.frame(lda.pred)

table(lda.pred$class,Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

library(klaR)
partimat(Direction~Lag1+Lag2, data=Smarket, method="lda")

# Check same variance but this time for each class
var(Smarket[Smarket$Direction == "Up",]$Lag1)
var(Smarket[Smarket$Direction == "Up",]$Lag2)
var(Smarket[Smarket$Direction == "Down",]$Lag1)
var(Smarket[Smarket$Direction == "Down",]$Lag2)

# QDA
qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
qda.fit

qda.pred <- predict(qda.fit,Smarket.2005)
class(qda.pred)
data.frame(qda.pred)

table(qda.pred$class,Smarket.2005$Direction)
mean(qda.pred$class==Smarket.2005$Direction)

partimat(Direction~Lag1+Lag2, data=Smarket ,method="qda")

data(iris)
iris.lda <- lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,  data = iris)
iris.lda

partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris, method="lda")

TrainData <- iris[,1:4]
TrainClasses <- iris[,5]
library(caret)
ldaFit <- train(TrainData, TrainClasses,
                method = "lda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
ldaFit
confusionMatrix(ldaFit)

qdaFit <- train(TrainData, TrainClasses,
                method = "qda",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"))
qdaFit
confusionMatrix(qdaFit)

#Exercise 1

#Try lda with all Lag variables.
#Make a quick comparison between logistic regression and lda.
#Try with qda and compare all three methods. Plot the results.

library(MASS)
library(ISLR)

# lda with all Lag variables for 2005
MarketLda.fit<-lda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket, subset=Year<2005)
MarketLda.fit

#evaluate 
Smarket.2005 <- subset(Smarket,Year==2005)
MarketLda.pred <- predict(MarketLda.fit,Smarket.2005)
class(MarketLda.pred)

table(MarketLda.pred$class,Smarket.2005$Direction)
mean(MarketLda.pred$class==Smarket.2005$Direction)
# quite better to qda model as its success median value was 0.55 but with lda it is 0.58

# Quick comparison between logistic regression and lda

# as we already have a lda model, we compute a rlg one
Marketglm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket.2005,family=binomial)
Marketglm.fit


Marketglm.probs <- predict(Marketglm.fit,type="response") 
Marketglm.probs

Marketglm.pred <- ifelse(Marketglm.probs>0.5,"Up","Down")
Marketglm.pred

class(Marketglm.pred)

table(Marketglm.pred,Smarket.2005$Direction)
mean(Marketglm.pred==Smarket.2005$Direction)

# with logistic regression the accuracy is 0.55952380952381 that is, random. with lda we got an accuracy of 0.58.. quite better 
# but not good enought to be a good prediction model

# Try with qda and compare all three methods. Plot the results.
MarketQda.fit<-qda(Direction~Lag1+Lag2+Lag3+Lag4+Lag5,data=Smarket, subset=Year<2005)
MarketQda.fit
MarketQda.pred <- predict(MarketQda.fit,Smarket.2005)
table(MarketQda.pred$class,Smarket.2005$Direction)
mean(MarketQda.pred$class==Smarket.2005$Direction)

# with qda we got an accuracy of 0.567460317460317 , so we can conclude that lda seems to be a better fit (0.58..) but not god enougth
# to be a predictible model, as 0.58 accuracy is very close to 0.50, that is, the same as flipping a coin randomly.

# plot the results
library(klaR)
#plot(MarketLda.fit)
plot(Marketglm.fit$model)
partimat(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket ,method="qda")
partimat(Direction~Lag1+Lag2+Lag3+Lag4+Lag5, data=Smarket ,method="lda")


#Exercise 2

#Using only the information in file clasif_train_alumnos.csv:
#Compare lda and qda using Wilcoxon.
#Perform a multiple comparison using Friedman.
#Using Holm see if there is a winning algorithm (even if Friedman says there is no chance…).

setwd("/home/cris/mrcrstnherediagmez@gmail.com/Intro_Ciencia_de_datos/EjerciciosClasificacion")
# we read th table with the mean errors for train
alumnos<-read.csv("clasif_train_alumnos.csv")
tabletrain <- cbind(alumnos[,2:dim(alumnos)[2]])
colnames(tabletrain) <- names(alumnos)[2:dim(alumnos)[2]]
rownames(tabletrain) <- alumnos[,1]

alumnos$out_train_lda

#Compare lda and qda using Wilcoxon.
difs <- (tabletrain[,2] - tabletrain[,3]) / tabletrain[,2]
wilc_2_3 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_2_3) <- c(colnames(tabletrain)[2], colnames(tabletrain)[3])
head(wilc_2_3)

# perform Wilcoxon test 
LDAvsQdatst <- wilcox.test(wilc_2_3[,1], wilc_2_3[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LDAvsQdatst$statistic
pvalue <- LDAvsQdatst$p.value
LDAvsQdatst <- wilcox.test(wilc_2_3[,2], wilc_2_3[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LDAvsQdatst$statistic
Rmas  
Rmenos 
pvalue 
# there's a (1-0.153646469116211)*100 = 84.635% confidence for models to be not equal. The difference is significant, so the're
# not from the same population

# Perform a multiple comparison using Friedman.
test_friedman <- friedman.test(as.matrix(tabletrain))
test_friedman$p.value # 0.522045776761016
# it seems like there no exist any significant difference between any of the algorithms

# Using Holm see if there is a winning algorithm (even if Friedman says there is no chance…).
tam <- dim(tabletrain)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tabletrain), groups, p.adjust = "holm", paired = TRUE)
# we can see that 2vs1 is 0.65 so there's a difference in favor of 2 (LDA). For 3vs1= 0.59 and 3vs2= 0.53 seems like 3 (QDA) 
# has not any advantage over LDA.




