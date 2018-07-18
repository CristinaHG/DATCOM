
require(ISLR)
names(Smarket)
summary(Smarket)

?Smarket

pairs(Smarket,col=Smarket$Direction)

cor(Smarket) # This won't work, why)

cor(Smarket[,-9]) # Note that Volume has some correlation with Year...

boxplot(Smarket$Volume~Smarket$Year)

# Direction is derive from Today
cor(as.numeric(Smarket$Direction),Smarket$Today)

glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

glm.probs <- predict(glm.fit,type="response") 
glm.probs

glm.pred <- ifelse(glm.probs>0.5,"Up","Down")
glm.pred

table(glm.pred,Smarket$Direction)
mean(glm.pred==Smarket$Direction)

# Make training and test set
train <- (Smarket$Year < 2005)
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.fit

glm.probs <- predict(glm.fit,newdata=Smarket[!train,], type="response") 
glm.pred <- ifelse(glm.probs >0.5,"Up","Down")
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred,Direction.2005) # Overfitting!
mean(glm.pred==Direction.2005)

glm.fit <- glm(Direction~Lag1+Lag2, data=Smarket,family=binomial, subset=train)
glm.fit

glm.probs <- predict(glm.fit,newdata=Smarket[!train,],type="response") 
glm.pred <- ifelse(glm.probs > 0.5,"Up","Down")
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)

require(caret)
glmFit <- train(Smarket[train,-9], y = Smarket[train,9], method = "glm", preProcess = c("center", "scale"),
                tuneLength = 10, control=glm.control(maxit=500), trControl = trainControl(method = "cv"))
glmFit


#Exercise 2

#Using the Smarket dataset perform 10 fold-cv with logistic regression.

require(ISLR)
require(caret)
train <- (Smarket$Year < 2005)
test<- Smarket$Direction[!train]
glmFit <- train(Smarket[train,-9], y = Smarket[train,9], method = "glm", preProcess = c("center", "scale"),
                tuneLength = 10, control=glm.control(maxit=10), trControl = trainControl(method = "cv"))
glmFit

# but we can see that even setting the max number of iterations to just 10, the model if overfitting, as it gets an accuracy of 
# 0.9949899 for train it such a difficult to predict data



