setwd("/home/cris/Descargas")
data<-read.csv("my_dataset_train.csv")
datatst<-read.csv("my_dataset_test.csv")
dim(data)
dim(datatst)
is.na(data)
is.na(dim(datatst))

# analizar variables factores
## cuales son factores

sapply(data, is.factor)
sapply(datatst, is.factor)

## cuantos son factores

sum(sapply(data, is.factor))
perdidosX0<-length(which(data$x0==""))
perdidosX14<-length(which(data$x14==""))
perdidosx17<-length(which(data$x17==""))
perdidosx51<-length(which(data$x51==""))
perdidosx61<-length(which(data$x61==""))
perdidosx63<-length(which(data$x63==""))
perdidosX0
perdidosX14
perdidosx17
perdidosx51
perdidosx61
perdidosx63



# Análisis valores numéricos
## cuantos valores na y nan hay

sum(apply(data, 2, is.nan)) # no hay nan
sum(apply(data, 2, is.na)) # hay 245 na
sum(apply(datatst,2,is.nan)) # no hay nan en test
sum(apply(datatst,2,is.na)) # 66 nas en test

## cuales son las columnas con na's

apply(apply(data, 2,is.na), 2, sum)
apply(apply(datatst,2,is.na),2,sum)

## cuantos na's hay por cada columna
which(apply(apply(data, 2,is.na), 2, sum) != 0)
which(apply(apply(datatst, 2,is.na), 2, sum) != 0)

## que columnas tienen más nas

sort(which(apply(apply(data, 2,is.na), 2, sum) != 0),decreasing = T)
sort(which(apply(apply(datatst, 2,is.na), 2, sum) != 0),decreasing = T)

## cuantas columnas hay con nas
length(which(apply(apply(data, 2,is.na), 2, sum) != 0))
length(which(apply(apply(datatst, 2,is.na), 2, sum) != 0))

## Conclusiones 

# Análisis de dependencia:

require(dplyr)
(data.numeric <- data %>% na.omit() %>% dplyr::select_if(is.numeric))
cor(data.numeric)
apply(cor(data.numeric),2,function(i){
  which(i>0.8)
})

(data.numerictst <- datatst %>% na.omit() %>% dplyr::select_if(is.numeric))
cor(data.numerictst)
apply(cor(data.numerictst),2,function(i){
  which(i>0.8)
})


#proporción de datos /clase ates de imputar
table(data$y)

#  0   1   2   3 
#249 620 920 939 


#quitar columnas correladas e imputar valores perdidos de x48

#cual tiene más valores perdidos
which(is.na(data$x41)) #train
which(is.na(data$x48)) #train
which(is.na(datatst$x41)) #test
which(is.na(datatst$x48)) #test
# calculamos los valores de x48 que faltan a partir de x41
escalar<-43.38980659714715
data$x48[703]<-data$x41[703]/escalar
data$x48[995]<-data$x41[995]/escalar
data$x48[1928]<-data$x41[1928]/escalar
data$x48[2535]<-data$x41[2535]/escalar
#comprobamos que ya no hay NAs en x48
which(is.na(data$x48))
data <- data %>% dplyr::select(-x41)
datatst <- datatst %>% dplyr::select(-x41)


# aplicamos imputación por KNN
library("robCompositions")
require("mice")
completos<-mice::ncc(data)
incompletos<-mice::nic(data)
completos
incompletos


#data.numeric.with.nas<-data %>%  dplyr::select_if(is.numeric) %>% select(x69,x70,x72,x73)
data.numeric.with.nas<-data %>%  dplyr::select_if(is.numeric)#train
data.numeric.with.nas.tst<-datatst %>%  dplyr::select_if(is.numeric)
imputados=robCompositions::impKNNa(data.numeric.with.nas,metric = "Euclidean")#train
imputadostst=robCompositions::impKNNa(data.numeric.with.nas.tst,metric = "Euclidean")
imputados#train
imputadostst #test
plot(imputados,which=2)#train
plot(imputadostst,which=2)

df<-as.data.frame(imputados$xImp)#train
dataTrain<-cbind.data.frame(df,data %>% select_if(is.factor))
dftst<-as.data.frame(imputadostst$xImp)#test
dataTest<-cbind.data.frame(dftst,datatst %>% select_if(is.factor))#test


# imputación de factores
trainFactors<-dataTrain %>% dplyr::select_if(is.factor)
sum(dataTrain$x0=="")
sum(dataTrain$x14=="")
sum(dataTrain$x17=="")
sum(dataTrain$x51=="")
sum(dataTrain$x61=="")
sum(dataTrain$x63=="")

TrainClasses<-dataTrain$y

imputefactors<-function(col){
  missing<-which(col=="")
  for(i in missing){
    col[i]<-colnames(table(dataTrain$y,col))[which.max(table(dataTrain$y,col)[TrainClasses[i]+1,])]
  }
  col
}

dataTrain$x0<-imputefactors(dataTrain$x0)


## Atajando Desbalanceo

library(caret)
# creamos dataset downsampled y oversampled
classindex<-which(colnames(dataTrain)=="y")
dataDownsampled<-downSample(dataTrain[,-classindex], as.factor(dataTrain[, classindex])) # train
dataUpsampled<-upSample(dataTrain[,-classindex], as.factor(dataTrain[, classindex])) #train

# ajustamos modelo KNN1 para comparar 
TrainIndex <- createDataPartition(dataUpsampled$Class, p = .8,
                                  list = F,
                                  times = 1)
Train <- dataUpsampled[TrainIndex, ]
Test <- dataUpsampled[-TrainIndex, ]


fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)
# For example, in problems where there are a low percentage of samples
# in one class, using metric = "Kappa" can improve quality of the final model.
#knnModel <- train(Class ~ .,
 #                 data = Train,
#                  method = "knn",
#                  preProcess = c("center", "scale"),
#                  trControl = fitControl,
#                  tuneGrid=expand.grid(k=1),
#                  metric = "Kappa")
#knnModel
#knnPredict <- predict(knnModel, Test)
#(knnacc <- postResample(pred = knnPredict,
#             obs = Test$Class))

# SMOTE
#library(unbalanced)
#n<-ncol(df)
#output<-df$y
#input<-df[ ,-n]

#dataSmote<-ubSMOTE(X=input, Y= output)
#newData<-cbind(dataSmote$X, dataSmote$Y)


# con ripper
#TrainClasses<-Train$Class
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)
ripper.fit <- caret::train(Class ~ .,Train,
                  method = "JRip",
                  preProcess = c("center", "scale"),
                  trControl = fitControl,metric = "Kappa")
ripper.fit
ripperPredict <- predict(ripper.fit, Test)
(ripperacc <- postResample(pred = ripperPredict,
             obs = Test$Class))

#dataDownsampledTrain<-downSample(dataTrain[,-classindex], as.factor(dataTrain[, classindex])) # train
#dataUpsampledTrain<-upSample(dataTrain[,-classindex], as.factor(dataTrain[, classindex])) #train


TrainClasses<-dataUpsampled$Class
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)
ripper.fit <- caret::train(Class~.,dataUpsampled,
                  method = "JRip",
                  preProcess = c("center", "scale"),
                  trControl = fitControl,metric = "Kappa",
                  control=Weka_control(O = 3, F = 4,N = 2))
ripper.fit
ripperPredict <- predict(ripper.fit, dataTest)
#(ripperacc <- postResample(pred = ripperPredict,
 #            obs = Test$Class))


Id<-c(seq(1:dim(dataTest)[1]))
Prediction<-ripperPredict
kaggle<-data_frame(Id,Prediction)
write.csv(kaggle,file = "/home/cris/mrcrstnherediagmez@gmail.com/Preprocesamiento_Y_clasificacion/RipperResults.csv",row.names = F)
