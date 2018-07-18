# Heredia Gomez Mª Cristina
# mrcrstnherediagmez@gmail.com
# Ejercicio de trabajo autónomo. Series temporales. Curso 2017-2018


# borrar entorno
rm(list=ls())
morella.dat<-read.csv("Estacion_9562X_diaria_76668203A.txt",sep=";")
# dimensiones
dim(morella.dat)
# muestra de las primeras filas
head(morella.dat)
# eliminar ID
morella.dat<-morella.dat[,-1]
# hay un dato diario desde el 2016-09-07 hasta el 2018-02-28
# cuantos NAs hay en total
length(which(is.na(morella.dat)))
# cuantos Nas hay por columna
na_count <-sapply(morella.dat, function(y) sum(length(which(is.na(y)))))

# seleccionar desde Diciembre del 2017 hasta febrero de 2018
morella.DectoFeb<-morella.dat[448:nrow(morella.dat),2]


library(tseries)
serie<-ts(morella.DectoFeb,frequency=7) 

##Preprocesamiento
#comprobando normalidad para usar mtsdi para imputación

monthplot(serie.imputed)shapiro.test(serie[0:5000])
dim(serie)


#install.packages('nortest')
library(nortest)
ad.test(serie) # como p-valor << 0.05 en ambos casos, no es normal

#Imputación de valores perdidos con 
#install.packages("imputeTS")
library("imputeTS")
serie.imputed<-na.interpolation(serie)
#gráfico de la serie
autoplot(serie.imputed)
# análisis de tendencia y de estacionariedad

plot(decompose(serie.imputed))
library(forecast)
library(ggplot2)
#ggseasonplot(serie.imputed, year.labels=FALSE, continuous=TRUE)
ggsubseriesplot(serie.imputed)

# transformación logaritmica - NO porque tenemos temperaturas negativas
#serie.log<-log(serie.imputed)


#dividimos datos en train y test (total=537 datos) 
#nsemanas<-length(serie.imputed[,2])/7 #76.71429
#ntrain<-floor(0.85*nsemanas)
#train<-window(serie.imputed[,2],end=ntrain)
#test<-window(serie.imputed[,2],start=ntrain+1,end=length(serie.imputed[,2]))
#length(train)
#length(test)
#timeTrain<-time(train)
#timeTest<-time(test)

testDim<-13 # 13 para test
train<-serie.imputed[1:(length(serie.imputed)-testDim)]
timeTrain<-1:length(train)
test<-serie.imputed[(length(serie.imputed)-testDim+1):length(serie.imputed)]
timeTest<-(length(train)+1):length(serie.imputed)
length(train)
length(test)
length(timeTrain)
length(timeTest)
# mostrar serie de entrenamiento + tiempo de test + serie de test
plot.ts(train,xlim=c(1,timeTest[length(timeTest)]))
lines(timeTest,test,col="red")

# modelado de la tendencia - no tiene tendencia

#--Pruebas descartadas--
#train.ts<-ts(train,frequency=7)
#fit.lin <- tslm(train.ts ~ trend)
#fit.spline<-tslm(train.ts~timeTrain+ I(timeTrain^2) + I(timeTrain^3))
#fcasts.pw <- forecast(fit.spline, newdata = timeTest)
#autoplot(train.ts) +autolayer(fitted(fit.lin), series = "Linear")+
#autoplot(train.ts) +autolayer(fitted(fit.spline), series = "Cubic Spline")+
#autolayer(fcasts.pw, series="Piecewise")

#params.H1<- lm(train~ sin((2*pi/180)*timeTrain) + cos((2*pi/180)*timeTrain))
#params.H1<- lm(train~ I(timeTrain) + I(timeTrain)^3)
# estimación de la tendencia de train y test
#estimatedTrainTrend<- params.H1$coefficients[1] + timeTrain^3*params.H1$coefficients[2]
#estimatedTestTrend<- params.H1$coefficients[1] + timeTest^3*params.H1$coefficients[2]

# eliminar la estacionalidad
ggAcf(serie.imputed)
# se supuso una estacionalidad de 7
estacionality<-decompose(serie.imputed)$seasonal[1:7]
# se elimina la estacionalidad de los 7 primeros valores de la serie, 8 al 14,  del 15 al 20... etc
ntimes<-length(train)/length(estacionality) # nveces= long serie/long estacionalidad
aux <-rep(estacionality,floor(ntimes))
serieTrainWithoutEst<-train-aux
serieTestWithoutEst<-test-estacionality
#gráfico sin est
plot.ts(serieTrainWithoutEst,xlim=c(1,timeTest[length(timeTest)]))
lines(timeTest,serieTestWithoutEst,col="red")
# comprobamos estacionariedad antes
acf(serieTrainWithoutEst) 
adfTrain<-adf.test(serieTrainWithoutEst)
adfTest<-adf.test(serieTestWithoutEst)
# como no es estacionaria,diferenciamos una vez
train.Diff<-diff(train)
test.Diff<-diff(test)
# volvemos a hacer los test y los pasa, ya es estacionaria
adfTrain<-adf.test(train.Diff)
acf(train.Diff)

ggtsdisplay(train.Diff)
# test pacf pasa => estacionaria, podemos ajustar modelo
pacf(train.Diff)
# ajustamos modelo ARIMA, probados varios [(1,1,0),(2,1,0),(3,1,0),(4,1,0)]
fit <- Arima(serieTrainWithoutEst, order=c(3,1,0))
summary(fit)
checkresiduals(fit)
autoplot(forecast(fit,h=7))
forecast(fit,h=7)

# otra forma
model<-arima(serieTrainWithoutEst,order=c(3,1,0))
val<-serieTrainWithoutEst+model$residuals
predictions<-predict(model, n.ahead = 13)
predictedvalues<-predictions$pred
# podemos calcular el error del ajuste en train y test con el error cuadrático
errorTrain<-sum(model$residuals^2)
errorTest<-sum((predictedvalues-serieTestWithoutEst)^2)
errorTrain
errorTest
         
# valores reconstruidos de la serie con el modelo ajustado
val<-serieTrainWithoutEst+fit$residuals

autoplot(ts(serieTrainWithoutEst), series="Data") +
  autolayer(val, series="Reconstruidos") +
  autolayer(forecast(fit,h=7),series="predichos")+
  autolayer(ts(serieTestWithoutEst,start = 78),series = "test")+
  
  xlab("week") + ylab("") +
  ggtitle("Predicción diaria") +
  guides(colour=guide_legend(title=" "))


# hacemos una predicción 
predictions<-predict(fit, n.ahead = 13)
predictedvalues<-predictions$pred
# podemos calcular el error del ajuste en train y test con el error cuadrático
errorTrain<-sum(fit$residuals^2)
errorTest<-sum((predictedvalues-serieTestWithoutEst)^2)

#validar modelo
boxTest<-Box.test(fit$residuals)
JBTest<-jarque.bera.test(fit$residuals)
ShapkiroTest<-shapiro.test(fit$residuals)
hist(fit$residuals,col="blue", prob=T)
lines(density(fit$residuals),col="green")


#Una vez que hemos validado todo el modelo, volvemos a seguir los
#pasos iniciales, sin dividir la serie en ajuste y test, para hacer la
#predicción de las temperaturas de marzo
total.serie<-serie.imputed
time<-1:length(total.serie)
aux<-ts(total.serie,frequency = 7)
aux<-decompose(aux)$seasonal
estacionality<-as.numeric(aux[1:7])
aux<-rep(estacionality,length(total.serie)/length(estacionality))
total.serieWithoutEst<-total.serie-aux
model<-arima(total.serieWithoutEst,order=c(3,1,0))
adjustvals<-total.serieWithoutEst+model$residuals
Predictions<-predict(model,n.ahead = 7)
predictedValues<-Predictions$pred
# deshacemos cambios para calcular predicciones reales
adjustvals<-adjustvals+aux
predictedValues<-predictedValues+estacionality
timePred<-(time[length(time)]+(1:7))
#plot del gráfico final
autoplot(total.serie)+
  autolayer(adjustvals,series="adjust")+
  autolayer(predictedValues,series = "predicted")


