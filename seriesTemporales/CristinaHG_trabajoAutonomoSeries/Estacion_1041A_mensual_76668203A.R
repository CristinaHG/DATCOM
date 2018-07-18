# Heredia Gomez Mª Cristina
# mrcrstnherediagmez@gmail.com
# Ejercicio de trabajo autónomo. Series temporales. Curso 2017-2018


# borrar entorno
rm(list=ls())
zumaia.dat<-read.csv("Estacion_1041A_mensual_76668203A.txt",sep=";")
# dimensiones
dim(zumaia.dat)
# muestra de las primeras filas
head(zumaia.dat)
# eliminar ID
zumaia.dat<-zumaia.dat[,-1]
# hay un dato diario desde el 2013-05-07 hasta el 2018-02-28
# cuantos NAs hay en total
length(which(is.na(zumaia.dat$Tmax)))

# seleccionar solo Fecha y Tmax
zumaia.dat<-zumaia.dat[,1:2]


library("imputeTS")
serie.imputed<-na.interpolation(zumaia.dat)
library(ggplot2)


library(xts)
serie.imputed.xts <- xts(serie.imputed[,2],
                 order.by = as.Date(serie.imputed[,1]),
                 origin="2013-05-07")
#agregamos por meses
serie.monthly<-apply.monthly(serie.imputed.xts,
              FUN = mean)

autoplot(serie.monthly)
#pasamos a TS con frecuencua mensual
(serie.monthly.ts<-ts(serie.monthly,frequency = 12,start = c(2013,5),end = c(2018,2)))
serie.monthly.ts<-window(serie.monthly.ts,start=c(2014,1))
plot(decompose(serie.monthly.ts))
length(serie.monthly)
# comprobando seasonality => hay una componente no estacionaria
ggsubseriesplot(serie.monthly.ts)


#dividimos datos en train y test (total=58 datos) 
ntrain<-c(2017,12)
train<-window(serie.monthly.ts,end=ntrain) #descartamos datos de 2013 dado que
# no están completos
test<-window(serie.monthly.ts,start=c(2018,1))
length(train)
length(test)
timeTrain<-time(train)
timeTest<-time(test)
timeTrain
timeTest

# modelado de la tendencia - no tiene tendencia
# modelado de la estacionalidad-eliminar la estacionalidad
ggAcf(serie.monthly.ts) # el patrón y los picos sobrepasando las líneas azules denotan estacionalidad

#observamos patrón estacionario de más frio en meses de invierno y picos de calor en agosto
# también identificamos excepciones como febrero de 2017 que ha sido excepcionalmente cálido
ggseasonplot(serie.monthly.ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("year") +
  ggtitle("Tmax mean by month by year")

# se supuso una estacionalidad de 12
estacionality<-decompose(serie.monthly.ts)$seasonal[1:12]
# se elimina la estacionalidad de los 12 primeros valores de la serie
ntimes<-length(train)/length(estacionality) # nveces= long serie/long estacionalidad
aux <-rep(estacionality,ntimes)
aux
serieTrainWithoutEst<-train-aux
serieTestWithoutEst<-test-estacionality[1:2]
# comprobamos estacionariedad antes
ggAcf(serieTrainWithoutEst) 
adfTrain<-adf.test(serieTrainWithoutEst)
ggPacf(serieTrainWithoutEst)

#como parece que ya hemos eliminado la estacionalidad, ajustamos modelo ARIMA
fit <- Arima(serieTrainWithoutEst, order=c(1,0,0))
summary(fit)
checkresiduals(fit)
autoplot(forecast(fit,h=2))
predicted<-forecast(fit,h=2)


val<-serieTrainWithoutEst+fit$residuals

# podemos calcular el error del ajuste en train y test con el error cuadrático
errorTrain<-sum(fit$residuals^2)
errorTest<-sum((predicted$mean-serieTestWithoutEst)^2)
errorTrain
errorTest
      

autoplot(serieTrainWithoutEst, series="Data") +
  autolayer(val, series="Reconstruidos") +
  autolayer(serieTestWithoutEst,series = "test")+
  autolayer(forecast(fit,h=2),series="predichos")+
  xlab("Year") + ylab("") +
  ggtitle("Predicción mensual") +
  guides(colour=guide_legend(title=" "))

#validar modelo
boxTest<-Box.test(fit$residuals)
JBTest<-jarque.bera.test(fit$residuals)
ShapkiroTest<-shapiro.test(fit$residuals)
hist(fit$residuals,col="blue", prob=T)
lines(density(fit$residuals),col="green")

#Una vez que hemos validado todo el modelo, volvemos a seguir los
#pasos iniciales, sin dividir la serie en ajuste y test, para hacer la
#predicción de las temperaturas medias de marzo y abril de 2018

zumaia.dat<-read.csv("./datos/DatosEstaciones - 2018-02/1041A.csv",sep=";")
# eliminar ID
zumaia.dat<-zumaia.dat[,-1]
# seleccionar solo Fecha y Tmax
zumaia.dat<-zumaia.dat[,1:2]
# imputación
serie.imputed<-na.interpolation(zumaia.dat)
#agregación por meses de los años
serie.imputed.xts <- xts(serie.imputed[,2],
                         order.by = as.Date(serie.imputed[,1]),
                         origin="2013-05-07")
#agregamos por meses
serie.monthly<-apply.monthly(serie.imputed.xts,FUN = mean)

autoplot(serie.monthly)
#pasamos a TS con frecuencua mensual
(serie.monthly.ts<-ts(serie.monthly,frequency = 12,start = c(2013,5),end = c(2018,2)))
serie.monthly.ts<-window(serie.monthly.ts,start=c(2014,1))
plot(decompose(serie.monthly.ts))
length(serie.monthly)
# comprobando seasonality => hay una componente no estacionaria
ggsubseriesplot(serie.monthly.ts)
# se supuso una estacionalidad de 12
estacionality<-decompose(serie.monthly.ts)$seasonal[1:12]
# se elimina la estacionalidad de los 12 primeros valores de la serie
ntimes<-length(serie.monthly.ts)/length(estacionality) # nveces= long serie/long estacionalidad
aux <-rep(estacionality,ntimes)
aux
serie.monthly.ts.WithoutEst<-serie.monthly.ts-aux
#como parece que ya hemos eliminado la estacionalidad, ajustamos modelo ARIMA
fit <- Arima(serie.monthly.ts.WithoutEst, order=c(1,0,0))
predicted<-forecast(fit,h=2)
val<-serie.monthly.ts.WithoutEst+fit$residuals

# deshacemos cambios para calcular predicciones reales
adjustvals<-val+aux
predictedValues<-predicted$mean+estacionality[1:2]
time<-1:length(serie.monthly.ts)
timePred<-(time[length(time)]+(1:12))


