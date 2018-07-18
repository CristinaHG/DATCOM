########################################################
# Mª Cristina Heredia Gómez - 76668203A
# mrcrstnherediagmez@gmail.com
# Ejercicio guiado. Curso 2017-2018
########################################################

# carga de librerías
library("tseries")
# cambio de directorio de trabajo
setwd("/home/cris/mrcrstnherediagmez@gmail.com/seriesTemporales")
# lectura de los datos
serie<-scan("pasajeros_1949_1959.dat")
# dimensión de conjuntos train y test
testDim<-12
toPredict<-12
# creación de objeto ST con estacionalidad=12 meses
timeSerie<-ts(serie,frequency = 12) 
plot(decompose(timeSerie))
# dado que la varianza de la componente aleatoria presenta muchas variaciones, 
# será más complicado predecir la estacionariedad de la serie 

# transformación logaritmica de la serie inicial
timeSerie.log<-log(timeSerie) # del objeto ST
serie.log<-log(serie) # de la serie original
# comprobamos que mejora la varianza
plot(decompose(timeSerie.log))
# división de los datos en train y test
serieTrain<-serie.log[1:(length(serie.log)-testDim)]
timeTrain<-1:length(serieTrain)
serieTest<-serie.log[(length(serie.log)-testDim+1):length(serie.log)]
timeTest<-(length(serieTrain)+1):length(serie.log)
# comprobamos dimensiones de los conjuntos y tiempos
length(timeTrain)
length(timeTest)
timeTrain
timeTest

# mostrar serie de entrenamiento + tiempo de test
plot.ts(serieTrain,xlim=c(1,timeTest[length(timeTest)]))
lines(timeTest,serieTest,col="red")
# modelado de la tendencia (suponiendo lineal cono H0)
params.H1<- lm(serieTrain~ timeTrain) # predecir la serie a partir del tiempo
# estimación de la tendencia de train y test
estimatedTrainTrend<- params.H1$coefficients[1] + timeTrain*params.H1$coefficients[2]
estimatedTestTrend<- params.H1$coefficients[1] + timeTest*params.H1$coefficients[2]
# representación de las tendencias (azul-train, morado-test)
lines(timeTrain,estimatedTrainTrend,col="blue")
lines(timeTest,estimatedTestTrend,col="purple")
# validar el modelo con test estadísticos de normalidad
# test de jarque bera
JBTrain<-jarque.bera.test(params.H1$residuals)
# como  p-value = 0.4158 > 0.05 => no podemos afirmar anormalidad en los errores
JBTest<-jarque.bera.test(serieTest-estimatedTestTrend)
# como  p-value = 0.6442 > 0.05 => no podemos afirmar anormalidad en los errores

# comparación de medias de errores de train y test para ver si son equivalentes
# test t-student
TStudent<-t.test(c(params.H1$residuals,estimatedTestTrend-serieTest))
# distribución de errores de media=0 y no hay desviación significativa con respecto a ella => tienen = media
# podemos asumir que la tendencia es lineal => como ya está modelada, podemos eliminarla
serieTrainWithoutTrend<-serieTrain-estimatedTrainTrend
serieTestWithoutTrend<-serieTest-estimatedTestTrend
plot.ts(serieTrainWithoutTrend,xlim=c(1,timeTest[length(timeTest)]))
lines(timeTest,serieTestWithoutTrend,col="red")
# una vez eliminada la tendencia, eliminamos la estacionalidad
# inicialmente se supuso una estacionalidad=12 meses
t<-12
# obtenemos los 12 vaqlores que se van repitiendo
estacionality<-decompose(timeSerie.log)$seasonal[1:t] 
# tenemos serie sin tendencia de 120 valores y estacionalidad de 
length(serieTrainWithoutTrend)
length(estacionality)
# se elimina la estacionalidad de los 12 primeros valores de la serie, 13 al 24,  del 24 al 35... etc
ntimes<-length(serieTrain)/length(estacionality) # nveces= long serie/long estacionalidad
aux <-rep(estacionality,ntimes)
serieTrainWithoutTrendAndEst<-serieTrainWithoutTrend-aux
serieTestWithoutTrendAndEst<-serieTestWithoutTrend-estacionality
# gráfico de las series sin tendencia ni estacionalidad
plot.ts(serieTrainWithoutTrendAndEst,xlim=c(1,timeTest[length(timeTest)]))
lines(timeTest,serieTestWithoutTrendAndEst,col="red")

# test estadístico para comprobar si la serie es o no estacionaria
# test acf
acf(serieTrainWithoutTrendAndEst) # decrece ràpidamente
# test de dickey-fuller aumentado para contrastar con test acf
adfTrain<-adf.test(serieTrainWithoutTrendAndEst)
# p-value = 0.6427 > 0.05 => no es estacionaria 

# diferenciamos la serie para hacerla estacionaria, ya que no lo es
serieTrainWithoutTrendAndEst.Diff<-diff(serieTrainWithoutTrendAndEst) # train
serieTestWithoutTrendAndEst.Diff<-diff(serieTestWithoutTrendAndEst) # test
# repetimos test adf de las series diferenciadas
adfTrain_diff<-adf.test(serieTrainWithoutTrendAndEst.Diff)
# ahora obtenemos p-value 0.01 < 0.05 => ya sí es estacionaria
acf(serieTrainWithoutTrendAndEst.Diff) # acf que ya no baja muy rápidamente
print("presione tecla para continuar")
pause<-readline()
pacf(serieTrainWithoutTrendAndEst.Diff)
# acf y pacf típicos de MA ya que quedan por debajo de las líneas azules. Podría modelarse como AR(4)
# pero también podría modelarse como ARIMA(4,1,0) [1 pq se ha diferenciado una vez]
model<-arima(serieTrainWithoutTrendAndEst,order=c(4,1,0))
# valores reconstruidos de la serie con el modelo ajustado
val<-serieTrainWithoutTrendAndEst+model$residuals
# hacemos una predicción 
predictions<-predict(model, n.ahead = 12)
predictedvalues<-predictions$pred
# podemos calcular el error del ajuste en train y test con el error cuadrático
errorTrain<-sum(model$residuals^2)
errorTest<-sum((predictedvalues-serieTestWithoutTrendAndEst)^2)# valroes predichos - serie sin tendencia & estacionalidad
# obtenemos un errorTrain=0.1344656 y errorTest=0.01965443

# gráfico de la serie sin tendencia ni estacionalidad
plot.ts(serieTrainWithoutTrendAndEst,xlim=c(1,timeTest[length(timeTest)]))
# pintamos los valores reconstruidos de la serie con el modelo entrenado en color azul
lines(val,col="blue")     
# serie sin tendencia ni estacionalidad en test
lines(timeTest,serieTestWithoutTrendAndEst,col="red")
# valores predichos 
lines(timeTest,predictedvalues,col="purple")         
# la reconstrucción para train es bastante precisa,pero para la predicción (morado) no tanto
# puede deberse a que, como se puede apreciar en el acf y pacf, los valores inmediatamente actuales al momento que queremos
# predecir son relevantes, pero el resto apenas aportan practicamente nada, ya que son muy cercanos a 0 (la serie es prácticamente ruido blanco)

# a pesar de que con Arima (4,1,0) se modela la serie como ruido prácticamente, podemos comprobar si el modelo es válido
# para ello podemos usar test estadísticos como:
# - Box-Pierce: para comprobar que los residuos no son aleatorios
# - Jarque Bera y Shaphiro-Wilk: para comprobar si los residuos son normales 

boxTest<-Box.test(model$residuals)
# p-value = 0.9417 > 0.05 => los errores son aleatorios, pues pasa el test (el modelo modela todas las características de la señal)
# comprobamos entonces si, aunque los residuos son aleatorios, son normales
JBTest<-jarque.bera.test(model$residuals)
ShapkiroTest<-shapiro.test(model$residuals)
# Jarque Bera p-value = 0.8188 , Shapiro  p-value=0.2904 => ambos > 0.04, podemos asumir la normalidad de los errores
# histograma y función de densidad
hist(model$residuals,col="blue", prob=T,ylim=c(0,20),xlim=c(-0.2,0.2))
lines(density(model$residuals),col="green")
# se observa media cero y desv tpípica =0.03357659
# por lo que el modelo queda validado

# para poder calcular las predicciones reales, es necesario deshacer todos los cambios que hemos ido haciendo previamente
# en primer lugar incluimos la estacionalidad
val.Est<-val+aux # valores reconstruidos con estacionalidad
predictedvalues.Est<-predictedvalues+estacionality
# en segundo lugar incluimos la tendencia
val.Est.Trend<-val.Est+estimatedTrainTrend
predictedvalues.Est.Trend<-predictedvalues.Est+estimatedTestTrend
# deshacer el logaritmo, ya que antes calculamos la serie con el log
val.Est.Trend.Exp<-exp(val.Est.Trend)
predictedvalues.Est.Trend.Exp<-exp(predictedvalues.Est.Trend)
# graficamos la serie real en negro, los valores reconstruidos en azul y los predichos en rojo
plot.ts(serie)
lines(timeTrain,val.Est.Trend.Exp,col="blue")
lines(timeTest,predictedvalues.Est.Trend.Exp,col="red")



