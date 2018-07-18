# María Cristina Heredia Gómez
# Ejercicios Regresión Ev.Continua

# Carga de librarías a usar
require(kknn)

####  EJERCICIOS LABORAROTORIO 1

# suponiendo que los datos se encuentran en el directorio de trabajo
# podemos comprobar nuestro directorio de trabajo con getwd()
getwd()

# cargamos los datos
california<-read.csv("california.dat", comment.char="@")
# renombramos las variables para facilitar trabajar con ellas
n<-length(names(california)) -1
names(california)[1:n] <- paste ("X", 1:n, sep="")
names(california)[n+1] <- "Y"
temp <- california

# pintamos una variable frente a la salida
plot(Y~X1,temp)
# vemos que va a ser difícil explicar el precio medio de la vivienda (Y) a partir de la Longitud (X1)
# previsualizamos todas las variables respecto a la salida
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=names(temp)[x], ylab=names(temp)[y])
}
par(mfrow=c(2,4))
x<-sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))

# podemos ver que las variables más relevantes parecen ser de X4,X5,X6,X7,X8 y quizás X3
# Obtenemos un modelo lineal para cada una de ellas
fitX8 <- lm(Y~X8, california)
fitX7 <- lm(Y~X7, california)
fitX6 <- lm(Y~X6, california)
fitX5 <- lm(Y~X5, california)
fitX4 <- lm(Y~X4, california)
fitX3 <- lm(Y~X3, california)
fitX2 <- lm(Y~X2, california)
fitX1 <- lm(Y~X1, california)


# los representamos
plotYabline <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=names(temp)[x], ylab=names(temp)[y])
  abline(lm(temp[,y]~temp[,x], california),col="red")
}
par(mfrow=c(2,4))
x<-sapply(1:(dim(temp)[2]-1), plotYabline, dim(temp)[2])
par(mfrow=c(1,1))

# vemos que los ajustes lineales simples son muy malos en todos los casos excepto X8
#probamos a ver resumen de estos modelos con Summary
summary(fitX1) # obtenemos un R cuadrado ajustado de 0.002063 
confint(fitX1)
summary(fitX2) # obtenemos un R cuadrado ajustado de 0.02074 
confint(fitX2)
summary(fitX3) # obtenemos un R cuadrado ajustado de 0.01111 
confint(fitX3)
summary(fitX4) # obtenemos un R cuadrado ajustado de 0.01796  
confint(fitX4)
summary(fitX5) # obtenemos un R cuadrado ajustado de 0.002516
confint(fitX5)
summary(fitX6) # obtenemos un R cuadrado ajustado de 0.0005582 
confint(fitX6)
summary(fitX7) # obtenemos un R cuadrado ajustado de 0.004294 
confint(fitX7)
summary(fitX8) # obtenemos un R cuadrado ajustado de 0.4734 
confint(fitX8)

# Centrándonos en el modelo fitX8
names(fitX8)
# Cálculo manual de la raíz del ECM
sqrt(sum(fitX8$residuals^2)/length(fitX8$residuals)) #83735.5
sqrt(sum(fitX8$residuals^2)/(length(fitX8$residuals)-2))#83739.56
# que si lo comparamos con la salida de summary, que es 83740, coincide

# Obtención del modelo añadiendo variables
fitX8X3=lm(Y~X8+X3,data=california)
summary(fitX8X3)
# obtenemos un modelo con un r cuadrado ajustado de 0.5091 con 20636 grados de libertad

#probamos a añadir X7
fitX8X7=lm(Y~X8+X7,data=california)
summary(fitX8X7)
# obtenemos un modelo con un r cuadrado ajustado de 0.4766 con 20636 grados de libertad

# Obtención del modelo con todas las variables
fitAll=lm(Y~.,data=california)
summary(fitAll)
# podemos observar que aunque no haya p-valores altos hay muchas variables insignificantes
# pues usando todos los valores obtenemos un r cuadrado ajustado de 0.637 

# podemos eliminar variables aparentemente poco significativas, como X4 
fitWithoutX4=lm(Y~.-X4,data=california)
summary(fitWithoutX4)
# obtenemos un R cuadrado ajustado 0.635

# probamos a eliminar X7 también
fitWithoutX4X7=lm(Y~.-X4-X7,data=california)
summary(fitWithoutX4X7)
#obtenemos un R cuadrado ajustado de 0.6343
#apenas ha varido el resultado de usar todas las variables y ya hemos eliminado dos

# probamos interacciones entre las variables
fitWithIter<-lm(Y~X1*X2*X3*X5*X6*X8,data = california)
summary(fitWithIter)
# obtenemos un R cuadrado ajustado de 0.6837 

#probamos iteracciones con No linealidad
fitNonLineal=lm(Y~X8*I((X7*X6)^2)*(X1*X2*X5*X3),california)
summary(fitNonLineal)
plot(Y~X8,california)
points(california$X8,fitted(fitNonLineal),col="red",pch=20)
# sale un resultado peor (0.6392)

# Cálculo manual de la raíz del ECM (RMSE) para conjuntos de test
yprime=predict(fitWithIter,california)
sqrt(sum(abs(california$Y-yprime)^2)/length(yprime))

# obtenemos un ECM de 69207.66, a pesar de que con el modelo el R^2 ajustado era 0.6837
# quizás una aproximación lineal no es la mejor forma de abordar el problema



####  EJERCICIOS LABORAROTORIO 2

# cargamos los datos
california<-read.csv("california.dat", comment.char="@")
# renombramos las variables para facilitar trabajar con ellas
n<-length(names(california)) -1
names(california)[1:n] <- paste ("X", 1:n, sep="")
names(california)[n+1] <- "Y"
temp <- california

# Uso de KNN - Obtención del modelo para un conjunto de datos
fitknn1 <- kknn(Y ~ ., california, california)
# Información en el objeto
names(fitknn1)
# Visualización
plot(Y~X8,california)
points(california$X8,fitknn1$fitted.values,col="blue",pch=20)

# Nuevas predicciones y cálculo manual de la raíz del ECM (RMSE)
yprime = fitknn1$fitted.values
# o también yprime=predict(fitknn1,california)
sqrt(sum((california$Y-yprime)^2)/length(yprime))
# obtenemos un error de 39132.79 , mucho mejor que el valor obtenido con el otro modelo de RL

# Influencia de las variables-Considerando las interacciones aprendidas con regressión lineal
fitknn2 <- kknn(Y ~ X8*X3+I(X3^2)+X5+X6+X7, california, california)
yprime = fitknn2$fitted.values; sqrt(sum((california$Y-yprime)^2)/length(yprime)) #RMSE
# Obtenemos un modelo con error 47937.28

# K-fold cross-validation – Obtención de las medias del error sobre las mismas particiones
# Como ejemplo usamos el modelo LM con todas las variables (para MSE)
nombre <- "california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=lm(Y~.,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))

# lmMSEtrain = 4826189710
# lmMSEtest = 4844365688

# Con k-NN (para MSE)
nombre <- "california"
run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=kknn(Y~.,x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
# knnMSEtrain= 1560868807
# knnMSEtest= 3845914481

# Cómo afrontar un problema dado
# Sobre el conjunto total:
# (1) Se analizan los datos para comprenderlos y se plantean los modelos lineales simples a modo de estudio
# como ya tenemos los datos leídos y renombrados de antes, y también tenemos la función plotY creada del laboratorio anterior
# pasamos directamente a representar los datos
par(mfrow=c(2,4)); x<-sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2]); par(mfrow=c(1,1))
fitX8 <- lm(Y~X8, california)
fitX7 <- lm(Y~X7, california)
fitX4 <- lm(Y~X4, california)
fitX6 <- lm(Y~X6, california)
fitX3 <- lm(Y~X3, california)
fitX5 <- lm(Y~X5, california)
fitX2 <- lm(Y~X2, california)
fitX1 <- lm(Y~X1, california)
# (2) Se construye el modelo multivariable con todas y luego intentamos
# mejorarlo con interacciones, no linealidad, etc. Es un proceso manual y
# tedioso, pero ayuda a comprender el problema.
fit1=lm(Y~X8+I(X8^2)+I(X8^3)+I(log(X3))+I(log(X4/X6))
        +I(log(X5/X6))+I(log(X6/X7))+I(log(X7)),california)
#Modelo que usa el logaritmo por ser Y=ln(median house value)
#Haciendo pruebas y analizando los datos se plantean posibles
#modelos hasta llegar a uno satisfactorio
fit2=lm(Y~., california)
fit3=lm(Y~.+X4*X7*X8, california)
fit4=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)+I(X8^4)
        +X7*X8*X4*X5*X6, california)
summary(fit1)$adj.r.squared
summary(fit2)$adj.r.squared
summary(fit3)$adj.r.squared
summary(fit4)$adj.r.squared
# (3) Se utiliza k-fcv para ver la capacidad de generalización del modelo.
# (4) Se prueba con otras técnicas y se comparan según el MSE de test medio,
# sacando las conclusiones oportunas que sirvan para escoger el modelo más
# adecuado atendiendo a los criterios de predición vs inferencia
nombre <- "california"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep=""); x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep=""); x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep=""); names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep=""); names(x_tst)[In+1] <- "Y"
  if (tt == "train") { test <- x_tra }
  else { test <- x_tst }
  fitMulti=lm(Y~.+I(X1^2)+I(X6^2)+I(X8^2)+I(X8^3)
              +I(X8^4)+X7*X8*X4*X5*X6,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}

lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
# lmMSEtrain= 4151294153
# lmMSEtest=5319480881
# vemos que el MSE de test se ha incrementado frente al que obtuvimos antes

# Comparativa general entre distintos algoritmos
# Dadas las tablas disponibles en csv con resultados de LM,
# k-NN y el modelo de regresión M5’ aplicamos test estadísticos

#leemos la tabla con los errores medios de test
resultados <- read.csv("regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]

#leemos la tabla con los errores medios de train
resultados <- read.csv("regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) <- resultados[,1]

# Comparativa por pares de LM y KNN (Wilcoxon’s test)
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)
# Se aplica el test y se interpretan los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas # 78 
Rmenos # 93
pvalue # 0.7660294
# hay un (1-0.7660)*100 = 23.4% de confianza en que sean distintos. No es significativa
# Comparativas Múltiples –  Friedman 
test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman$p.value # 0.01466602
# existen diferencias significativas al menos entre un par de algoritmos

# Comparativas Múltiples – post-hoc Holm
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
# Existen diferencias significativas a favor de M5  

# Aplicamos lo mismo Sobre train
# Comparativa por pares de LM y KNN (Wilcoxon’s test)
difs <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2)
# Se aplica el test y se interpretan los resultados
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas # 10 
Rmenos # 161 
pvalue #  0.000328064
# para train parece que sí es significativa la diferencia

# Comparativas Múltiples –  Friedman 
test_friedman <- friedman.test(as.matrix(tablatra))
test_friedman$p.value # 3.843021e-05
# existen diferencias significativas al menos entre un par de algoritmos

# Comparativas Múltiples – post-hoc Holm
tam <- dim(tablatra)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatra), groups, p.adjust = "holm", paired = TRUE)
# 3 vs 1 = 0.0032 y 3 vs 2=0.0032 no hay diferencias a favor de M5' 



