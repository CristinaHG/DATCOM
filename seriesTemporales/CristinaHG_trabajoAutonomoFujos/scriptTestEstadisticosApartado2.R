setwd("/home/cris/Downloads/moa-release-2017.06b/moa-release-2017.06b/Ej1Res")
hoeff<-read.csv("Apartado2Ej1ACCKappa_Res.txt",sep = " ")
hoeffAdaptativo<-read.csv("Apartado2Ej2ACCKappa_Res.txt",sep = " ")

# test de normalidad
# shapiro
shapiro.test(hoeff$acc) # p-value = 0.3106 > 0.05
shapiro.test(hoeffAdaptativo$acc) # p-value = 0.112 > 0.05
# jarque bera
library("tseries")
jarque.bera.test(hoeff$acc) # p-value = 0.4348  > 0.05
jarque.bera.test(hoeffAdaptativo$acc) # p-value = 0.4432  > 0.05

# test de normalidad valor Kappa
shapiro.test(hoeff$kappa) # p-value = 0.3323
shapiro.test(hoeffAdaptativo$kappa) #  p-value = 0.1766

# test de diferencia significativa
t.test(hoeff$acc,hoeffAdaptativo$acc) # p-value = 0.008351
t.test(hoeff$kappa,hoeffAdaptativo$kappa) # p-value = 0.008368

# valor medio de acierto en clasif
mean(hoeff$acc) #83.86725
mean(hoeffAdaptativo$acc) #83.80733
# kappa medio
mean(hoeff$kappa) #75.8009
mean(hoeffAdaptativo$kappa) #75.71099
