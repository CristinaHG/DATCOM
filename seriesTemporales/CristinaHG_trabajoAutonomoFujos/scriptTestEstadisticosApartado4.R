setwd("/home/cris/Downloads/moa-release-2017.06b/moa-release-2017.06b/Ej1Res")
hoeff<-read.csv("Apartado3Ej1ACCKappa_Res.txt",sep = " ")
hoeffAdaptativo<-read.csv("Apartado3Ej2ACCKappa_Res.txt",sep = " ")

# test de normalidad
# shapiro
shapiro.test(hoeff$acc) # p-value = 0.2712 > 0.05
shapiro.test(hoeffAdaptativo$acc) # p-value = 0.2088 > 0.05
# jarque bera
library("tseries")
jarque.bera.test(hoeff$acc) # p-value = 0.6738  > 0.05
jarque.bera.test(hoeffAdaptativo$acc) # p-value = 0.4408  > 0.05

# test de normalidad valor Kappa
shapiro.test(hoeff$kappa) # p-value = 0.3623
shapiro.test(hoeffAdaptativo$kappa) #  p-value = 0.006073 < 0.05

# test de diferencia significativa
t.test(hoeff$acc,hoeffAdaptativo$acc) # p-value =2.127e-06
wilcox.test(hoeff$kappa,hoeffAdaptativo$kappa) # p-value = 2.017e-09

# valor medio de acierto en clasif
mean(hoeff$acc) #77.70211
mean(hoeffAdaptativo$acc) #93.75567
# kappa medio
median(hoeff$kappa) #27.26597
median(hoeffAdaptativo$kappa) #80.40007
