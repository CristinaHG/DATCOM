setwd("/home/cris/Downloads/moa-release-2017.06b/moa-release-2017.06b/Ej1Res")
hoeff<-read.csv("Apartado1Ej1ACCKappa_Res.txt",sep = " ")
hoeffAdaptativo<-read.csv("Apartado1Ej2ACCKappa_Res.txt",sep = " ")

# test de normalidad
# shapiro
shapiro.test(hoeff$acc) # p-value = 0.3328 > 0.05
shapiro.test(hoeffAdaptativo$acc) # p-value = 0.1754 > 0.05
# jarque bera
library("tseries")
jarque.bera.test(hoeff$acc) # p-value = 0.1759  > 0.05
jarque.bera.test(hoeffAdaptativo$acc) # p-value = 0.4724  > 0.05

# test de normalidad valor Kappa
shapiro.test(hoeff$kappa) # p-value = 0.3323
shapiro.test(hoeffAdaptativo$kappa) #  p-value = 0.1766

# test de diferencia significativa
t.test(hoeff$acc,hoeffAdaptativo$acc) # p-value = 0.0006479
t.test(hoeff$kappa,hoeffAdaptativo$kappa) # p-value = 0.0006495

# valor medio de acierto en clasif
mean(hoeff$acc) #84.52885
mean(hoeffAdaptativo$acc) #84.39025
# kappa medio
mean(hoeff$kappa) #76.795
mean(hoeffAdaptativo$kappa) #76.58715
