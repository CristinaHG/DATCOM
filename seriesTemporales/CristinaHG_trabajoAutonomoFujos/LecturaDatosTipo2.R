setwd("/home/cris/Downloads/moa-release-2017.06b/moa-release-2017.06b/Ej1Res")

# creamos tabla a rellenar con sus nombres en las columnas
data<-cbind(seq(1:20),rep(0,20),rep(0,20))
colnames(data)<-c("seed","acc","kappa")

# rellenamos tabla a partir de los datos de los ficheros
for(i in seq(1:20)){
  dat<-read.csv(paste0("Apartado2ejercicio1semilla",i,".txt"))
  data[i,2]<-dat[nrow(dat),5]
  data[i,3]<-dat[nrow(dat),6]
}

write.table(data,"Apartado2Ej1ACCKappa_Res.txt")