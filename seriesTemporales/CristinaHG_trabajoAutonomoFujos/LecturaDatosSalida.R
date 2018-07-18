setwd("/home/cris/Downloads/moa-release-2017.06b/moa-release-2017.06b/Ej1Res")

# creamos tabla a rellenar con sus nombres en las columnas
data<-cbind(seq(1:20),rep(0,20),rep(0,20))
colnames(data)<-c("seed","acc","kappa")

# rellenamos tabla a partir de los datos de los ficheros
for(i in seq(1:20)){
  dat<-read.csv(paste0("Apartado1ejercicio1semilla",i,".txt"),sep="=")
  data[i,2]<-as.numeric(levels(dat[,2])[dat[1,2]])
  data[i,3]<-as.numeric(levels(dat[,2])[dat[2,2]])
}
# escribimos los resultados a un fichero nuevo
write.table(data,"Apartado1Ej1ACCKappa_Res.txt")

