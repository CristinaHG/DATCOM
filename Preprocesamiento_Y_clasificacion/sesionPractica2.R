library(dplyr)
library(nycflights13)

#conjunto de datos como tibble
flights

unoEnero<-filter(flights,month==1,day==1)

#operaciones relacionales
retrasosLlegadas<-filter(flights,arr_delay>120 & dep_delay>120)
#vuelos en enero,febrero o marzo
eneroFebreroMarzo<-filter(flights,month %in% c(1,2,3))
# reordenar las filas
origenDestino<- arrange(flights,origin,dest)
# selección de columnas
algunasColumnas<-select(flights,year,month,day,origin,dest)
# selección con rango
selecConRango<-select(flights,year:dest)
selecSinRango<-select(flights,-(year:dest))
#que empiecen o terminen por algo
seleccionadasEmpiezan<-select(flights,starts_with("a"))
seleccionadasAcaban<-select(flights,ends_with("a"))
seleccionadasContienen<-select(flights,contains("a"))
# cambio de nombres de variables
conCamio<-rename(flights,tailnumber=tailnum)
# isntroducción de nuevas variables
versionNueva<-mutate(flights,gain=arr_delay-dep_delay,speed=distance/air_time*60)
#quedarnos solo con las variables nuevas generadas
versionNueva<-transmute(flights,gain=arr_delay-dep_delay,speed=distance/air_time*60)
#agrupamiento de instancias
porDia<-group_by(flights,year,month,day)
#resumen
resume<-summarize(porDia,delay=mean(dep_delay,na.rm=T))

#encadenamiento mediante pipas
#paso 1: agrupamiento por destino
porDestino<-group_by(flights,dest)
#paso 2:determinar el retraso por destino
retraso<-summarize(porDestino,count=n(),dist=mean(distance,na.rm=T),delay=mean(arr_delay,na.rm=T))
#paso 3_ filtrado
filtrado<-filter(retraso,count > 20, dest!="HNL")
#representación de los datos
ggplot(filtrado, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = F)

resultado<-flights %>%
            group_by(dest) %>%
            summarize(count=n(),dist=mean(distance,na.rm=T),delay=mean(arr_delay,na.rm=T)) %>%
            filter(count>20,dest!="HNL") %>%
            ggplot(mapping =  aes(dist, delay)) +
            geom_point(aes(size = count), alpha = 1/3) +
            geom_smooth(se = F)

#conversion a tibble
datos<-as_tibble(iris)
