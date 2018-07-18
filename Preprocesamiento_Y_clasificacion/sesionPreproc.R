library(ggplot2)
library(tidyverse)

?mpg

# agrupación de puntos en gráficos diferentes
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=class))+
  facet_wrap(~class,nrow=2)


# representación de los puntos en función de la agrupación de dos variables
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ,y=hwy,color=class))+
  facet_grid(drv~cyl)

# agregar líneas de tendencia sobre los datos
#con se=FALSE quita la franja gris con la desv asociada
ggplot(data=mpg,mapping = aes(x=displ,y=hwy))+geom_smooth(se=FALSE)+
  geom_point(mapping = aes(x=displ,y=hwy,color=class))

library(dplyr)
ggplot(data=mpg,mapping = aes(x=displ,y=hwy))+
  geom_point(mapping = aes(color=class))+
  geom_smooth(data=filter(mpg,class=="subcompact"),se=FALSE)

?diamonds

ggplot(data=diamonds)+ geom_bar(mapping = aes(x=cut))

# representar en el eje x proporciones
ggplot(data = diamonds)+geom_bar(mapping = aes(x=cut,y=..prop..,group=1))
#agregar + inf al gráfico (min,max,mediana)
ggplot(data=diamonds)+stat_summary(mapping = aes(x=cut,y=depth),
                                   fun.ymin = min,
                                   fun.ymax = max,
                                   fun.y = median)
#para que salgan los solapados despazandolos un poco
ggplot(data=mpg)+
  geom_point(mapping = aes(x=displ,y=hwy),position = "jitter")

#boxplots
ggplot(data=mpg,mapping = aes(x=class,y=hwy))+geom_boxplot()+
  coord_flip() #rota coordnadas


