#Agrupen la tabla de los vuelos de su aerolínea por los meses del año; 
#usen summarise para calcular resúmenes sobre los retrasos 
#en la salida y tiempo ganado en el aire.
library(tidyverse)
library(nycflights13)
library(ggplot2)
library(ggridges)

df_1 <- group_by(flights, month)
df_2 <- summarise(df_1, nvuelos = n(),
                  nganado=sum((arr_delay<0)<(dep_delay>0), na.rm=TRUE),
                  total =(nganado/nvuelos)*100,
                  Media=mean(arr_delay, na.rm=TRUE), 
                  Mediana=median(arr_delay, na.rm=TRUE),
                  DesvEstandar=sd(arr_delay, na.rm=TRUE),
                  DistInter=IQR(arr_delay, na.rm=TRUE))

ggplot(data=df_2) + geom_bar(aes(x=month, y=Media, fill= month)
                        ,stat = 'identity') +
                        xlim(0,13) +
  labs(x='Meses', y='Media de tiempo de vuelos tardíos en la llegada', 
       title='Promedio de tardanza en Aeropuertos US',
       subtitle='Vuelos de septiembre 2013')

            