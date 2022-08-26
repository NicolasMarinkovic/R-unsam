#Agrupen la tabla de los vuelos de su aerolínea por los meses del año; 
#usen summarise para calcular resúmenes sobre los retrasos 
#en la salida y tiempo ganado en el aire.
library(tidyverse)
library(nycflights13)

df_1 <- group_by(flights, month)
df_2 <- summarise(df_1, nvuelos = n(),
                  nganado=sum(arr_delay<dep_delay, na.rm=TRUE),
                  total =(nganado/nvuelos)*100)

ggplot(data=df_2) + geom_bar(aes(x=month, y=total, color= month), 
                             fill= 'Red', width=0.6,
                            stat = 'identity') +
                     xlim(0,12) + 
  labs(x='Meses', y='Retraso en la partida [min]', 
       title='Retraso de las aerolinea US')
