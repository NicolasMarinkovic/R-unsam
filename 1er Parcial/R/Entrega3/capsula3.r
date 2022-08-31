library(tidyverse)
library(forcats)

df <- read.csv('C:/Facultad/Ciencia de Datos/Introduccion a Ciencia de datos/1er Parcial/data/flights_september.csv')

# Agrupo por aerolinea
df_ma <- group_by(df, carrier)

# Hago una tablita con el numero de vuelos, el numero de retraso y la fraccion de nretra
df1 <- summarise(df_ma, nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
                 frac_retra=nretra/nvuelos * 100)

# Hago el grafico reordenado por carrier y fraccion de retrasado
p <- ggplot(data=df1) + geom_bar(aes(x=fct_reorder(carrier,frac_retra), y=frac_retra),fill='Red', width=0.5,
                                 stat = 'identity')
p <- p + labs(title='Fraccion de vuelos retrasados por aeropuerto de origin',
              subtitle= 'Vuelos de septiembre de 2013',
              x = 'Aeropuerto de origen',
              y = 'Porcentajes de vuelos retrasados')
