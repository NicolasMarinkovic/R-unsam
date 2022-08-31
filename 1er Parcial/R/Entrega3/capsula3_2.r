library(tidyverse)
library(forcats)

df <- read.csv('C:/Facultad/Ciencia de Datos/Introduccion a Ciencia de datos/1er Parcial/data/flights_september.csv')

df_m <- mutate(df, origin=fct_rev(fct_infreq(origin)))

df3 <- summarise(df_ma, nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
                 frac_retra=nretra/nvuelos * 100)

# fill es el color(en este caso esta adentro del aes porque queremos que lo coloree por aeropuerto)
p <- ggplot(data=df3) + geom_bar(aes(x=fct_reorder(carrier,frac_retra), y=frac_retra, fill=origin)
                                 , width=0.5,
                                 stat = 'identity',
                                 position = position_dodge(width=0.75,
                                                           preserve='single'))

p <- p + labs(title='Fraccion de vuelos retrasados por aeropuerto de origin',
              subtitle= 'Vuelos de septiembre de 2013',
              x = 'Aeropuerto de origen',
              y = 'Porcentajes de vuelos retrasados')

p