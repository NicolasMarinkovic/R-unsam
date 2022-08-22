library(tidyverse)
library(forcats)

# el /help es con el ? adelante. Ej : poner en consola ?geom_bar

# Leo la tabla (df = data frame, pero le podes poner cualquier nombre)
df <- read.csv('C:/Facultad/Ciencia de Datos/Introduccion a Ciencia de datos/1er Parcial/data/flights_september.csv')

# Hago un grafico Basico
ggplot(data=df) + geom_bar(mapping=aes(x=origin),fill='Blue', width=0.5)

# Reordeno los aeropuertos de origen
ggplot(data=df) + geom_bar(mapping=aes(x=fct_rev(fct_infreq(origin)))
                           ,fill='Blue', width=0.5)

# Cambio el orden de origin antes de graficar
df_m <- mutate(df, origin=fct_rev(fct_infreq(origin)))

# Grafico
ggplot(data=df_m) + geom_bar(aes(x=origin),fill='Red', width=0.5)

# Filtrar
df2 <- filter(df_m, dep_delay > 60)
ggplot(data=df2) + geom_bar(aes(x=origin),fill='Red', width=0.5)

# Agrupar (se desagrupa con ungroup)
df_ma <- group_by(df_m, origin)

#summarise hace sumarios. n cuenta la cantidad de elementos por cada grupo
df3 <- summarise(df_ma, nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
          frac_retra=nretra/nvuelos * 100)

p <- ggplot(data=df3) + geom_bar(aes(x=origin, y=frac_retra),fill='Red', width=0.5,
                            stat = 'identity')
p <- p + labs(title='Fraccion de vuelos retrasados por aeropuerto de origin',
           subtitle= 'Vuelos de septiembre de 2013',
           x = 'Aeropuerto de origen',
           y = 'Porcentajes de vuelos retrasados')


