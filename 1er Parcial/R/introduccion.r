library(tidyverse)
library(forcats)

# Leo la tabla
df <- read.csv('C:/Facultad/Ciencia de Datos/Introduccion a Ciencia de datos/1er Parcial/flights_september.csv')

# Hago un grafico Basico
ggplot(data=df) + geom_bar(mapping=aes(x=origin),fill='Blue', width=0.5)

# Reordeno los aeropuertos de origen
ggplot(data=df) + geom_bar(mapping=aes(x=fct_rev(fct_infreq(origin)))
                           ,fill='Blue', width=0.5)

# Cambio el orden de origin antes de graficar
df_m <- mutate(df, origin=fct_rev(fct_infreq(origin)))

# Grafico
ggplot(data=df_m) + geom_bar(aes(x=origin),fill='Red', width=0.5)
