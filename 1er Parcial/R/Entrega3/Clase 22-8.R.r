library(tidyverse)
library(nycflights13)

# aerolineas <- unique(flights['carrier'])
# Agrupa por aerolinea
df_1 <- group_by(flights, carrier)

#Cuenten cuantos vuelos tienen
# sirve para modificar una columna existente
df_m <- mutate(df_1, n_vuelos=n())

#Filtrar mayores de 1000 vuelos
df_f <- filter(df_m, n_vuelos > 1000)

df_final <- ungroup(df_f)
#Ejercicio 8
df_8 <- filter(df_f, carrier == 'US')

df_9 <- ggplot(data=df_f) + geom_histogram(aes(x=arr_delay), bins=100) +
  labs(x='Retraso en la partida [min]', y='Cantidad de vuelos', 
       title='Retraso de las aerolinea US') + xlim(-10, 250)   
summary(flights$arr_delay)

#Ejercicio 10

a <- tibble(Métrica = c('Media', 'Mediana', 'DesvEstandar','DistInter'),
            valor = c(mean(flights$arr_delay, na.rm=TRUE), 
                      median(flights$arr_delay, na.rm=TRUE),
                      sd(flights$arr_delay, na.rm=TRUE),
                      IQR(flights$arr_delay, na.rm=TRUE)))

ggplot(data=df_8) + geom_histogram(aes(x=dep_delay), bins=100) 

#Ejercicio 11

df_11 <- filter(df_8, dep_delay > 0)

df_11m <- mutate(df_11, time_recovered = dep_delay + arr_delay)

df_11f <- filter(df_11m, time_recovered < dep_delay)
