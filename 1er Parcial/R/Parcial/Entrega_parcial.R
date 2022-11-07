library(modelr)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggplot2)
library(dplyr)
library(readxl)
library(nycflights13)

setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/Parcial')

df <- read.csv('../../data/flights_september.csv')

clima <- nycflights13::weather %>%
  filter(month == 9)

aeropuertos <- nycflights13::airports
names(aeropuertos)[1] <- 'dest'

aerolineas <- nycflights13::airlines

# Agrupo por aerolinea
df_ma <- group_by(df, carrier)

# Hago una tablita con el numero de vuelos, el numero de retraso y la fraccion de nretra
df1 <- summarise(df_ma, nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
                 frac_retra=nretra/nvuelos * 100)

p0 <- ggplot(df_total, aes(x=nvuelos, y=nretra,color=origin)) + geom_point()
grid <- modelr::data_grid(df_total, nvuelos)
# Ajusto modelo lineal
mod1 <- lm(nretra ~ nvuelos, data=df_total)
# Agrego modelo
grid <- modelr::add_predictions(grid, mod1)
# Agrego modelo
p <- p0 + geom_smooth(data=grid, mapping=aes(y=pred), color='red')
p <- p + labs(title = 'Retraso de aerolineas por aeropuerto',
              x = 'Número de vuelos',
              y = 'Vuelos retrasados',
              color = "Aeropuerto") +
  theme(
    plot.title = element_text(color='Black',face="bold.italic"),
    axis.title.x = element_text(color='Black',face="bold"),
    axis.title.y = element_text(color='Black',face="bold"))
p
plot_grid(gfRetraso,p)

# Hago el grafico reordenado por carrier y fraccion de retrasado
gfRetraso <- ggplot(data=df1) + geom_bar(aes(x=fct_reorder(carrier,nvuelos), 
                                     y=frac_retra),fill='Red'
                                 , color = 'Black', width=0.9,
                                 stat = 'identity')
gfRetraso <- gfRetraso + labs(title='Fracción de vuelos retrasados por aerolínea',
              subtitle= 'Vuelos de septiembre de 2013',
              x = 'Aerolínea',
              y = 'Porcentajes de vuelos retrasados')

gfCantVuelos <- ggplot(data=df1) + geom_bar(aes(x=fct_reorder(carrier,nvuelos), 
                                     y=nvuelos), color = 'Black', fill = 'red', 
                                 width=0.9,
                                stat = 'identity') 

gfCantVuelos <- gfCantVuelos + labs(title='Cantidad de vuelos por aerolínea',
              subtitle= 'Vuelos de septiembre de 2013',
              x = 'Aerolínea',
              y = 'Número de vuelos')


plot_grid(gfCantVuelos,gfRetraso)


################################3

# 2 GRAFICO

df_ma <- group_by(df, origin)
df2 <- summarise(df_ma, nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
                 frac_retra=nretra/nvuelos * 100) 

gfRetraso <- ggplot(data=df2) + geom_bar(aes(x=fct_reorder(origin,nvuelos), 
                                             y=frac_retra),fill='Red'
                                         , color = 'Black', width=0.9,
                                         stat = 'identity')
gfRetraso <- gfRetraso + labs(title='Fracción de vuelos retrasados por aeropuerto de origen',
                              subtitle= 'Vuelos de septiembre de 2013',
                              x = 'Aeropuerto de origen',
                              y = 'Porcentajes de vuelos retrasados')

gfCantVuelos <- ggplot(data=df2) + geom_bar(aes(x=fct_reorder(origin,nvuelos), 
                                                y=nvuelos), color = 'Black', fill = 'red', 
                                            width=0.9,
                                            stat = 'identity') 

gfCantVuelos <- gfCantVuelos + labs(title='Cantidad de vuelos por aeropuerto de origen',
                                    subtitle= 'Vuelos de septiembre de 2013',
                                    x = 'Aeropuerto de origen',
                                    y = 'Número de vuelos')

plot_grid(gfCantVuelos,gfRetraso)


################################


df_ma <- group_by(df, dest)
df3 <- summarise(df_ma, nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
                 frac_retra=nretra/nvuelos * 100)
df3 <- df3 %>%
  inner_join(aeropuertos, by='dest')
df3 <- group_by(df3, tzone)
df4 <- summarise(df3, nvuelos2=sum(nvuelos), nretra2=sum(frac_retra, na.rm=TRUE),
                 frac_retra2=nretra2/nvuelos2 * 100)

p1 <- ggplot(df4, aes(x=nvuelos2, y=nretra2,color=tzone)) + geom_point()
grid <- modelr::data_grid(df4, nvuelos2)
# Ajusto modelo lineal
mod1 <- lm(nretra2 ~ nvuelos2, data=df4)
# Agrego modelo
grid <- modelr::add_predictions(grid, mod1)
# Agrego modelo
p2 <- p1 + geom_smooth(data=grid, mapping=aes(y=pred), color='red')
p2 <- p2 + labs(title = 'Retraso de aerolineas por aeropuerto',
              x = 'Número de vuelos',
              y = 'Vuelos retrasados',
              color = "Aeropuerto") +
  theme(
    plot.title = element_text(color='Black',face="bold.italic"),
    axis.title.x = element_text(color='Black',face="bold"),
    axis.title.y = element_text(color='Black',face="bold"))
p2 


gfRetraso <- ggplot(data=df4) + geom_bar(aes(x=fct_reorder(tzone,nvuelos2), 
                                             y=frac_retra2),fill='Red'
                                         , color = 'Black', width=0.9,
                                         stat = 'identity')
gfRetraso <- gfRetraso + labs(title='Fracción de vuelos retrasados en aeropuertos de llegada x Zona Horaria',
                              subtitle= 'Vuelos de septiembre de 2013',
                              x = 'Zona Horaria',
                              y = 'Porcentajes de vuelos retrasados')

gfCantVuelos <- ggplot(data=df4) + geom_bar(aes(x=fct_reorder(tzone,nvuelos2), 
                                                y=nvuelos2), color = 'Black', fill = 'red', 
                                            width=0.9,
                                            stat = 'identity') 

gfCantVuelos <- gfCantVuelos + labs(title='Cantidad de vuelos en aeropuertos de llegada x Zona Horaria',
                                    subtitle= 'Vuelos de septiembre de 2013',
                                    x = 'Zona Horaria',
                                    y = 'Número de vuelos')

gf <- ggplot(data=df4) + geom_bar(aes(nvuelos2, y=frac_retra2,
                                      fill=tzone),
                                      width = 300,
                                            stat = 'identity') 

gf <- gf + labs(title='Retraso en la salida x Zona Horaria',
                                    subtitle= 'Vuelos de septiembre de 2013',
                                    y = 'Porcentaje de retraso',
                                    x = 'Cantidad de vuelos',
                                    fill='Zona Horaria')

gf

df4 <- df4 %>%
  filter(nvuelos2 > 56)
plot_grid(gfCantVuelos,gfRetraso)


df_clima <- df %>%
  inner_join(clima, by = c("year", "month", "day", "origin", "hour"))

df_JFK <- df_clima %>%
  filter(origin == 'JFK') %>%
  filter(wind_speed < 25)

df_EWR <- df_clima %>%
  filter(origin == 'EWR') %>%
  filter(wind_speed < 25)

df_LGA <- df_clima %>%
  filter(origin == 'LGA') %>%
  filter(wind_speed < 25)

df_total <- df5 %>%
  full_join(df6) %>%
  full_join(df7) 
  

df_ma <- group_by(df_JFK, carrier)

# Hago una tablita con el numero de vuelos, el numero de retraso y la fraccion de nretra
df5 <- df_ma %>%
  summarise(nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
                 frac_retra=nretra/nvuelos * 100)
df5$origin = 'JFK'
#################33333
df_ma <- group_by(df_EWR, carrier)

# Hago una tablita con el numero de vuelos, el numero de retraso y la fraccion de nretra
df6 <- df_ma %>%
  summarise(nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
            frac_retra=nretra/nvuelos * 100)
df6$origin = 'EWR'
#########################3
df_ma <- group_by(df_LGA, carrier)

# Hago una tablita con el numero de vuelos, el numero de retraso y la fraccion de nretra
df7 <- df_ma %>%
  summarise(nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
            frac_retra=nretra/nvuelos * 100)
df7$origin = 'LGA'
