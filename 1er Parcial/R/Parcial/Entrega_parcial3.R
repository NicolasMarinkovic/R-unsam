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

# Agrupo por aerolinea
df_ma <- group_by(df_EWR, carrier)

# Hago una tablita con el numero de vuelos, el numero de retraso y la fraccion de nretra
df1 <- summarise(df_ma, nvuelos=n(), nretra=sum(dep_delay>60, na.rm=TRUE),
                 frac_retra=nretra/nvuelos * 100)

gfRetraso <- ggplot(data=df1) + geom_bar(aes(x=fct_reorder(carrier,nvuelos), 
                                             y=frac_retra),fill='darkred'
                                         , color = 'Black', width=0.9,
                                         stat = 'identity')
gfRetraso <- gfRetraso + labs(title='Porcentaje de Vuelos con retraso mayor a 60 minutos',
                              subtitle= 'Salieron del Aeropuerto EWR',
                              x = 'Aerolínea',
                              y = 'Porcentajes de vuelos retrasados')

gfCantVuelos <- ggplot(data=df1) + geom_bar(aes(x=fct_reorder(carrier,nvuelos), 
                                                y=nvuelos), color = 'Black', 
                                            fill = 'darkred', 
                                            width=0.9,
                                            stat = 'identity') 

gfCantVuelos <- gfCantVuelos + labs(title='Vuelos que arribaron después de 60 minutos',
                                    subtitle= 'Salieron con retraso del Aeropuerto EWR',
                                    x = 'Aerolínea',
                                    y = 'Número de vuelos')

plot_grid(gfCantVuelos,gfRetraso)

