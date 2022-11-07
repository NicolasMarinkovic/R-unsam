library(modelr)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggplot2)
library(dplyr)
library(readxl)
library(nycflights13)

setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/EntregaFinal')

df = read_csv('forbes_celebrity_100.csv')

df_pagosAnuales <- df %>%
  group_by(Name) %>%
  filter(Category == 'Actors') %>%
  mutate(n_apariciones = n(), usdTotales = sum(`Pay (USD millions)`),
         usdPromedio = round(usdTotales/n_apariciones,digits=2)) %>%
  arrange(-usdTotales)

colnames(df_pagosAnuales)[2] <- "Pay"

df_actoresElegidos <- df_pagosAnuales %>%
  filter(Name %in% c('Tom Hanks', 'Vin Diesel','Ryan Reynolds',
                     'Tom Cruise','Robert Downey Jr.','Will Smith',
                     'Leonardo DiCaprio', 'Amitabh Bachchan'))

df_actoresElegidos %>%
group_by(Name) %>%
ggplot() + geom_bar(aes(reorder(Name,usdPromedio), y=(Pay/n_apariciones),
                                fill=Name),
                            stat = 'identity') +
  labs(title = 'Sueldos aproximados de algunas estrellas mencionadas',
         subtitle = 'Sueldos entre 2005 y 2020', 
         x = 'Actor',
         y = 'Pago promedio por pelicula',
         fill = "Actores") +
  theme(
    plot.title = element_text(family = 'sans', color='Black',face="bold"),
    plot.subtitle = element_text(family = 'sans', color='Black'),
    axis.title.x = element_text(family = 'serif', color='Black',face="bold"),
    axis.title.y = element_text(family = 'serif', color='Black',face="bold"),
    plot.caption = element_text(family = 'serif', color = '#757575'),
    axis.line = element_line(color = "black", size = 0.5, linetype = 1),

    axis.ticks.x = element_blank() #esto lo deja en blanco al eje
  )

