library(tidyverse)
library(ggplot2)
library(dplyr)


setwd('C:/Facultad/Unsam/Licenciatura en ciencia de Datos/Introduccion a Ciencia de Datos/1er Parcial/R/EntregaFinal')

df = read_csv('forbes_celebrity_100.csv')

df_pagosAnuales <- df %>%
  group_by(Name) %>%
  filter(Category == 'Actors') %>%
  summarise(n_apariciones = n(), usdTotales = sum(`Pay (USD millions)`),
            usdPromedio = usdTotales/n_apariciones) %>%
  arrange(-usdTotales)
  

problems(df_actors)

colnames(df_actors)
  
sum(is.na(df_actors))

glimpse(df_actors)
