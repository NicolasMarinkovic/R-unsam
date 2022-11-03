library(tidyverse)
library(ggplot2)
library(dplyr)


setwd('C:/Facultad/Unsam/Licenciatura en ciencia de Datos/Introduccion a Ciencia de Datos/1er Parcial/R/EntregaFinal')

df = read_csv('movies.csv')

df_movies <- df %>%
  filter(year > 2004)

## Le cambio el valor a los socres nulos por la mediana
df_movies <- df_movies %>%
  group_by(year) %>%
  mutate(score = replace(score, is.na(score), median(score,na.rm=TRUE)) )

df_movies <- df_movies %>%
  arrange(-score) %>%
  head(50)

df_directores <- df_movies %>%
  group_by(director) %>%
  summarise(n_peliculas = n()) %>%
  arrange(-n_peliculas) %>%
  head(10)

df_actores <- df_movies %>%
  group_by(star) %>%
  summarise(n_peliculas = n()) %>%
  arrange(-n_peliculas) %>%
  head(10)

df_actores_peliculas <- left_join(df_actores, df_movies, by = "star")


