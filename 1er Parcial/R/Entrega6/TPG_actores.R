library(tidyverse)
library(ggplot2)
library(dplyr)


setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/EntregaFinal')

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

df_actores <- df_movies %>%
  group_by(star) %>%
  summarise(n_peliculas = n()) %>%
  arrange(-n_peliculas) %>%
  head(10)

df_actores_peliculas <- left_join(df_actores, df_movies, by = "star")

df_actores_peliculas <- df_actores_peliculas %>%
  group_by(star) %>%
  summarise(n_peliculas = n(), sProm = round((sum(score)/n_peliculas) ,digits=2)) %>%
  arrange(-n_peliculas)

