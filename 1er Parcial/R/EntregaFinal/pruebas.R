library(tidyverse)
library(forcats)
library(dplyr)
library(plyr)

setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/EntregaFinal')
df = read_csv('movies.csv')

problems(df)
sum(is.na(df))
df %>% summarise(distintos=n_distinct(df),
                 total=n(), resta=total-distintos)

df_movies <- df %>%
  filter(year > 2016)

## Le cambio el valor a los scores nulos por la mediana
df_movies <- df_movies %>%
  group_by(year) %>%
  mutate(score = replace(score, is.na(score), median(score,na.rm=TRUE)))%>%
  arrange(-score)

df_top <- ddply(df_movies, "year", function(df) df[1:min(nrow(df), 3),])
