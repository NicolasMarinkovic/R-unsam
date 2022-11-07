library(tidyverse)
library(forcats)
library(dplyr)

setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/EntregaFinal')
df = read_csv('movies.csv')

df_movies <- df %>%
  filter(year > 2004)
  
df_movies <- df_movies %>%
  group_by(genre) %>%
    summarise(n_peliculas = n())

df_movies_conteo <- df %>%
  group_by(genre) %>%
  filter(year > 2004) %>%
  summarise(n_peliculas = n()) %>%
  arrange(-n_peliculas) %>%
  head(8)

df_movies_pelis <- df_movies %>%
  filter(genre %in% c("Action", "Comedy","Drama","Biography","Animation","Crime","Adventure","Horror"))

## Le cambio el valor a los socres nulos por la mediana
df_movies <- df_movies %>%
  group_by(year) %>%
  mutate(score = replace(score, is.na(score), median(score,na.rm=TRUE)) )

df_movies <- df_movies %>%
  arrange(-score) %>%
  head(50)

glimpse(df_1)
df_movies$genre = as.factor(df_movies$genre)

# los 10 generos con mejor score
df_1 <- df_movies %>% 
  group_by(genre) %>%
  summarise( n_genero = n(),
             total_score =  sum(score, na.rm = TRUE),
             prom_score=(total_score /n_genero)) %>% 
  arrange(desc(n_genero)) %>% 
  head(10)

####################
#GRAFICOS

ggplot(df_1)+ theme(legend.position = "none") +
  geom_bar(mapping = aes(x = fct_infreq(genre),
                         y = n_genero, fill = genre),
           stat = "identity") + 
  labs(title = "¿Cuantas peliculas hay de cada genero?",
       x = "Genero",
       y = "Cantidad de Peliculas")

ggplot(df_1)+ theme(legend.position = "none") +
  geom_bar(mapping = aes(x = fct_infreq(genre),
                         y = total_score, fill = genre),
           stat = "identity") + 
  labs(title = "Puntaje Total por Genero",
       x = "Genero",
       y = "Sumatoria de Puntaje")

ggplot(df_movies_pelis, aes(x = genre, y = score, fill = genre))+
  geom_violin(trim=FALSE) + 
  labs(title = "Promedio de Puntajes",
       x = "Genero",
       y = "Score",
       legend = "asd") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.title  = element_blank())


# DIRECTOR

df_dir <- df_movies %>%
  group_by(director) %>% 
  summarise(cant = n(), prom_puntaje = mean(score)) %>% 
  head(7)

