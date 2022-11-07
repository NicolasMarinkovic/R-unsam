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

df_companias <- df_movies %>%
  group_by(company) %>%
  summarise(n_peliculas = n()) %>%
  arrange(-n_peliculas) %>%
  head(9)

df_companias_porc <- df_companias %>% 
  summarise(porc = round((n_peliculas*100)/sum(n_peliculas), digits = 2),
            company = company)



df_companias_peliculas <- left_join(df_companias, df_movies, by = "company")


## Graficos
# Compute the position of labels
df_companias_porc <- df_companias %>% 
  arrange(desc(company)) %>%
  mutate(prop = round(n_peliculas / sum(df_companias$n_peliculas) *100) ,digits = 2) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart

ggplot(df_companias_porc, aes(x="", y=prop, fill=company)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = ypos, label = percent(prop/100)), size=10) +
  scale_fill_brewer(palette = "Set1") +
  labs(fill="Compañías")

