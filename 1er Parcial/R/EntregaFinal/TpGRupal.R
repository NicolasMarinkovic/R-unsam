library(tidyverse)
library(ggplot2)
library(dplyr)


setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/EntregaFinal')

df = read_csv('forbes_celebrity_100.csv')

df_pagosAnuales <- df %>%
  group_by(Name) %>%
  filter(Category == 'Actors') %>%
  summarise(n_apariciones = n(), usdTotales = sum(`Pay (USD millions)`),
            usdPromedio = round(usdTotales/n_apariciones,digits=2)) %>%
  arrange(-usdTotales)
  
df_actoresElegidos <- df_pagosAnuales %>%
  filter(Name %in% c('Tom Hanks', 'Vin Diesel','Ryan Reynolds',
                     'Tom Cruise','Robert Downey Jr.','Will Smith',
                     'Leonardo DiCaprio'))


problems(df_actors)

colnames(df_actors)
  
sum(is.na(df_actors))

glimpse(df_actors)

dmod <- lm(n_apariciones ~ usdTotales, data = df_pagosAnuales)
usdTotales <- unique(df_pagosAnuales$usdTotales)
cuts_df <- data.frame(
  usdTotales,
  predict(dmod, data.frame(usdTotales), se = TRUE)[c("fit", "se.fit")]
)
ggplot(cuts_df) +
  aes(
    x = usdTotales,
    y = fit,
    ymin = fit - se.fit,
    ymax = fit + se.fit,
  ) +
  geom_pointrange() +
  xlim(0,200) + ylim(0,5)

ggplot(df_pagosAnuales) + aes(x = usdTotales,y = n_apariciones)

summary(dmod)
