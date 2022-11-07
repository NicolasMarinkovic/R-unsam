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

grid <- data_grid(df_pagosAnuales, usdPromedio)

# Ajusto modelo lineal
mod <- lm(Pay ~ usdPromedio, data=df_pagosAnuales)
# Agrego modelo
grid <- add_predictions(grid, mod)

# Agrego modelo
summary(mod)

p0 <- ggplot(df_pagosAnuales, aes(y=Pay, x=usdPromedio,
                                  color= df_pagosAnuales$n_apariciones)) + 
  geom_point() +
  geom_line(aes(x=median(usdPromedio)),linetype = "dotdash",
            color='black')
p <- p0 + geom_line(data=grid, mapping=aes(x=usdPromedio,y=pred), color='red')
p <- p + labs(title = 'Aproximaciones de sueldos',
              subtitle = 'Sueldos entre 2005 y 2020', 
              x = 'Pago promedio por pelicula',
              y = 'Pago anual',
              color = "Cantidad de peliculas",
              caption = "Los promedios son en Millones USD") +
  theme(
    plot.title = element_text(family = 'sans', color='Black',face="bold"),
    plot.subtitle = element_text(family = 'sans', color='Black'),
    axis.title.x = element_text(family = 'serif', color='Black',face="bold"),
    axis.title.y = element_text(family = 'serif', color='Black',face="bold"),
    plot.caption = element_text(family = 'serif', color = '#757575',size = 15),
    axis.line = element_line(color = "black", size = 0.5, linetype = 1),
    axis.text.x = element_blank(), #esto lo deja en blanco al eje
    axis.ticks.x = element_blank() #esto lo deja en blanco al eje
  )

p + ylim(0,100) + xlim(0,100) #+ scale_x_continuous(labels = 40)

ggsave('sueldos.png', device = 'png',
       width = 18, height = 9, units = "cm")
