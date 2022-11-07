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
dfmod1 <- df %>%
  filter(arr_delay > 60) %>%
  filter(carrier == 'B6')


grid <- data_grid(dfmod1, arr_delay)

# Ajusto modelo lineal
mod2 <- lm(dep_delay ~ arr_delay, data=dfmod1)
# Agrego modelo
grid <- add_predictions(grid, mod2)

# Agrego modelo
summary(mod2)

p0 <- ggplot(dfmod1, aes(y=dep_delay, x=arr_delay)) + geom_point()
p <- p0 + geom_line(data=grid, mapping=aes(x=arr_delay,y=pred), color='red')
p <- p + labs(title = 'Modelo lineal de restraso de salida sobre retraso de llegada',
              subtitle = 'Aerolínea B6', 
            x = 'Retraso de salida',
            y = 'Retraso de llegada',
            color = "Aeropuerto") +
  theme(
    plot.title = element_text(color='Black',face="bold.italic"),
    axis.title.x = element_text(color='Black',face="bold"),
    axis.title.y = element_text(color='Black',face="bold"))
p + xlim(50,500)

