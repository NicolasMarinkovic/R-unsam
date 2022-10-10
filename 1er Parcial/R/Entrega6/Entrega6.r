#Entrega 6 para la Unsam 10/10/2022. Por Nicolás Marinkovic

library(modelr)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggplot2)
library(dplyr)

setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/Entrega6')

#############################################

df = read_csv('../../data/arbolado-publico-lineal-2017-2018.csv')
colnames(df)
areas = read_csv('../../data/area_comunas.csv')

#############################################
# Limpiamos los datos. 

# Diámetros cero no tiene sentido porque están medidos en centímetros
df %>%
  filter(diametro_altura_pecho==0) %>%
  summarize(ceros_diametro = n())

# Decidimos reemplazar estos valores por NA.
df <- df %>%
  mutate(diametro_altura_pecho
         = replace(diametro_altura_pecho,
                   diametro_altura_pecho==0,
                   NA))

# Reemplacemos esos valores faltantes por el valor medio y agrupando por especie
df <- df %>%
  group_by(nombre_cientifico) %>%
  mutate(diametro_altura_pecho
         = replace(diametro_altura_pecho,
                   is.na(diametro_altura_pecho),
                   median(diametro_altura_pecho, na.rm = TRUE))) %>%
  ungroup()

# Usemos el mismo criterio para la altura.
df <- df %>%
  group_by(nombre_cientifico) %>%
  mutate(altura_arbol
         = replace(altura_arbol,
                   is.na(altura_arbol),
                   median(altura_arbol, na.rm = TRUE))) %>%
  ungroup()

#############################################

mod <- lm(altura_arbol ~ log(diametro_altura_pecho), data=df)

grid <- data_grid(df, diametro_altura_pecho)

grid <- add_predictions(grid, mod)

grid <- sim1 %>% add_residuals(mod)

summary(mod)

ggplot(data=df) + 
  geom_point(aes(diametro_altura_pecho,altura_arbol,color=altura_arbol)) +
  geom_line(data=grid, aes(diametro_altura_pecho,pred)) +
  labs(title="Modelo lineal ajustado relacionando Ancho sobre Altura de un Árbol",
       subtitle="Con línea de predicciones",
       y="Altura del Árbol",
       x="Ancho del Árbol") +
  ylim(0,45)

ggplot(df, aes(x=diametro_altura_pecho, y=altura_arbol) ) +
  geom_violin(fill="DarkBlue") +
  stat_summary(fun = "mean",geom = "crossbar", width = 0.5,colour = "red") +
  geom_line(data=grid, aes(diametro_altura_pecho,pred)) +
  labs(title="Amplitud del modelo ajustado con el promedio de ancho por altura",
       subtitle="Con línea de predicciones",
       y="Altura del Árbol",
       x="Ancho del Árbol") +
  ylim(0,40)
