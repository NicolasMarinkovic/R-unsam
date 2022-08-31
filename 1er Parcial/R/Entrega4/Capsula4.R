library(tidyverse)
library(ggridges)

setwd('C:/Facultad/Unsam/Licenciatura en ciencia de Datos/Introduccion a Ciencia de Datos/1er Parcial/R/Entrega4')

#############################################

df = read_csv('arbolado-publico-lineal-2017-2018.csv')
colnames(df)
areas = read_csv('area_comunas.csv')

#############################################

arboles_x_comuna <- df %>% 
  group_by(comuna) %>% 
  summarise(n = n()) %>% 
  arrange(n)

jacarandas_x_comuna <- df %>% 
  group_by(comuna) %>% 
  filter(nombre_cientifico == 'Jacaranda mimosifolia') %>% 
  summarise(n = n()) %>% 
  arrange(n)


#############################################

indice_arboles_x_comuna = arboles_x_comuna %>% 
  left_join(areas, by='comuna') %>% 
  mutate(arboles_x_km2 = n/Área) %>% 
  select(comuna, arboles_x_km2) %>% 
  arrange(arboles_x_km2)

indice_jacarandas_x_comuna = jacarandas_x_comuna %>% 
  left_join(areas, by='comuna') %>% 
  mutate(jacarandas_x_km2 = n/Área) %>% 
  select(comuna, jacarandas_x_km2) %>% 
  arrange(jacarandas_x_km2)

densidad_arboles_x_comuna <- indice_arboles_x_comuna %>% 
  left_join(indice_jacarandas_x_comuna, by='comuna')


##########################################################

densidad_normalizada <- densidad_arboles_x_comuna %>% 
  mutate(arb_x_com_norm = arboles_x_km2/mean(arboles_x_km2),
         jac_x_com_norm = jacarandas_x_km2/mean(jacarandas_x_km2)) %>% 
  mutate(densidad_compleja = (arb_x_com_norm + jac_x_com_norm) / 2) %>% 
  arrange(densidad_compleja)

##########################################################

altura_jac_x_comuna <- df %>% 
  group_by(comuna) %>%
  filter(nombre_cientifico == 'Jacaranda mimosifolia') %>% 
  select(comuna, altura_arbol) %>% 
  mutate(altura_promedio = mean(altura_arbol, na.rm=TRUE)) %>% 
  summarise(comuna, altura_promedio) %>% 
  unique() %>% 
  arrange(-altura_promedio) %>% 
  ungroup()

##########################################################

orden = list(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)

indice_arboles_x_comuna <- indice_arboles_x_comuna %>% 
  mutate(num1 = as.numeric(orden))

indice_jacarandas_x_comuna <- indice_jacarandas_x_comuna %>% 
  mutate(num2 = as.numeric(orden))

altura_jac_x_comuna <- altura_jac_x_comuna %>% 
  mutate(num3 = as.numeric(orden))

total <- indice_arboles_x_comuna %>% 
  left_join(indice_jacarandas_x_comuna, by='comuna') %>% 
  left_join(altura_jac_x_comuna, by='comuna') %>% 
  mutate(definitivo = num1 + num2 + num3) %>% 
  select(comuna, definitivo) %>% 
  arrange(-definitivo)

##########################################################
##########################################################
##########################################################

comunas_elegidas <- head(total$comuna, 5)

comunas_elegidas <- df %>% 
  filter(comuna %in% comunas_elegidas)


##########################################################
##########################################################
##########################################################

comunas_elegidas_filtradas <- comunas_elegidas %>% 
  filter(ancho_acera > 3.5) %>% 
  group_by(calle_nombre) %>% 
  group_by(calle_altura) %>% 
  mutate(arboles_x_cuadra = n()) %>% 
  filter(arboles_x_cuadra < 15)

calles_disponibles <- comunas_elegidas_filtradas %>% 
  select(calle_nombre, calle_altura, comuna) %>% 
  unique()


##########################################################
################        GRAFICOS          ################
##########################################################

ggplot(arboles_por_comuna) + geom_col(aes(comuna,y = narboles),
                                      fill='darkblue') +
  scale_x_continuous(breaks = round(seq(min(arboles_por_comuna$comuna), max(arboles_por_comuna$comuna), by = 1),1))

ggplot(arboles_por_comuna) + geom_col(aes(comuna,y = total_altura), fill='darkblue') +
  scale_x_continuous(breaks = round(seq(min(arboles_por_comuna$comuna), max(arboles_por_comuna$comuna), by = 1),1))
