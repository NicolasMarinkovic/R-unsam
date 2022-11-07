library(tidyverse)
library(sf)

#############################################
### Cargo los datos (el set de arbolado público y el área de las comunas)
df <- read_csv('arbolado-publico-lineal-2017-2018.csv') %>% 
  mutate(comuna = as.factor(comuna))
areas_comunas <- read_csv('area_comunas.csv') %>% mutate(comuna = as.factor(comuna))

# Cuántos Jacarandás hay actualmente en la ciudad
num_jacarandas <- df %>% 
  filter(nombre_cientifico == 'Jacaranda mimosifolia') %>% 
  nrow()

# Diferencia porcentual que representaría plantar 20 árboles más
diferencia <- round(20 / (num_jacarandas + 20), 5)

colnames(df)

#############################################

## Construimos tablas con los tres criterios que consideramos de interés
# La densidad de árboles por comuna, la densidad de jacarandás por comuna,
# y la altura promedio de los jacarandás de una comuna como estimación
# de la altura que van a alcanzar los nuevos jacarandás

arboles_x_comuna <- df %>% 
  group_by(comuna) %>% 
  summarise(n_arboles = n()) %>% 
  arrange(n_arboles) %>% 
  left_join(areas_comunas, by= 'comuna') %>% 
  mutate(arboles_x_km2 = round(n_arboles/area, 2)) %>% 
  select(-area) %>% 
  arrange(arboles_x_km2)

jacarandas_x_comuna <- df %>% 
  group_by(comuna) %>% 
  filter(nombre_cientifico == 'Jacaranda mimosifolia') %>% 
  summarise(n_jacarandas = n()) %>% 
  arrange(n_jacarandas) %>% 
  left_join(areas_comunas, by= 'comuna') %>% 
  mutate(jacarandas_x_km2 = round(n_jacarandas/area, 2)) %>% 
  select(-area) %>% 
  arrange(jacarandas_x_km2)

rm(areas_comunas)  

altura_jac_x_comuna <- df %>% 
  group_by(comuna) %>%
  filter(nombre_cientifico == 'Jacaranda mimosifolia') %>% 
  select(comuna, altura_arbol) %>% 
  mutate(altura_promedio = round(mean(altura_arbol, na.rm=TRUE), 2)) %>% 
  summarise(comuna, altura_promedio) %>% 
  unique() %>% 
  arrange(-altura_promedio) %>% 
  ungroup()

##########################################################

# Asigno puntajes a las comunas en función de los valores obtenidos
# en las tablas anteriores, los sumo para construir un índice que 
# considera equitativamente los tres criterios

puntaje = c(15:1)

arboles_x_comuna <- arboles_x_comuna %>% 
  mutate(num1 = puntaje)

jacarandas_x_comuna <- jacarandas_x_comuna %>% 
  mutate(num2 = puntaje)

altura_jac_x_comuna <- altura_jac_x_comuna %>% 
  mutate(num3 = puntaje)

puntaje_x_comuna <- arboles_x_comuna %>% 
  left_join(jacarandas_x_comuna, by='comuna') %>% 
  left_join(altura_jac_x_comuna, by='comuna') %>% 
  mutate(puntaje = num1 + num2 + num3) %>% 
  select(comuna, puntaje) %>% 
  arrange(-puntaje)

rm(puntaje)

##########################################################
##########################################################
##########################################################

# Filtro el dataset para quedarme con las comunas elegidas
# Dejo de lado las comunas 8 y 4 por las razones planteadas en el informe

comunas_elegidas <- c(8, 2, 15, 3, 4, 9, 1, 13, 6, 7)

comunas_elegidas <- df %>% 
  #filter(comuna %in% comunas_elegidas) %>% 
  drop_na(calle_altura)

#############################################
# Algunas filas no tienen normalizado el valor de la altura de la calle


##            CUIDADO             ##

### >> >> MUY PESADO !!! >> >> ####

 ##                             ##
comunas_elegidas$codigo_calle_corregida <- ''
for (i in 1:nrow(comunas_elegidas)){
  la_altura <- comunas_elegidas[['calle_altura']][[i]] 
  if (nchar(la_altura) > 4 & substring(la_altura, 1, 1) != 1){
    comunas_elegidas[['codigo_calle_corregida']][[i]] <- '0'
  } else if (nchar(la_altura) < 3) {
    comunas_elegidas[['codigo_calle_corregida']][[i]] <- '0'
  } else {
    comunas_elegidas[['codigo_calle_corregida']][[i]] <- substring(la_altura, 1, nchar(la_altura)-2)
  }
}

#############################################

comunas_elegidas_filtradas <- comunas_elegidas %>% 
  filter(ancho_acera > 3.5) %>% 
  group_by(calle_nombre) %>% 
  group_by(codigo_calle_corregida) %>% 
  mutate(arboles_x_cuadra = n()) %>% 
  filter(arboles_x_cuadra < 15) %>% ungroup()

## Al quedarnos con las calles disponibles nos encontramos con 
# que se trata de calles que tienen sus canteros ya ocupados
# o directamente sobreocupados

calles_disponibles <- comunas_elegidas_filtradas %>% 
  select(calle_nombre, calle_altura, comuna, estado_plantera) %>% 
  unique()

write.csv(calles_disponibles, 'calles_disponibles_totales.csv', row.names= FALSE)

################# Curiosidad
### Más allá de los criterios utilizados para considerar el problemas, 
## parece no haber de por si mucha disponibilidad de espacios que cumplan con los requisitos para plantar

tamanio_filtro_general_total <- df %>% 
  filter(ancho_acera > 3.5) %>% 
  group_by(calle_nombre) %>% 
  group_by(calle_altura) %>% 
  mutate(arboles_x_cuadra = n()) %>% 
  filter(arboles_x_cuadra < 15) %>% ungroup() %>% 
  select(calle_nombre, calle_altura, comuna, estado_plantera) %>% 
  unique() %>% 
  nrow()

tamanio_filtro_ajustado_total <- calles_disponibles %>% nrow()

##########################################################
##########################################################
##########################################################

# Enfoque alternativo para buscar canteros vacíos y subocupados

unique(df$estado_plantera)

canteros_disponibles <- df %>% 
  filter(estado_plantera == 'Vacía' | estado_plantera == 'Subocupada')

# Nos encontramos con que no existen canteros con subocupados o vacíos
# con las características necesarias de ancho de acera y árboles por cuadra
# en las comunas en las que queremos plantar.
# Volvemos al plan original a regañadientes

detalle_canteros_disponibles <- canteros_disponibles %>% 
  filter(ancho_acera > 3.5) %>% 
  group_by(calle_nombre) %>% 
  group_by(calle_altura) %>% 
  mutate(arboles_x_cuadra = n()) %>% 
  ungroup() %>% 
  filter(arboles_x_cuadra < 15) %>% 
  filter(comuna %in% comunas_elegidas)

##########################################################
##########################################################
##########################################################

# La geometría de los polígonos para construir el mapa de las comunas
comunas <- st_read("https://bitsandbricks.github.io/data/CABA_comunas.geojson")

##########################################################
################        GRAFICOS          ################
##########################################################

# Mapa de Jacarandás de la Ciudad de Buenos Aires
geo_jacarandas <- df %>% 
  filter(nombre_cientifico == 'Jacaranda mimosifolia') %>% 
  select(long, lat) %>% 
  drop_na() %>% 
  st_as_sf(coords = c('long', 'lat')) %>% 
  st_set_crs(4326) 

ggplot() +
  geom_sf(data = comunas) +
  geom_sf(data = geo_jacarandas, color= '#A683BD', size= .1) +
  geom_sf_text(comunas, mapping= aes(label = comunas), size= 5) +
  labs(title = 'Jacarandás de la Ciudad de Buenos Aires',
       subtitle = 'Arbolado Público Lineal | 2017-2018',
       x = '', y = '') + 
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.border = element_blank()) 

ggsave('mapa_jacarandas.png', device = 'png',
       width = 13, height = 11, units = "cm")


# Gráfico de barras de la cantidad de árboles por comuna
ggplot(arboles_x_comuna) +
  geom_bar(aes(x = reorder(comuna, arboles_x_km2), 
               y = arboles_x_km2, 
               fill = n_arboles), 
           stat='identity') +
  ggtitle(bquote('Árboles por'~km^2~', según comuna')) +
  labs(subtitle = 'CABA | 2017-2018',
       y = '', x= 'Comuna', fill= 'N° de árboles (total)') +
  theme_bw()

ggsave('arboles_x_comuna.png', device = 'png',
       width = 18, height = 9, units = "cm")

# Mapa de la densidad de árboles por comuna

geo_arboles_x_comuna <- comunas %>% left_join(arboles_x_comuna, by= c('comunas'='comuna'))
ggplot() +
  geom_sf(geo_arboles_x_comuna, mapping= aes(fill= -arboles_x_km2)) +
  geom_sf_text(comunas, mapping= aes(label = comunas), size= 5) +
  ggtitle(bquote('Árboles por'~km^2~', según comuna')) +
  labs(fill= '',
       subtitle = 'Arbolado Público Lineal | 2017-2018',
       x = '', y = '') +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.border = element_blank()) 

ggsave('mapa_arboles_x_comuna.png', device = 'png',
       width = 13, height = 11, units = "cm")

# Gráfico de barras de la cantidad de jacarandás por comuna
ggplot(jacarandas_x_comuna) +
  geom_bar(aes(x = reorder(comuna, jacarandas_x_km2), 
               y = jacarandas_x_km2, 
               fill = n_jacarandas), 
           stat='identity') +
  scale_fill_continuous(low= '#EEC9E5', high= '#7C4D79') +
  ggtitle(bquote('Jacarandás por'~km^2~', según comuna')) +
  labs(subtitle = 'CABA | 2017-2018',
       y = '', x= 'Comuna', fill= 'N° de jacarandás (total)') +
  theme_bw()

ggsave('jacarandas_por_comuna.png', device = 'png',
       width = 18, height = 9, units = "cm")

# Mapa de la densidad de jacarandas por comuna
geo_jacarandas_x_comuna <- comunas %>% left_join(jacarandas_x_comuna, by= c('comunas'='comuna'))
ggplot() +
  geom_sf(geo_jacarandas_x_comuna, mapping= aes(fill= jacarandas_x_km2)) +
  geom_sf_text(comunas, mapping= aes(label = comunas), size= 5) +
  scale_fill_continuous(low= '#EEC9E5', high= '#7C4D79') +
  ggtitle(bquote('Jacarandás por'~km^2~', según comuna')) +
  labs(fill= '',
       subtitle = 'Arbolado Público Lineal | 2017-2018',
       x = '', y = '') +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.border = element_blank()) 

ggsave('mapa_jacarandas_x_comuna.png', device = 'png',
       width = 13, height = 11, units = "cm")

# Gráfico de barras de la altura promedio de los jacarandás por comuna
ggplot(altura_jac_x_comuna) +
  geom_bar(aes(x = reorder(comuna, altura_promedio), 
               y = altura_promedio,
               fill = altura_promedio), 
           stat='identity') +
  scale_fill_continuous(low= '#EEC9E5', high= '#7C4D79') +
  labs(title = 'Altura promedio de los jacarandás, según comuna',
       subtitle = 'CABA | 2017-2018',
       y = '', x= 'Comuna', fill= 'Altura promedio (metros)') +
  theme_bw()

ggsave('alt_jac_por_comuna.png', device = 'png',
       width = 18, height = 9, units = "cm")

# Mapa de la densidad de jacarandas por comuna
geo_alt_jac_x_comuna <- comunas %>% left_join(altura_jac_x_comuna, by= c('comunas'='comuna'))
ggplot() +
  geom_sf(geo_alt_jac_x_comuna, mapping= aes(fill= altura_promedio)) +
  geom_sf_text(comunas, mapping= aes(label = comunas), size= 5) +
  scale_fill_continuous(low= '#EEC9E5', high= '#7C4D79') +
  labs(title = 'Altura promedio de los jacarandás, según comuna',
       fill= 'Altura promedio (metros)', x = '', y = '') +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.border = element_blank()) 

ggsave('mapa_alt_jac_x_comuna.png', device = 'png',
       width = 13, height = 11, units = "cm")

# Gráfico de barras del puntaje obtenido por cada comuna en función de los distintos criterios
ggplot(puntaje_x_comuna) +
  geom_bar(aes(x = reorder(comuna, puntaje), 
               y = puntaje,
               fill = puntaje), 
           stat='identity') +
  scale_fill_continuous(low= '#EEC9E5', high= '#7C4D79') +
  labs(title = 'Puntaje calculado para cada comuna',
       y = '', x= 'Comuna', fill= 'Puntaje de selección') +
  theme_bw()

ggsave('puntaje_x_comuna.png', device = 'png',
       width = 18, height = 9, units = "cm")

# Mapa de la densidad de jacarandas por comuna

geo_puntaje_x_comuna <- comunas %>% left_join(puntaje_x_comuna, by= c('comunas'='comuna'))
ggplot() +
  geom_sf(geo_puntaje_x_comuna, mapping= aes(fill= puntaje)) +
  geom_sf_text(comunas, mapping= aes(label = comunas), size= 5) +
  scale_fill_continuous(low= '#EEC9E5', high= '#7C4D79') +
  labs(title = 'Puntaje calculado para cada comuna',
       fill= 'Puntaje de selección', x = '', y = '') +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(panel.border = element_blank()) 

ggsave('mapa_puntaje_x_comuna.png', device = 'png',
       width = 13, height = 11, units = "cm")
