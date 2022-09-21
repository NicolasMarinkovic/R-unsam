library(tidyverse) 
#########################################
# Documentación de cambios y decisiones
#
#'''
#    En la importación se ignoraron 733 filas con número de manzana.
#    El atributo ‘nivel_plantera’ tenía datos mal capitalizados, se corrigieron. No se hizo nada con datos NA.
#    El atributo ‘diametro_altura_pecho’ tenia valores cero, que son incorrectos en el contexto del problema. Se cambiaron a NA.
#    Los valores NA de ‘diametro_altura_pecho’ y ‘altura_arbol’ se reemplazaron con los valores medios por especie.
#'''

#                         IMPORTACIÓN
# Cambia el separador decimal por un punto (bmi, charges)
df_seguros <- read.csv('~/Descargas/Insurance.csv', dec=',')
# Si quisieramos cambiar el tipo de datos de alguna columna podemos usar as.factor(), as.numeric() o as.character().
df_seguros$smoker = as.character(df_seguros$smoker)
# Muestra los tipos de datos de cada columna
glimpse(df_seguros)

#########################################

df_arbol = read_csv('~/Descargas/arbolado_comuna14.csv') # Tira Warnings
# Muestra donde y cual es el problema
problems(df_arbol)
glimpse(df_arbol)

#########################################
#########################################
#########################################
#                         DATOS REPETIDOS

# n_distinc() , simplemente nos dice cuantas filas distintas hay.
df_seguros %>% 
  summarise(distintos=n_distinct(df_seguros), total=n())
# De las filas duplicadas se va a quedar solo con la primera aparición y va a descartar el resto.
df_seguros <- unique(df_seguros)

unique(df_arbol$estado_plantera)
unique(df_arbol$nivel_plantera) # Descubrimos que habia capitalizaciones incorrectas

# Podemos pasar todo a minúsculas 
df_arbol$nivel_plantera <- str_to_lower(df_arbol$nivel_plantera)

# O reemplazar para un mejor formato
df_arbol <- df_arbol %>%
  mutate(nivel_plantera =                           
           replace(nivel_plantera,
                   nivel_plantera=="a nivel",
                   "A nivel"))

df_arbol <- df_arbol %>%
  mutate(nivel_plantera =                           
           replace(nivel_plantera,
                   nivel_plantera=="bajo nivel",
                   "Bajo nivel"))

#########################################
#########################################
#########################################
#         IRREGULARIDADES EN LOS DATOS Y DATOS FALTANTES

# Hacemos un resumen usando is.na para detectar los valores faltantes y sum para sumarlos.
df_arbol %>% summarise(cant_arboles = n(),
                       na_altura = sum(is.na(altura_arbol)),
                       na_diametro = sum(is.na(diametro_altura_pecho))
)
# Podemos hacer un par de histogramas rápidos para visualizar la informaciòn.
ggplot(data = df_arbol,mapping = aes(x=altura_arbol)) + geom_histogram() + 
  labs(title="Histograma de alturas") # Habria que refinarlo un poco más y ver esos valores extremos

ggplot(data = df_arbol,mapping = aes(x=diametro_altura_pecho)) + geom_histogram() + 
  labs(title="Histograma de diámetros") # En los díametros hay unos valores cercanos a cero.

# Diámetros cero no tiene sentido porque están medidos en centímetros
df_arbol %>%
  filter(diametro_altura_pecho==0) %>%
  summarize(ceros_diametro = n())

# Decidimos reemplazar estos valores por NA.
df_arbol <- df_arbol %>%
  mutate(diametro_altura_pecho
         = replace(diametro_altura_pecho,
                   diametro_altura_pecho==0,
                   NA))

# Reemplacemos esos valores faltantes por el valor medio y agrupando por especie
df_arbol <- df_arbol %>%
  group_by(nombre_cientifico) %>%
  mutate(diametro_altura_pecho
         = replace(diametro_altura_pecho,
                   is.na(diametro_altura_pecho),
                   median(diametro_altura_pecho, na.rm = TRUE))) %>%
  ungroup()

# otra opción es hacerlo sobre el mismo dataframe indexando con los []
df_arbol$diametro_altura_pecho[is.na(df_arbol$diametro_altura_pecho)] <- mean(df_arbol$diametro_altura_pecho, na.rm=T)

#En el Wickham hay una opción muy cómoda usando ifelse que quedaria así
df_arbol <- df_arbol%>%
  mutate(diametro_altura_pecho = 
           ifelse(diametro_altura_pecho==0, NA,diametro_altura_pecho))

# Usemos el mismo criterio para la altura.
df_arbol <- df_arbol %>%
  group_by(nombre_cientifico) %>%
  mutate(altura_arbol
         = replace(altura_arbol,
                   is.na(altura_arbol),
                   median(altura_arbol, na.rm = TRUE))) %>%
  ungroup()

