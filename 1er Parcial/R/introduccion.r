library(tidyverse)
library(forcats)

# Leo la tabla
df <- read.csv('C:\Users\nicol\OneDrive\Escritorio\Facultad\Introducccion a Ciencia de Datos\Primer Parcial/flights_september.csv')

# Hago un gráfico básico
ggplot(data=df)+ geom_bar(mapping=aes(x=fct_rev(fct_infreq(origin))),
                          fill = 'Blue', width=0.5)

# Cambio el orden de origin antes de graficar
df_m <- mutate(df, origin=fct_rev(fct_infreq(origin)))

# Grafico 
ggplot(data=df) + geom_bar(aes(x=origin),fill = 'Red', width=0.5)

# Filtrar
df2 <- filter(df_m, dep_delay > 60)
ggplot(data=df2) + geom_bar(aes(x=origin),fill = 'Red', width=0.5)

# Agrupar
df_ma <- group_by(df_m, origin)
df3 = summarise(df_ma, nvuelos=n(), nretra =sum(dep_delay>60, na.rm=TRUE),
          frac_retra=nretra/nvuelos * 100)
ggplot(data=df2) + geom_bar(aes(x=origin, y=frac_retra),fill = 'Red', width=0.5,
                            stat = 'identity')
