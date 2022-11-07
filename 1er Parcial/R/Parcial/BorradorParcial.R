library(modelr)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggplot2)
library(dplyr)
library(readxl)


setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/R/Parcial')

df <- readxl::read_xlsx('../../data/insurance.xlsx') %>% 
  rename(bmi = names(df)[3])

p0 <- ggplot(df, aes(x=age, y=charges)) + geom_point()
p0

# Preparamos la grilla para hacer los modelos (al menos los más simples)
grid <- data_grid(df, age)


######################################
######################################



# MODELO 1
# Ajusto modelo lineal
mod1 <- lm(charges ~ age, data=df)

# Agrego modelo
grid <- add_predictions(grid, mod1)

# Agrego modelo
p <- p0 + geom_line(data=grid, mapping=aes(y=pred), color='red')
p

summary(mod1)
#En este ejercicio, nos interesa particularmente el valor del parámetro 
#que acompaña a age, porque queremos estudiar exactamente esto; pero no 
#queremos confundirnos por efectos de otras variables.

#Registremos entonces el valor, 257.7±22.5.

#Para estudiar la calidad del modelo podemos ver el desvío de los residuos 
#(Residual standard error), pero también el valor del coeficiente de 
#determinación (R2, R-squared).



######################################
######################################



# MODELO 2
mod2 <- lm(charges ~ age + I(age^2), data=df)

# Agrego modelo
grid <- add_predictions(grid, mod2)

# Agrego modelo
p <- p + geom_line(data=grid, mapping=aes(y=pred), color='green')
p

summary(mod2)
#Podemos ver que los errores de los parámetros se vuelven enormes.
#De ahora en más, seguimos con el modelo lineal en edad.



######################################
######################################




# MODELO 3 - CON OTRAS VARIABLES
mod3 <- lm(charges ~ age + smoker, data=df)
# Escribamos la fórmula para este modelo
# charges=w0+w1∗age+w2∗smoker
# En realidad, el modelo son dos curvas:
#  charges= -> w0+w1∗age          SI NO SOS FUMADOR
#           -> (w0+w2)+w1∗age     SI SOS FUMADOR

summary(mod3)
# Podemos pensar en el parámetro w2 como el recargo en el costo 
# del seguro para los fumadores (que aparece como smokeryes arriba; 
# y que vale más de $ 23,500 !).

# Agrego modelo
grid <- data_grid(df, age, smoker)

# Agrego modelo
grid <- add_predictions(grid, mod3)

# Agrego modelo
p <- ggplot(df, aes(x=age, y=charges, color=smoker)) + 
  geom_point(shape=1) + 
  geom_line(data=grid, mapping=aes(y=pred, color=smoker))
p

# plot(mod3)
df_res <- add_residuals(df, mod3) %>% 
  add_predictions(mod3)

# Grafico residuos
ggplot(df_res, aes(x=pred, y=resid)) + 
  geom_point(aes(color=smoker), shape=1) + 
  geom_hline(yintercept = 0, size=0.25, linetype='dashed') + 
  geom_smooth(aes(x=pred, y=resid), method='loess', size=0.5, se = F, color = 'red')



######################################
######################################




# MODELO 4 CON DOS VARIABLES EXTRAS
# Hack para que compile el Knit: repetir esta línea
df <- read_xlsx('../../data/insurance.xlsx') %>% 
  rename(bmi = names(df)[3])

df <- mutate(df, bmi30 = (bmi > 30))

mod4 <- lm(charges ~ age +  smoker + bmi30, data=df)

summary(mod4)
# Encontramos que el costo extra por BMI alto y 
# el costo extra por ser fumadores son independientes. 
# La penalidad por BMI alto y ser fumador solo puede ser la suma de ambos.


# Agrego modelo
grid <- data_grid(df, age, smoker, bmi30)

# Agrego modelo
grid <- add_predictions(grid, mod4)

# Agrego modelo
p <- ggplot(df, aes(x=age, y=charges, color=smoker, shape=bmi30)) + 
  geom_point() + 
  geom_line(data=grid, mapping=aes(y=pred, color=smoker, linetype=bmi30))
p
# Vemos que el modelo no parece funcionar
# Veamos solo los datos de un grupo

dd <- filter(df, (smoker=='yes') & (bmi30))
ggplot(dd, aes(x=age, y=charges)) + geom_point()
# Explicacion en el pdf.





######################################
######################################






# MODELO 5 CON INTERACCIONES
mod5 <- lm(charges ~ age + smoker * bmi30, data=df)

summary(mod5)
# Vemos que ahora el término que representa el exceso de costo solo por 
# BMI alto no es significativo y podríamos sacarlo del modelo.
# Además, vean cómo mejoró la determinación del coeficiente que 
# acompaña a age (recuerden, es nuestro objeto interés!), 
# y cómo cambió el RSE y el  R2

# Agrego modelo
grid <- data_grid(df, age, smoker, bmi30)

# Agrego modelo
grid <- add_predictions(grid, mod5)

# Agrego modelo
p <- ggplot(df, aes(x=age, y=charges, color=smoker, shape=bmi30)) + 
  geom_point() + 
  geom_line(data=grid, mapping=aes(y=pred, color=smoker, linetype=bmi30))
p

# plot(mod5)

df_res <- add_residuals(df, mod5) %>% 
  add_predictions(mod5)

# Grafico residuos
ggplot(df_res, aes(x=pred, y=resid)) + 
  geom_point(aes(color=smoker), shape=1) + 
  geom_hline(yintercept = 0, size=0.25, linetype='dashed') + 
  geom_smooth(aes(x=pred, y=resid), method='loess', size=0.5, se = F, color = 'red')
# Vemos que los residuos están mucho mejor, aunque hay varios puntos outliers 
# que habría que mirar con más detalle.




############################
############################




# MODELO FINAL 
df <- mutate(df, bmi30smoker = (bmi > 30) & (smoker=='yes'))

mod6 <- lm(charges ~ age + smoker + bmi30smoker, data = df)

summary(mod6)
# Acá vemos que todos los parámetros son significativos, 
# y que tenemos el menor error el estimador del coeficiente que acompaña a age. 
# Además, redujimos la dispersión de los residuos a ~$4000; 
# esto está dominado por los puntos de personas no 
# fumadoras que por alguna razón pagan mucho seguro de salud. 


# Agrego modelo
grid <- data_grid(df, age, smoker, bmi30smoker)

# Agrego modelo
grid <- add_predictions(grid, mod6)

# Agrego modelo
p <- ggplot(df, aes(x=age, y=charges, color=smoker, shape=bmi30smoker)) + 
  geom_point() + 
  geom_line(data=grid, mapping=aes(y=pred, color=smoker, linetype=bmi30smoker))
p