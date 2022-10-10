library(modelr)
library(tidyverse)
library(ggridges)
library(cowplot)
library(ggplot2)
library(dplyr)
 
mod <- lm(y ~ x, data=sim1)

grid <- data_grid(sim1, x)

grid <- add_predictions(grid, mod)

grid <- sim1 %>% add_residuals(mod)

summary(mod)


ggplot(data=sim1) + 
  geom_point(aes(x,y)) +
  geom_line(data=grid, aes(x,pred)) +
  labs(title="Modelo lineal ajustado relacionando y sobre x",
       subtitle="Con línea de predicciones")

ggplot(data=sim1) + 
  geom_point(data=grid, aes(y,resid)) +
  labs(title="Residuos en función de Y")

ggplot(data=sim1) + 
  geom_point(data=grid, aes(x,resid)) +
  labs(title="Residuos en función de X")

plot(rnorm(30, mean = 15.50425, sd = 6.372218), main = "n = 30",
     xlab = "", prob = TRUE)

#################################################################
# PArte 2
ggplot(sim3) + geom_point(aes(x1,y, color= x2),size=1.5) +
  labs(title="Dataset Sim3")

mod1 <- lm(y ~ x1 + x2,sim3)
mod2 <- lm(y ~ x1 * x2,sim3)

model_matrix(sim3 , mod1)
model_matrix(sim3 , mod2)
# GRID 1
grid1 <- data_grid(sim3, x1)

grid1 <- add_predictions(sim3, mod1)


grid1 <- sim3 %>% add_residuals(mod1)

gra1 <- ggplot(data=sim3) + 
  geom_point(aes(x1,y,color=x2),size=2) +
  geom_line(data=grid1, aes(x1,pred)) +
  labs(title="Modelo lineal ajustado usando y ~ x1 + x2",
       subtitle="Con línea de predicciones")

# GRID 2
grid2 <- data_grid(sim3, x1)

grid2 <- add_predictions(sim3, mod2)

grid2 <- sim3 %>% add_residuals(mod2)

gra2 <- ggplot(data=sim3) + 
  geom_point(aes(x1,y,color=x2),size=2) +
  geom_line(data=grid2, aes(x1,pred)) +
  labs(title="Modelo lineal ajustado usando y ~ x1 * x2",
       subtitle="Con línea de predicciones")


plot_grid(gra1,gra2)




################################################################################
#Tarea 3

ggplot(data=diamonds) + geom_point(aes(carat,price,color=cut))+
labs(title="Carat en función de price segun el tipo de cut",
     subtitle="Dataset de diamonds")
mod <- lm(log2(price) ~ log2(carat), data=diamonds)

grid <- data_grid(diamonds, log2(carat),carat)

grid <- add_predictions(grid, mod)

grid <- diamonds %>% add_residuals(mod)

ggplot(data=diamonds) + 
  geom_point(aes(log2(carat),log2(price),color=cut)) +
  geom_line(data=grid, aes(carat-2,pred))+
  labs(title="Carat en función de price segun el tipo de cut",
       subtitle="Predicción de la linea de tendencia")

dia1<-ggplot(data=diamonds) + 
  geom_point(data=grid, aes(price,resid,color=cut)) +
  labs(title="Residuos en función de price")

dia2<-ggplot(data=diamonds) + 
  geom_point(data=grid, aes(carat,resid,color=cut)) +
  labs(title="Residuos en función de carat")
plot_grid(dia1,dia2)

################################################################################
#Tarea 4
colores <- ggplot(diamonds, aes(x=color, y=price, color=color) ) + geom_col()
corte <- ggplot(diamonds, aes(x=cut, y=price, color=cut) ) + geom_col()
claridad <- ggplot(diamonds, aes(x=clarity, y=price, color=clarity) ) + geom_col()

plot_grid(colores,corte,claridad)

colores <- ggplot(diamonds, aes(x=color, y=price, color=color) ) + geom_violin() +stat_summary(fun = "mean",
                                                                                              geom = "crossbar", 
                                                                                              width = 0.5,
                                                                                              colour = "red")
corte <- ggplot(diamonds, aes(x=cut, y=price, color=cut) ) + geom_violin()+ stat_summary(fun = "mean",
                                                                                        geom = "crossbar", 
                                                                                        width = 0.5,
                                                                                        colour = "red")
ggplot(diamonds, aes(x=clarity, y=cut, color=clarity) ) + geom_point()
plot_grid(colores,corte,claridad)

diamantes <- group_by(diamonds,cut)
diamantes$cuts <- as.integer(as.factor(diamantes$cut))
mapping <- c("Fair"=1, "Good"=2, "Very Good"= 3,"Premium"=4,"Ideal"= 5)


mod <- lm(cuts ~ clarity, data=diamantes)

grid <- data_grid(diamantes, clarity)

grid <- add_predictions(grid, mod)

grid <- diamonds %>% add_residuals(mod)

ggplot(data=diamante) + 
  geom_col(aes(clarity,cuts,color=cuts))

