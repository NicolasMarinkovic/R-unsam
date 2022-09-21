library(tidyverse)
library(ggplot2)
library(ggridges)
library(modelr)

setwd('/home/datascience/Descargas/archive')
summaryWeather <- read.csv('Summary of Weather.csv')
weatherStation <- read.csv('Weather Station Locations.csv')

summaryWeather <- subset(summaryWeather, select = -c(WindGustSpd,DR,SPD,SND,FT,
                                                     FB,FTI,ITH,PGT,SD3,RHX,RHN,RVG,
                                                     WTE,TSHDSBRSGF))

colnames(summaryWeather)[1] <-'WBAN'

weather <- summaryWeather %>%
  inner_join(weatherStation, by='WBAN')

weather <- weather %>%
  filter(WBAN == 34113)

plot(weather$MeanTemp)
plot(weather$MinTemp,weather$MaxTemp)

mod <- lm(MaxTemp~MinTemp, data=weather)

summary(mod)

ggplot(weather, aes(MinTemp, MaxTemp,color=YR)) +
  geom_point()