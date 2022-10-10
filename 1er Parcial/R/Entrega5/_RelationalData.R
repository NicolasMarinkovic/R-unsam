library(tidyverse)
library(ggplot2)
library(ggridges)

setwd('C:/Users/nicol/OneDrive/Escritorio/Facultad/Introducccion a Ciencia de Datos/R-unsam/1er Parcial/data')

appearances <- read.csv('appearances.csv', dec=',')
games <- read.csv('games.csv', dec=',')
leagues <- read.csv('leagues.csv', dec=',')
players <- read.csv('players.csv', dec=',')
teamstats <- read.csv('teamstats.csv', dec=',')
shots <- read.csv('shots.csv', dec=',')
teams <- read.csv('teams.csv', dec=',')

######################################################
# ANALIZAMOS LOS GOLES TOTALTES
totalGolesXTeamID <- teamstats %>%
  group_by(teamID) %>%
  summarise( totalGoles= sum(goals, na.rm=TRUE))

maximosEquiposGoleadores <- totalGolesXTeamID %>%
  filter( totalGoles > 498) 

maximosEquiposGoleadores <- maximosEquiposGoleadores %>%
  inner_join(teams, by="teamID")

maximosEquiposGoleadores <- maximosEquiposGoleadores[order(-maximosEquiposGoleadores$totalGoles),]
######################################################

######################################################
# ANALIZAMOS LOS TIROS(shots) TOTALTES
totalTirosXTeamID <- teamstats %>%
  group_by(gameID) %>%
  summarise(teamID=teamID,totalTiros = shots) %>%
  arrange(desc(totalTiros))

totalTirosXTeamID[order(-totalTirosXTeamID$totalTiros),]
######################################################

######################################################
# JUNTO LOS EQUIPOS GOLEADORES CON SU CANTIDAD DE TIROS
gytXTeam <- maximosEquiposGoleadores %>%
  left_join(totalTirosXTeamID, by="teamID")

######################################################

######################################################
# SELECCIONO LOS 5 JUGADORES CON MAS GOLES EN LA PREMIER
maximosGoleadores <- appearances %>%
  group_by(playerID, leagueID) %>%
  summarise( totalGoles= sum(goals, na.rm=TRUE)) %>%
  filter( leagueID == 1 & totalGoles > 93) %>%
  arrange(desc(totalGoles))

maximosGoleadores <- maximosGoleadores %>%
  inner_join(players, by="playerID")
######################################################

######################################################
# DISTRIBUCION DE TIEMPOS
tiempoDistribuido <- shots %>%
  filter(shooterID %in% maximosGoleadores$playerID) %>%
  mutate(playerID = shooterID) %>%
  filter( shotResult == 'Goal') %>%
  arrange(shooterID)

tiempoDistribuido <- tiempoDistribuido %>%
  inner_join(maximosGoleadores, by="playerID")

#####
# Este for me ayudo a contar la cantidad de filas de un mismo nombre
for (x in unique(gytXTeam$name)) {
       print(nrow(subset(gytXTeam,gytXTeam$name == x)))
       print(x)
     }
#####


######################################################
######################################################
### GRAFICOS
maximosEquiposGoleadores %>%
  mutate(name = fct_reorder(name, totalGoles)) %>%
  ggplot(mapping=aes(x=name, y=totalGoles, fill=name)) + 
  geom_bar(stat='identity' ) +
  labs(title='Máximos goles x Equipo',
       subtitle='Temporadas 2014 - 2020',
       y = 'Total de goles x equipo',
       fill = "Color del equipo") +
      theme(
        plot.title = element_text(color="red", face="bold.italic"),
        plot.subtitle = element_text(color="red", face="bold.italic"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color="#993333", face="bold"))

######################################################

ggplot(gytXTeam, aes(x=totalGoles, y=totalTiros, color=name)) + 
  geom_point() +
  labs(title='Diferencias de tiros y goles x Equipo',
       subtitle='Temporadas 2014 - 2020',
       y = 'Cantidad de Tiros x Partido',
       x = 'Cantidad de Goles',
       color = "Color del equipo") +
  theme(
    plot.title = element_text(color="#993399", face="bold.italic"),
    plot.subtitle = element_text(color="#993399", face="bold.italic"),
    axis.title.x = element_text(color="black", face="bold"),
    axis.title.y = element_text(color="black", face="bold"))

######################################################

ggplot(tiempoDistribuido, aes(minute, name, fill = totalGoles)) +
  geom_density_ridges(rel_min_height=.01) +
  labs(title='Densidad de goles x minuto para los mayor goleadores de la Premier League',
       subtitle='Temporadas 2014 - 2020',
       y = 'Jugador',
       x = 'Minuto',
       fill = "Total de goles") +
  theme(
    plot.title = element_text(color="darkblue", face="bold.italic"),
    plot.subtitle = element_text(color="darkblue", face="bold.italic"),
    axis.title.x = element_text(color="black", face="bold"),
    axis.title.y = element_text(color="black", face="bold"))


