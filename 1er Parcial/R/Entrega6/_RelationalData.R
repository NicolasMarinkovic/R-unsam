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
  filter( totalGoles > 498) %>%
  arrange(desc(totalGoles))

maximosEquiposGoleadores <- maximosEquiposGoleadores %>%
  inner_join(teams, by="teamID")

maximosEquiposGoleadores[order(-maximosEquiposGoleadores$totalGoles),]
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

######################################################
######################################################
### GRAFICOS

ggplot(maximosEquiposGoleadores) + geom_bar(aes(x=as.character(teamID), y=totalGoles, fill=name)
                                     ,stat='identity', )

plot(gytXTeam$totalTiros, gytXTeam$totalGoles, col="blue")

ggplot(tiempoDistribuido, aes(minute, as.character(shooterID), fill = name)) +
  geom_density_ridges(rel_min_height=.01)


