#Setting directory

setwd("C:/Users/daily/Desktop/Baseball-America-Position-Analysis/R")

#Loading libraries

library(tidyr)
library(ggplot2)
library(dplyr)

#Grabbing csv files

Pitchers <- read.csv(file = "C:/Users/daily/Desktop/Baseball-America-Position-Analysis/Csv/Pitchers.csv")
PositionPlayers <- read.csv(file = "C:/Users/daily/Desktop/Baseball-America-Position-Analysis/Csv/Position Players.csv")
Catchers <- read.csv(file = "C:/Users/daily/Desktop/Baseball-America-Position-Analysis/Csv/Catchers.csv")
CornerIF <- read.csv(file = "C:/Users/daily/Desktop/Baseball-America-Position-Analysis/Csv/Corner IF.csv")
MiddleIF <- read.csv(file = "C:/Users/daily/Desktop/Baseball-America-Position-Analysis/Csv/Middle IF.csv")
OF <- read.csv(file = "C:/Users/daily/Desktop/Baseball-America-Position-Analysis/Csv/Outfielders.csv")

#Replacing null values with 0

Pitchers[is.na(Pitchers)] = 0
PositionPlayers[is.na(PositionPlayers)] = 0
Catchers[is.na(Catchers)] = 0
CornerIF[is.na(CornerIF)] = 0
MiddleIF[is.na(MiddleIF)] = 0
OF[is.na(OF)] = 0


#Calculating the total WAR accumulated by a player over six seasons

Pitchers$TotalWAR <- Pitchers$WARYearOne + Pitchers$WARYearTwo + Pitchers$WARYearThree +
Pitchers$WARYearFour + Pitchers$WARYearFive + Pitchers$WARYearSix

PositionPlayers$TotalWAR <- PositionPlayers$WARYearOne + PositionPlayers$WARYearTwo + PositionPlayers$WARYearThree +
PositionPlayers$WARYearFour + PositionPlayers$WARYearFive + PositionPlayers$WARYearSix

Catchers$TotalWAR <- Catchers$WARYearOne + Catchers$WARYearTwo + Catchers$WARYearThree +
Catchers$WARYearFour + Catchers$WARYearFive + Catchers$WARYearSix

CornerIF$TotalWAR <- CornerIF$WARYearOne + CornerIF$WARYearTwo + CornerIF$WARYearThree +
CornerIF$WARYearFour + CornerIF$WARYearFive + CornerIF$WARYearSix

MiddleIF$TotalWAR <- MiddleIF$WARYearOne + MiddleIF$WARYearTwo + MiddleIF$WARYearThree +
MiddleIF$WARYearFour + MiddleIF$WARYearFive + MiddleIF$WARYearSix

OF$TotalWAR <- OF$WARYearOne + OF$WARYearTwo + OF$WARYearThree +
OF$WARYearFour + OF$WARYearFive + OF$WARYearSix

#Adding position column to data

Pitchers <- Pitchers %>%
mutate(Position = "Pitcher")

PositionPlayers <- PositionPlayers %>%
mutate(Position = "Position Player")

Catchers <- Catchers %>%
mutate(Position = "Catcher")

CornerIF <- CornerIF %>%
mutate(Position = "Corner IF")

MiddleIF <- MiddleIF %>%
mutate(Position = "Middle IF")

OF <- OF %>%
mutate(Position = "Outfielder")

#Combining position player and pitcher data

PitchersAndPositionPlayers <- rbind(Pitchers, PositionPlayers)

PitchersAndPositionPlayerBreakdown <- rbind(Pitchers, Catchers, CornerIF, MiddleIF, OF)

#Create a column for table categories

PitchersAndPositionPlayers <- mutate(PitchersAndPositionPlayers, WARGroup = ifelse(TotalWAR >= 20, "20+",
ifelse(TotalWAR >= 15, "15-19.9",
ifelse(TotalWAR >= 10, "10-14.9",
ifelse(TotalWAR >= 5, "5-9.9",
ifelse(TotalWAR > 0, "0.1-4.9", "0 or less"))))))

PitchersAndPositionPlayerBreakdown <- mutate(PitchersAndPositionPlayerBreakdown, WARGroup = ifelse(TotalWAR >= 20, "20+",
ifelse(TotalWAR >= 15, "15-19.9",
ifelse(TotalWAR >= 10, "10-14.9",
ifelse(TotalWAR >= 5, "5-9.9",
ifelse(TotalWAR > 0, "0.1-4.9", "0 or less"))))))


#Loading function for crosstab

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

#Crosstab function

crosstab(PitchersAndPositionPlayers, row.vars = "Position", col.vars = "WARGroup", type = "r")

crosstab(PitchersAndPositionPlayers, row.vars = "Position", col.vars = "WARGroup", type = "f")

crosstab(PitchersAndPositionPlayerBreakdown, row.vars = "Position", col.vars = "WARGroup", type = "r")

crosstab(PitchersAndPositionPlayerBreakdown, row.vars = "Position", col.vars = "WARGroup", type = "f")
