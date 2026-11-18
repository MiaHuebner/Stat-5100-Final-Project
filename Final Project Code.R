#2024-2025 season data
library(corrplot)
library(tidyverse)
library(MASS)
library(car)
library(ggplot2)
library(dplyr)

rm(list = ls()) # keep the environment clean and predictable between runs
options(digits = 4, scipen = 999) # make printed numbers and plots a bit nicer
set.seed(123) # set the seed for reproducibility

train_season <- read.csv("2024-25-player-season-totals.csv")
train_season <- train_season |>
  mutate(turnover_pg = turnovers/games_played, minutes_pg = minutes/games_played, 
         field_goal_attempts_pg = field_goal_attempts/games_played)

train_season <- train_season |> 
  mutate(three_point_attempt_rate = three_point_attempts/field_goal_attempts)

#data exploration
str(train_season)
nrow(train_season)
    #569 rows

#Three Point Attempt Rate
hist(train_season$three_point_attempt_rate, main= "Histogram of Three Point Attempt Rate",
     xlab= "Three Point Attempt Rate",
     col= "#463")

#Average points per game
hist(train_season$points_pg, main= "Histogram of Points Per Game",
     xlab="Points Per Game",
     col= "lavender")
#age
hist(train_season$age, main="Histogram of Age",
     xlab= "Age",
     col="yellow")

#Minutes per game
hist(train_Per_game$minutes_pg, main="Histogram of Minutes Played Per Game", 
     xlab= "Minutes Per Game",
     col = "tan")
str(train_Per_game)

#Field Goal Attemps Per Game 
hist(train_Per_game$field_goal_attempts_pg, main="Histogram of Field Goal Attempts Per Game",
     xlab="Field Goal Attempts Per Game",
     col="red")

#Games played
hist(train_season$games_played, main="Histogram of Games Played",
     xlab= "Games Played",
     col = "orange")

#Assists
hist(train_season$assists, main="Histogram of Assists",
     xlab= "Games Played",
     col = "green")

#Rebounds
hist(train_season$rebounds, main="Histogram of Rebounds",
     xlab="Rebounds",
     col= "blue")

#Steals
hist(train_season$steals, main="Histogram of Steals",
     xlab= "Games Played",
     col = "red")

#Blocks 
hist(train_season$blocks, main="Histogram of Blocks",
     xlab= "Blocks",
     col = "brown")

#Turnovers 
hist(train_season$turnovers, main="Histogram of Turnovers",
     xlab= "Turnovers",
     col = "pink")

#Rebounds Per Game 
hist(train_season$rebounds_pg, main="Histogram of Rebounds per Game",
     xlab= "Rebounds per Game",
     col = "grey")

#Assists per Game 
hist(train_season$assists_pg, main="Histogram of Assists Per Game",
     xlab= "Assists Per Game",
     col = "darkblue")

#Turnovers per game 


hist(train_Per_game$turnover_pg, main="Histogram of Turnovers Per Game",
     xlab="Turnovers Per Game",
     col="lightyellow")

#Position
barplot(train_subset$position)

#Subset data 
train_subset <- train_season |> 
  dplyr::select(minutes, field_goal_attempts_pg, points_pg, age, 
  position, assists_pg, rebounds_pg, turnover_pg, three_point_attempt_rate)


#Correlation Plot
corr_matrix <- cor(train_subset %>% select_if(is.numeric), use = "complete.obs") 
corrplot(corr_matrix, method = "circle")

