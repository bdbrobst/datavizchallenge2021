#Brandon Brobst
#1 July 2021
#This script pulls in NFL salary cap and Vegas game line data
  #to compare team spending to market-implied production

#install and load packages
#install.packages("nflfastR")
#install.packages("tidyverse")
#install.packages("ggimage")
library(nflfastR)
library(tidyverse)
library(ggimage)

#bring in datasets
game_lines <- read.csv(file = "Data/tidy_data/nfl_preseason_game_lines.csv")
team_unit_spending <- read.csv(file = "Data/tidy_data/2021_team_cap_spending.csv")

#create vector of teams to loop through
teams_list <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB","HOU","IND","JAX","KC","LAC","LAR","LV","MIA","MIN","NE","NO","NYG","NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS")
#create vectors to hold implied points data
mean_implied_points_scored <- vector(mode = "numeric", length = 32)
mean_implied_points_against <- vector(mode = "numeric", length = 32)

#calculate mean implied points scored/against for each team
for (i in 1:32) {
  mean_implied_points_scored[i] <- 
    (sum(game_lines$away_implied[game_lines$away == teams_list[i]]) +
       sum(game_lines$home_implied[game_lines$home == teams_list[i]])) / 17
  mean_implied_points_against[i] <- 
    (sum(game_lines$away_implied[game_lines$home == teams_list[i]]) + 
       sum(game_lines$home_implied[game_lines$away == teams_list[i]])) / 17
}

#create df with implied score data
team_mean_implieds <- data.frame(teams_list, mean_implied_points_scored, mean_implied_points_against)
colnames(team_mean_implieds) <- c("team", "implied_scored", "implied_against")

#plots
team_mean_implieds %>% 
  inner_join(team_unit_spending, by = c("team" = "team")) %>% 
  inner_join(teams_colors_logos, by = c("team" = "team_abbr")) %>% 
  ggplot(aes(x=offense/1000000, y=implied_scored)) +
  geom_image(aes(image = team_logo_wikipedia), asp = 16 / 9, size = 0.05) +
  labs(x = "Total Spending on Offense (millions)",
       y = "Market-Implied Points Scored per Game",
       title = "Cap Dollars Spent vs Vegas-Implied Production",
       subtitle = "Comparing actual spending to predicted production",
       caption = "@bdbrobst \n Data: OverTheCap.com, Westgate Superbook")

team_mean_implieds %>% 
  inner_join(team_unit_spending, by = c("team" = "team")) %>% 
  inner_join(teams_colors_logos, by = c("team" = "team_abbr")) %>% 
  ggplot(aes(x=defense/1000000, y=implied_against)) +
  geom_image(aes(image = team_logo_wikipedia), asp = 16 / 9, size = 0.05) +
  labs(x = "Total Spending on Defense (millions)",
       y = "Market-Implied Points Allowed per Game",
       title = "Cap Dollars Spent vs Vegas-Implied Production",
       subtitle = "Comparing actual spending to predicted production",
       caption = "@bdbrobst \n Data: OverTheCap.com, Westgate Superbook")

