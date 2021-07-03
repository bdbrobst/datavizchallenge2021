# Brandon Brobst
#2 July 2021
#This script aggregates WNBA pbp data to look at 
  #season-level Offensive and Defensive Efficiency

#install and load libraries
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("ggimage")

library(ggimage)
library(devtools)
library(tidyverse)

devtools::install_github(repo = "saiemgilani/wehoop")

#pulling logos into a separate df for future use with ggimage
team_logo <- unique(team_box_scores$team_logo)
team_name <- unique(team_box_scores$team_name)
team_logos <- data.frame(team_name, team_logo)

#Efficiency Calculation: Points  / (FGA - OR + TO + (0.4 x FTA))

#this has the metrics needed to calculate possesions
team_box_scores <- wehoop::load_wnba_team_box(2021)

#this has the points scored data
espn_2021_scoreboard <- wehoop::espn_wnba_scoreboard (season = 2021)
#filter out preseason data
espn_2021_scoreboard <- espn_2021_scoreboard %>% 
  filter(type == 2)


#FGA, FGM and FTA, FTM are stored in the same column, let's split them
team_box_scores <- team_box_scores %>% 
  separate(field_goals_made_field_goals_attempted, c("FGM","FGA"), sep = "-")

tsb2 <- team_box_scores %>% 

team_box_scores <- team_box_scores %>% 
  separate(free_throws_made_free_throws_attempted, c("FTM","FTA"), sep = "-")

#calculate team aggregate possesions for each team
#also adding logo urls here 
team_season_aggregate_possesions <- team_box_scores %>% 
  group_by(team_name) %>% 
  summarise(n_possesions = sum(as.numeric(FGA)) - 
              sum(as.numeric(offensive_rebounds)) +
              sum(as.numeric(turnovers)) +
              0.4 * sum(as.numeric(FTA)))

#calculate possesions against
possesions_by_game <- team_box_scores %>% 
  group_by(game_id, team_name) %>% 
  summarise(n_possesions = sum(as.numeric(FGA)) - 
              sum(as.numeric(offensive_rebounds)) +
              sum(as.numeric(turnovers)) +
              0.4 * sum(as.numeric(FTA)))

team_season_aggregate_possesions_against %>% 
  left_join(possesions_by_game, by = c("game_id" = "game_id")) %>% 
  subset(team_name.x != team_name.y) %>% 
  group_by(team_name.x) %>% 
  summarise(n_possesions_against = sum(n_possesions.y))
  

#calculate team aggregate scoring for and against
teams_list <- c("Aces","Dream","Fever","Liberty","Lynx","Mercury","Mystics","Sky","Sparks","Storm","Sun","Wings")
aggregate_points_scored <- vector(mode = "numeric", length = 12)
aggregate_points_against <- vector(mode = "numeric", length = 12)

for (i in 1:12) {
  aggregate_points_scored[i] <-
    sum(espn_2021_scoreboard$home_score[espn_2021_scoreboard$home_team_name == teams_list[i]]) +
    sum(espn_2021_scoreboard$away_score[espn_2021_scoreboard$away_team_name == teams_list[i]])

  aggregate_points_against[i] <-
    sum(espn_2021_scoreboard$home_score[espn_2021_scoreboard$away_team_name == teams_list[i]]) +
    sum(espn_2021_scoreboard$away_score[espn_2021_scoreboard$home_team_name == teams_list[i]])
}


team_season_aggregate_scoring <- data.frame(teams_list, aggregate_points_scored, aggregate_points_against)
colnames(team_season_aggregate_scoring) <- c("team", "aggregate_points_scored", "aggregate_points_against")
#join scoring and possesion dfs, then plot
team_season_aggregate_scoring %>% 
  inner_join(team_season_aggregate_possesions, by = c("team" = "team_name")) %>% 
  inner_join(team_season_aggregate_possesions_against, by = c("team" = "team_name.x")) %>% 
  inner_join(team_logos, by = c("team" = "team_name")) %>% 
  ggplot(aes(x = aggregate_points_scored / n_possesions * 100, 
             y = aggregate_points_against / n_possesions_against * 100)) +
  geom_hline(yintercept = sum(team_season_aggregate_scoring$aggregate_points_scored) / 
               sum(team_season_aggregate_possesions$n_possesions) * 100) +
  geom_vline(xintercept = sum(team_season_aggregate_scoring$aggregate_points_against) / 
             sum(team_season_aggregate_possesions_against$n_possesions_against) * 100) +
  geom_image(aes(image = team_logo), asp = 16 / 9, size = 0.06) +
  labs(title = "WNBA Efficiency Landscape",
       subtitle = "As of July 2 2021",
       x = "Offensive Efficiency",
       y = "Defensive Efficiency",
       caption = "@bdbrobst \n Data: saiemgilani.github.io/wehoop/") +
  theme(plot.background = element_rect(fill = '#EFE3C6'),
        panel.background = element_rect(fill = "#EFE3C6"))

