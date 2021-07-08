#Brandon Brobst
#7 July 2021
#This script makes a table of the most-played League of Legends
  #champions in competitive play on the current patch

install.packages("tidyverse")
install.packages("gt")

library(tidyverse)
library(gt)

#datasets
oe_match_data <- read.csv(url("https://oracleselixir-downloadable-match-data.s3-us-west-2.amazonaws.com/2021_LoL_esports_match_data_from_OraclesElixir_20210707.csv"))
champ_logos <- read.csv(url("https://raw.githubusercontent.com/bdbrobst/dataviz_assets/master/lol_champ_logos.csv"))

#filter for current patch
top_lane_1113 <- oe_match_data %>%
  filter(position == "top" & patch == 11.13)

#calculate stats by champ
top_1113_by_champ <- top_lane_1113 %>% 
  group_by(champion) %>% 
  summarise(games_played = n(),
            wr = mean(result),
            meanscd10 = mean(csdiffat10)) %>% 
  filter(games_played >= 10)

#join logos table to render images and create table
top_1113_by_champ %>% 
  left_join(champ_logos, by = c("champion" = "champ")) %>%
  arrange(desc(games_played)) %>% 
  slice(1:10) %>% 
  select(url, games_played, wr, meanscd10) %>% 
  gt() %>% 
  tab_header(
    title = "LoL Top Lane Meta",
    subtitle = md("Patch 11.13, top 10 by number of games played")
  ) %>%
  fmt_percent(wr) %>% 
  fmt_number(meanscd10) %>% 
  text_transform(
    locations = cells_body(columns = url),
    fn = function(x) {
      web_image(
        url = x,
        height = 40
        )
    }
  ) %>% 
  cols_label(
    url = md("**Champion**"),
    games_played = md("**Games Played**"),
    wr = md("**Winrate**"),
    meanscd10 = md("**Avg CSD @ 10 min**"),
  ) %>% 
  tab_source_note(md("**Data:** oracleselixir.com<br>**Table:** @bdbrobst<br> 7 July 2021"))
  