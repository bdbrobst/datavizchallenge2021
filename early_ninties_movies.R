#Brandon Brobst
#5 July 2021
#This scripts makes a table of top-rated early ninties movies
  #mutated a variable based on existing variable's values
  #used a bit of markdown to style the table a bit

install.packages("ggplot2movies")
install.packages("gt")
library(ggplot2movies)
library(gt)
#Filter for movies from the early 90s
early_ninties_movies <- movies %>% 
  filter(year >= 1991 & year <= 1995)

#Let's filter for movies with at least 500 total ratings,
  #at least 80 minutes long,
  #have an mpaa rating,
  #and that are either solely a Comedy, Action, or Drama
early_ninties_movies <- early_ninties_movies %>% 
  filter(votes >= 500) %>%
  filter(length >= 80) %>% 
  filter(mpaa != "") %>% 
  filter((Comedy == 1 & Action == 0 & Drama == 0) | 
          (Comedy == 0 & Action == 1 & Drama == 0) |
           (Comedy == 0 & Action == 0 & Drama == 1)) %>% 
  arrange(desc(rating))

#mutate a "Genre" variable based on category
early_ninties_movies <- early_ninties_movies %>% 
  mutate(genre = case_when(Action == 1 ~ "Action", 
                        Comedy == 1 ~ "Comedy",
                        Drama == 1 ~ "Drama",
                        TRUE ~ NA_character_))

#build the table
early_ninties_movies %>% 
  group_by(genre) %>% 
  slice(1:5) %>%
  select(title, year, length, rating, votes) %>% 
  gt() %>% 
  tab_header(
    title = "Top 5 Early 90s Movies by Genre",
    subtitle = md("based on IMDB rating, minimum 500 ratings<br>and 80 minutes in runtime")
  ) %>%
  tab_style(
    style = list(cell_text(style = "italic")),
    locations = cells_row_groups()
  ) %>% 
  fmt_number(
    columns = votes,
    decimals = 0,
    suffixing = F) %>% 
  cols_label(
    title = md("**Title**"),
    year = md("**Release**"),
    length = md("**Runtime in minutes**"),
    rating = md("**Rating**"),
    votes = md("**Number of Ratings**")
  ) %>% 
  tab_source_note(md("**Data:** ggplot2movies<br>**Table:** @bdbrobst"))
