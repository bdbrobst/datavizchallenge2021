#Brandon Brobst
#3 July 2021
#This script provides an exploratory look at penguin bill dimensions
  #facted by inhabited island

install.packages("tidyverse")
install.packages("palmerpenguins")
library(tidyverse)
library(palmerpenguins)

penguins %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(colour = species, shape = sex),
             size=2) +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  theme(legend.position = c(0.6, 0.17),
        legend.box = "horizontal",
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot") +
  facet_wrap(vars(island)) +
  labs(title = "Bill dimensions",
       subtitle = "for Adelie, Chinstrap and Gentoo Penguins \nat Palmer Station LTER by inhabited island",
       x = "Bill length (mm)",
       y = "Bill depth (mm)",
       color = "Species",
       shape = "Sex",
       caption = "@bdbrobst \n Data: github.com/allisonhorst/palmerpenguins")
