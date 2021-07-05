#Brandon Brobst
# 4 July 2021
#This script is a quick exploratory plot of
  #Consumer Product Safety Commision data on fireworks injuries

#Load data
fireworks_injuries <- read.csv(file = "Data/fireworks_injuries.csv", check.names = F)

#move from wide to long for plotting
long_fi <- fireworks_injuries %>% 
  gather(variable, value, -ages)

long_fi$ages <- factor(long_fi$ages, levels = c("0 to 4",
                                            "5 to 14",
                                            "15 to 24",
                                            "25 to 44",
                                            "45 to 64"))
long_fi %>% 
  ggplot(aes(x = variable, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ages) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot") +
  scale_fill_manual(values = c("#D40402","#EFCC90","#84BDC6", "#D8D8D8", "#FB9AAC")) +
  labs(title = "2020 Fireworks-Related, Emergency Department-Treated Injuries",
       subtitle = "by Age Group and Device Type",
       caption = "@bdbrobst \nData: cpsc.gov",
       y= "Number of injuries",
       fill = "Device Type")

