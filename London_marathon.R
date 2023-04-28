# Libraries
library(tidyverse)
library(ggtext)
library(ggalt)
library(showtext)

# Data
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

# Data processing to prepare the data frame for visualisation
# Years selected to focus on race attendance in the last decade
# Years selected up to 2019 since attendance from 2020 onward is affected by Covid-19
df_viz<-london_marathon|>
  select(Year, Starters, Finishers)|>
  filter(Year<2020  & Year >=2013)|>
  mutate(finish_perc = (Finishers/Starters)*100)|>
  mutate(label_perc = paste(format(round(finish_perc,digits = 1), nsmall = 1), "%"))|>
  mutate(Year = as.factor(Year))

# Average completion rate
mean(df_viz$finish_perc)

# Import fonts
# To use the icons, download the otf file into your working directory
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")
font_add_google(name = "Cabin")

showtext_auto()

####----Visualisation----####

p<-ggplot(df_viz, aes(y = Year, x = Starters, xend = Finishers))+
  geom_dumbbell(size = 2,
                size_x = 3,
                size_xend = 3,
                colour = "#B3B3B3",
                colour_x = "#141F52",
                colour_xend = "#F9C31F")+
  geom_text(aes(x = Finishers, label = Finishers), hjust = 1.4, size = 12)+
  geom_text(aes(x = Starters, label = Starters), hjust = -0.4, size = 12)+
  geom_textbox(aes(x = 33500, y = 5.5),
               label = "Participating numbers in the London Marathon have been increasing over the last decade. Interestingly, 0.6 % to 1.4 % of runners did not cross the finish line each year.",
               hjust = 0, halign = 0,
               size = 12,
               lineheight = 0.2,
               box.colour = NA,
               fill = NA)+
  geom_text(aes(x = 32500, label = label_perc), size = 12, colour = "#0D0D0D")+
  geom_vline(xintercept = 32000)+
  scale_x_continuous(limits = c(32000,44000),
                     breaks = c(34000,36000,38000,40000,42000),
                     expand = c(0,0))+
  labs(title = "London Marathon: 98.9% of Runners Finished the Race",
       subtitle = "Number of <span style = 'color:#141F52'><b>Starters</b></span> and <span style = 'color:#F9C31F'><b>Finishers</b></span> in the London Marathon",
       x = "Number of Runners",
       caption = "Data Visualisation: Nien Xiang Tou | Nienxiangtou.com | <span style='font-family:fb;'>&#xf09b; </span>Nxrunning | <span style='font-family:fb;'>&#xf099; </span>Nxrunning")+
  theme(plot.background = element_rect(fill = "#FFFFFF", colour = "#FFFFFF"),
        panel.background = element_rect(fill = "#FFFFFF"),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 35, colour = "#0D0D0D", face = "bold"),
        axis.text.x = element_text(size = 30, colour = "#0D0D0D"),
        axis.title.x = element_textbox_simple(size = 40, halign = 0.5, face = "bold", 
                                              margin = margin(t = 0.5, unit = "cm")),
        plot.title = element_textbox_simple(colour = "#E3120B", family = "Cabin",
                                            face = "bold", size = 60),
        plot.subtitle = element_textbox_simple(colour = "#0D0D0D", family = "Cabin",
                                               face = "bold", size = 50,
                                               margin = margin(t = 0.3, unit = "cm")),
        plot.caption = element_textbox_simple(colour = '#141F52', size = 35,
                                        family = "Cabin", halign = 0.5,
                                        margin = margin(t = 0.5, b = 0.5, unit = "cm")))

# Save the plot
ggsave("Londonmarathon.png",plot = p,
       dpi = 400, width = 8, height = 4)
