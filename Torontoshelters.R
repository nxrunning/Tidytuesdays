# Import libraries
library(tidyverse)
library(ggTimeSeries)

# Import data
shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

# Visualisation
p<-shelters%>%
  separate(occupancy_date, c("year", "month", "day"), sep = "-", remove = FALSE) %>%
  filter(capacity != 0)%>%
  mutate(rate = (occupancy/capacity)*100,
         year = as.numeric(year))%>%
  group_by(occupancy_date, year)%>%
  summarise(occupancy = mean(rate))%>%
  ggplot_calendar_heatmap(cDateColumnName = "occupancy_date", cValueColumnName = "occupancy",
                           dayBorderSize = 0.1, monthBorderSize = 1)+
  scale_fill_viridis_c(option = "cividis")+
  labs(title = "Daily occupancy of Toronto homeless shelters",
       subtitle = "There are a total of 114 homeless shelter facilities in Toronto, and average daily occupancy rate was\nabove 90% every single day. ",
       fill = "Occupancy rate (%)",
       caption = "Source: opendatatoronto | Visualisation: @nxrunning")+
  facet_wrap(~year, ncol = 1)+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        plot.title.position = "plot",
        plot.title = element_text(colour = "#dba622"),
        plot.subtitle = element_text(colour = "white", size = 6),
        plot.caption = element_text(colour = "#dba622", size = 6),
        strip.text.x = element_text(colour = "white"),
        strip.background = element_rect(fill=alpha("#dba622", 0.6), colour="transparent", size=0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white", size = 7),
        legend.text = element_text(colour = "white"),
        legend.background = element_rect(fill = "black"),
        legend.title = element_text(colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())