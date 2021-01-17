library(tidyverse)

# Import data
artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

# Visualisation
p <- artwork%>%
  select(id, year, width, height, units)%>%
  na.omit()%>%
  mutate(century = case_when(year < 1801 ~ "Before 19th Century",
                             year >= 1801 & year <1901 ~ "19th Century",
                             year >= 1901 & year <2001 ~ "20th Century",
                             year >= 2001 ~ "21st Century"))%>%
  mutate(century = factor(century, levels = c("Before 19th Century",
                                                 "19th Century",
                                                 "20th Century",
                                                 "21st Century")))%>%
  filter(height < 30000)%>%
  ggplot(aes(x = width/1000, y = height/1000))+
  geom_rect(aes(xmin = 0, ymin = 0, xmax = width/1000, ymax = height/1000), 
            alpha = 0.1, fill = "#c04460", colour = "#500e31", size = 1.2)+
  scale_x_continuous(limits = c(0, 12), breaks = c(0,2,4,6,8,10,12))+
  labs(title = "Dimensions of Art Collections",
       subtitle = "While most art collections originated in the 19th Century,\nthey increased in size notably from the 20th Century onwards.",
       x = "Width (m)",
       y = "Height (m)",
       caption = "Source: Tate Collection | Visualisation: @nxrunning")+
  facet_wrap(~century, ncol = 2)+
  theme(plot.title = element_text(colour = "#500e31", face="bold"),
        plot.subtitle = element_text(colour = "black"),
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        strip.text.x = element_text(colour = "white"),
        strip.background = element_rect(fill= "#c04460", colour="transparent", size=0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Save the plot
ggsave(p, dpi =600, filename = "Artcollection.jpeg")
