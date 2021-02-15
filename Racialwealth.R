library(tidyverse)
library(ggtext)
library(glue)

# Import data
race_wealth <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/race_wealth.csv')

# Data Wrangling
ribbon_data<-race_wealth%>%
  filter(type == "Average")%>%
  filter(year != 1963)%>%
  filter(race %in% c("White", "Black"))%>%
  pivot_wider(names_from = race, values_from = wealth_family)

line_data<-race_wealth%>%
  filter(type == "Average")%>%
  filter(year != 1963)%>%
  filter(race %in% c("White", "Black"))

# Visualisation
p<-ggplot()+
  geom_ribbon(data = ribbon_data, aes(x = year, ymin = White, ymax = Black), fill = "#D96459")+
  geom_line(data = line_data, aes(x = year, y = wealth_family, color = race), size = 1.8)+
  scale_colour_manual(values = c("#89c646", "#fec410"))+
  scale_x_continuous(limits = c(1978, 2020) ,
                     breaks = c(seq(1983,2016,10)))+
  scale_y_continuous(labels = scales::dollar)+
  labs(y = "Average family wealth (USD)",
       x = " ",
       caption = "Source: Urban Institute & US Census<br>Visualisation: @nxrunning")+
  annotate(GeomRichtext, x = 1978, y = 1000000, label = glue("**Persistence in Racial Wealth Gap:**<br><span style = 'color:#fec410'>**White**</span> versus <span style = 'color:#89c646'>**Black**</span>"), 
           hjust = 0, vjust = 1, size = 5, label.color = NA, fill = "black", colour = "white") +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(size = 9, colour = "white"),
    axis.text.x = element_text(colour = "white"),
    axis.ticks = element_line(colour = "white"),
    axis.title.y = element_text(colour = "white", size = 12),
    plot.caption = element_markdown(size = 10, hjust =0.5, colour = "white"))

# Save the plot
ggsave(p, dpi = 300, filename = "racialwealth.jpeg")
