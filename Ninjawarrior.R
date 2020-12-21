library(tidyverse)
library(ggforce)
library(showtext)

# Import data
ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')

# Data wrangling
df<-ninja_warrior%>%
  group_by(obstacle_order)%>%
  count()%>%
  rename("obstacle_total" = n)

df2<- ninja_warrior%>%
  group_by(obstacle_name, obstacle_order)%>%
  count()%>%
  arrange(obstacle_order, -n)%>%
  ungroup()%>%
  left_join(., df)%>%
  mutate(perc = round(n/obstacle_total*100))%>%
  group_by(obstacle_order)%>%
  slice_head(n = 1)%>%
  mutate(label = paste("Round", paste0(obstacle_order, ":"), obstacle_name, paste0("(",perc,"%)")))

# Choose fonts for visualisation
font_add_google("Lobster", "Lob")
font_add_google("Abril Fatface", "Abril")
showtext_auto()
  
# Visualisation
p<-ggplot()+
  geom_circle(aes(x0 = -5, y0 = 3.3, r = 1.5), fill= "#101820FF")+
  geom_ellipse(aes(x0 = -5, y0 = 0, a = 3, b = 2, angle = 0), fill = "#101820FF")+
  geom_ellipse(aes(x0 = -5, y0 = 3.5, a = 1.3, b = 0.5, angle = 0), fill = "khaki")+
  geom_ellipse(aes(x0 = -7, y0 = 2, a = 1, b = 0.4, angle = -45), fill = "#101820FF")+
  geom_point(inherit.aes = F, aes(x = x, y = y),
             data = data.frame(x = c(-4.5, -5.5),
                               y = c(3.5, 3.5)))+
  geom_point(data = df2, aes(y = obstacle_order-1, x = 3.5), colour = "#FF6E40", size = 3)+
  geom_line(data = df2, aes(y = obstacle_order-1, x = 3.5), linetype = "dashed")+
  geom_text(data = df2, aes(x = 4, y = obstacle_order-1, label = label), 
            size = 10, hjust = 0, colour = "#1E3D59", family = "Abril")+
  geom_text(aes(x = -5.05, y = 8.5, label = "NINJA WARRIOR"), fontface = "bold", 
            size = 25, colour = "#000000")+
  geom_text(aes(x = -5, y = 8.5, label = "NINJA WARRIOR"), fontface = "bold", 
            size = 25, colour = "#1E3D59")+
  geom_text(aes(x = -5, y = 7.5, label = "There are a total of 225 different obstacles"), 
            size = 10, colour = "#000000", family = "Lob")+
  geom_text(aes(x = -5, y = 7, label = "across 10 seasons of Ninja Warrior."), 
            size = 10, colour = "#000000", family = "Lob")+
  geom_text(aes(x = -5, y = 6.5, label = "These are the most common obstacles"), 
            size = 10, colour = "#000000", family = "Lob")+
  geom_text(aes(x = -5, y = 6, label = "expected at each obstacle round."), 
            size = 10, colour = "#000000", family = "Lob")+
  xlim(-12, 15)+
  ylim(0, 9)+
  labs(caption = "Source: Data.World/Sasukepedia | Visualisation: @nxrunning")+
  theme(plot.background = element_rect(fill = "#e0c29c"),
        panel.background = element_rect(fill = "#e0c29c"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0.5, size = 25, family = "Abril"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Save the plot
ggsave(p, dpi = 200, width = 20, height = 12, units = "cm", filename = "ninjawarrior.jpeg")
