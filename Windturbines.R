library(tidyverse)
library(ggforce)
library(cowplot)

# Import data
wind_turbine <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv')

plot_df<-wind_turbine%>%
  select(province_territory, hub_height_m)%>%
  group_by(province_territory)%>%
  summarise(height = mean(hub_height_m))

# Create plot
p<-ggplot(plot_df, aes(fill = province_territory, alpha = height))+
  geom_circle(aes(x0 = 4.5, y0 = 5.25, r = 0.15), fill = "navy")+
  geom_rect(mapping = aes(xmin = 4.35, xmax = 4.65, ymin= 1, ymax = 4.75), fill = "navy")+
  geom_polygon(inherit.aes = F, aes(x = x, y = y), fill = "grey",
               data = data.frame(x = c(4.2,4.3,4.25,3.7),
                                 y = c(5.25,4.75,4.5,3.25)))+
  geom_polygon(inherit.aes = F, aes(x = x, y = y), fill = "grey",
               data = data.frame(x = c(4.7,4.8,4.9,5.5),
                                 y = c(4.75,5.2,5.1,3.25)))+
  geom_polygon(inherit.aes = F, aes(x = x, y = y), fill = "grey",
               data = data.frame(x = c(4.45,4.55,4.55,4.4),
                                 y = c(5.75,5.75,8.75,6.25)))+
  facet_wrap(~province_territory, nrow = 3, ncol = 4)+
  theme_void()+
  theme(
    legend.position = "none",
    strip.text.x = element_text(size= 7, colour="white", margin = margin(.1, 0, .1, 0, "cm")),
    strip.background = element_rect(fill="red", colour="black", size=0.5),
    plot.background = element_rect(fill = "white", colour = "white"),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"))

# Create title
title<-ggplot() +
  labs(x = NULL, y = NULL,
       title = "Height of Canadian Wind Turbines",
       subtitle = "Lighter shade indicates shorter height of wind turbine")+
  theme_void(10)+
  theme(
    plot.title = element_text(color="Navy", size=18, hjust=0.5, margin = margin(5, 2, 2, 2)),
    plot.subtitle = element_text(color="black", size=10, hjust=0.5, margin = margin(2, 2, 2, 2)),
    axis.text.y = element_blank(),
    axis.ticks.y =  element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(fill = NA, color = NA),
    strip.background = element_rect(fill = NA, color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"))

# Combine title and plot
comb_plot<-plot_grid(title, p, nrow =2, ncol = 1, rel_heights = c(3,23), rel_widths = c(18,18))

# Add caption
turbine_plot<-ggdraw(add_sub(comb_plot, "Visualisation: @nxrunning", size = 10, hjust = -0.90, color = "black"))+
  theme(plot.background = element_rect(fill = "white", color = "white"))
