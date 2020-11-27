library(tidyverse)
library(ggridges)
library(cowplot)


hike_data <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds'))

# Data cleaning
clean_hike_data <- hike_data %>% 
  mutate(
    trip = case_when(
      grepl("roundtrip",length) ~ "roundtrip",
      grepl("one-way",length) ~ "one-way",
      grepl("of trails",length) ~ "trails"),
    
    length_total = as.numeric(gsub("(\\d+[.]\\d+).*","\\1", length)) * ((trip == "one-way") + 1),
    
    gain = as.numeric(gain),
    highpoint = as.numeric(highpoint),
    rating = as.numeric(rating),
    
    region = gsub("(.*)\\s[-][-].*","\\1",location)
  ) %>%
  mutate_if(is.list, simplify_all) %>%   
  unnest(cols=c(features)) %>%
  mutate(features = factor(features))


ggplot(feature_data, aes(x = highpoint, y = reorder(features, highpoint), fill = features))+
  geom_density_ridges()+
  theme_minimal()+
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.border = element_blank())

ggplot(feature_data, aes(x = gain, y = reorder(features, gain), 
                         fill = features, colour = features))+
  geom_density_ridges(alpha = 0.6)+
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.1))) +
  scale_x_continuous(expand = c(0.01,0))+
  theme_ridges()+
  theme(legend.position = "none",
        axis.text.y = element_text(colour = "black"),
        axis.text.x = element_text(colour = "black"),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank())


p1<-clean_hike_data%>%
  filter(features %in% c("Ridges/passes", "Mountain views", "Good for kids", "Dogs allowed on leash",
                         "Summits", "Rivers"),
         gain < quantile(gain, 0.75)+IQR(gain))%>%
  ggplot(aes(x = gain, y = reorder(features, gain), fill = factor(stat(quantile))))+
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = c(0.25, 0.75), quantile_lines = TRUE
  ) +
  scale_fill_manual(values = c("#374d7c", "#46edc8", "#fdf289"))+
  scale_y_discrete(expand = expansion(mult = c(0.01, 0.1))) +
  scale_x_continuous(expand = c(0.01,0))+
  labs(title = "Washington Trails",
       subtitle = "Dog and child friendly trails tend to have lower elevation gains\nas compared to those with ridges and summits",
       x = "Elevation gain (ft)")+
  theme_ridges()+
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(colour = "#821332", size = 20, margin = margin(0,1,0.5,0, "cm")),
        plot.subtitle = element_text(size = 10, margin = margin(0,1, 0.5, 0, "cm")),
        plot.background = element_rect(fill = "grey"),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1,1,0,0.5), "cm"))


# Legend

legend<-ggplot()+
  geom_rect(mapping = aes(xmin = 1, xmax = 3, ymin = 1, ymax = 2), fill = "#374d7c")+
  geom_rect(mapping = aes(xmin = 3, xmax = 7, ymin = 1, ymax = 2), fill = "#46edc8")+
  geom_rect(mapping = aes(xmin = 7, xmax = 9, ymin = 1, ymax = 2), fill = "#fdf289")+
  annotate(geom ="text", x = 3, y = 5, label = "Quartile 1", colour = "black", size = 4)+
  annotate(geom ="text", x = 7, y = 5, label = "Quartile 3", colour = "black", size = 4)+
  annotate(geom = "curve", x = 3, y = 3.5, yend = 2.5, xend = 3, curvature = 0, color = "black",
           arrow = arrow(length = unit(0.03, "npc")))+
  annotate(geom = "curve", x = 7, y = 3.5, yend = 2.5, xend = 7, curvature = 0, color = "black",
           arrow = arrow(length = unit(0.03, "npc")))+
  ylim(0, 7)+
  xlim(0,10)+
  theme_void()+
  labs(caption = "Source: Washington Trail Association | Visualisation: @nxrunning")+
  theme(plot.background =  element_rect(fill = "grey", colour = "grey"),
        plot.caption = element_text(size = 10, hjust = 1.25),
        plot.margin = unit(c(0,1,0,1), "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Combine legend and plot
comb_plot<-plot_grid(p1, legend, nrow = 2, ncol = 1, rel_heights = c(50, 10))

# Save plot
ggsave(comb_plot, dpi = 600, filename = "washingtontrails.jpeg")
