# Import libraries
library(tidyverse)
library(ggforce)
library(tidytext)
library(ggwordcloud)
library(cowplot)
library(showtext)

# Import data
women <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-08/women.csv')

# Data processing
role_words <- women%>%
  filter(name != "Unsung hero")%>%
  select(role, category)%>%
  unnest_tokens(word, role)%>%
  anti_join(stop_words)

# Choose fonts for visualisation
font_add_google("Rum Raisin", "Rum")
font_add_google("Kite One", "Kite")
showtext_auto()

# Wordcloud plot

# Image for wordcloud mask
image <- readPNG("woman_mask.png")

set.seed(4567)
wordcloud<-role_words%>%
  count(word, sort = TRUE)%>%
  mutate(word = reorder(word, n),
         angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))%>%
  ggplot(aes(label = word, size = n, angle = angle))+
  geom_text_wordcloud_area(mask = image, colour = "#FBDE44FF", rm_outside = TRUE)+
  scale_size_area(max_size = 15) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "#28334AFF", colour = "#28334AFF"))

# Title plot
title<-ggplot()+
  geom_circle(aes(x0 = 5, y0 = 5, r = 1), lwd = 5, colour = "#915ebd")+
  geom_polygon(inherit.aes = F, aes(x = x, y = y),
               data = data.frame(x = c(3.75,4.75,4.75,5.25,5.25,6.25,6.25,5.25,5.25,4.75,4.75,3.75),
                                 y = c(2.75,2.75,1.5,1.5,2.75,2.75,3.15,3.15,3.9,3.9,3.15,3.15)),
               fill = "#915ebd")+
  geom_text(aes(x = 5.05, y = 9, label = "BBC's 100\nWomen of 2020", fontface = "bold"), 
            colour = "black", size = 8, family = "Rum")+
  geom_text(aes(x = 5, y = 9, label = "BBC's 100\nWomen of 2020", fontface = "bold"), 
            colour = "#FBDE44FF", size = 8, family = "Rum")+
  geom_text(aes(x = 5, y = 7, label = "Women from all walks of life\nleading change and making a difference", fontface = "bold"), 
            colour = "white", size = 4, family = "Kite")+
  geom_text(aes(x = 5, y = 1, label = "Source: BBC | Visualisation: @nxrunning"),
            colour = "#FBDE44FF", size = 3)+
  xlim(0, 10)+
  ylim(1,10)+
  theme(plot.background = element_rect(fill = "#28334AFF", colour = "#28334AFF"),
        panel.background = element_rect(fill = "#28334AFF"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Combine plots
p<-plot_grid(title, wordcloud, ncol = 2)

# Save plot
ggsave(p, dpi = 100, width = 16, height = 10, units = "cm", filename = "BBCWomen2020.jpeg", device = "jpeg")
