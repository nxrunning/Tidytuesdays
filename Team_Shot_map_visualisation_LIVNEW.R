# Libraries
library(worldfootballR)
library(tidyverse)
library(ggsoccer)
library(showtext)
library(ggtext)
library(ggimage)

# Retrieving data
df<-understat_match_shots("https://understat.com/match/22088")|>
  mutate(shot_team = NA)

# Differentiating the goal attempts between teams
df <- df|>
  mutate(shot_team = case_when(home_away == 'h' ~ "Liverpool",
                               home_away == 'a' ~ "Newcastle"))

# Differentiating the coordinates into both sides of the pitch
# Home team's goal will be shown on the left hand side of the visualisation
df2<-df|>
  mutate(shot_x = case_when(shot_team == "Liverpool" ~ X,
                            shot_team == "Newcastle" ~ 1 - X),
         shot_y = case_when(shot_team == "Liverpool" ~ Y,
                            shot_team == "Newcastle" ~ 1 - Y),
         shot_label = case_when(result == "Goal" ~ "Goal",
                                result != "Goal" ~ "Non-Goal"))
# Team stats
df2|>
  group_by(shot_team)|>
  summarise(n= n(),
            sum = sum(xG),
            mean_xG = round((sum(xG)/n),2))

####----Fonts----####

# To use the icons, download the otf file into your working directory
font_add_google("Roboto Condensed")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")
showtext_auto()

####----Visualisation----####

plot_output<-ggplot(data = df2, aes(x = shot_x*100, y = shot_y*100))+
  annotate_pitch(colour = "white",
                 fill = "#304529",
                 limits = TRUE)+
  geom_point(aes(color = shot_label), alpha = 0.8, size = 4)+
  scale_color_manual(values = c("#CF4B4B", "#F29330"))+
  geom_text(data = subset(df2, shot_label == "Goal"),
            aes(x = shot_x*100, y = shot_y*100, label = round(xG,2)),
            hjust = 0.5, vjust = 0.5, color = "white", size = 6)+
  # Home Team Name
  annotate("text", x = 1, y = 99, label = "Liverpool", color = "white",
           hjust = 0, vjust =1, size = 20, fontface = "bold", 
           family = "Roboto Condensed")+
  # Away Team Name
  annotate("text", x = 99, y = 99, label = "Newcastle United", color = "white",
           hjust = 1, vjust = 1, size = 20, fontface = "bold", 
           family = "Roboto Condensed")+
  # Home Team Image
  geom_image(aes(x = 18, y = 95,
                 image = "https://upload.wikimedia.org/wikipedia/en/0/0c/Liverpool_FC.svg"), size = 0.08)+
  # Away Team Image
  geom_image(aes(x = 69, y = 95,
                 image = "https://upload.wikimedia.org/wikipedia/en/5/56/Newcastle_United_Logo.svg"), size = 0.095)+
  # Home Team Score
  annotate("text", x = 45, y = 99, label = "4", color = "#C0C7BE",
           hjust = 1, vjust = 1, size = 20, fontface = "bold",
           family = "Roboto Condensed")+
  # Away Team Score
  annotate("text", x = 55, y = 99, label = "2", color = "#C0C7BE",
           hjust = 0, vjust = 1, size = 20, fontface = "bold",
           family = "Roboto Condensed")+
  # Home Team Total xG
  annotate("text", x = 45, y = 90, label = "7.14 xG", color = "white",
           hjust = 0.5, vjust = 1, size = 12, family = "Roboto Condensed")+
  # Away Team Total xG
  annotate("text", x = 55, y = 90, label = "0.91 xG", color = "white",
           hjust = 0.5, vjust = 1, size = 12, family = "Roboto Condensed")+
  # Home Team Shot stats
  annotate("text", x = 49, y = 5, label = "34 Shots | 0.21 xG per Shot", color = "white",
           hjust = 1, vjust = 0, size = 12, family = "Roboto Condensed")+
  # Away Team Shot stats
  annotate("text", x = 51, y = 5, label = "5 Shots | 0.18 xG per Shot", color = "white",
           hjust = 0, vjust = 0, size = 12, family = "Roboto Condensed")+
  labs(title = "Record-breaking Expected Goals (xG) in English Premier League History",
       subtitle = "Liverpool registered the highest expected goals against Newcastle United during the new year day showdown at Anfield.<br>The home team had a total of 34 shots, in which two were penalties.",
       caption = "Data Visualisation: Nien Xiang Tou | Nienxiangtou.com | <span style='font-family:fb;'>&#xf09b; </span>Nxrunning | <span style='font-family:fb;'>&#xf099; </span>Nxrunning | Data source: Understat.com")+
  theme(plot.background = element_rect(fill = "#304529", color = "#304529"),
        panel.background = element_rect(fill = "#304529"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#304529", color = "#304529"),
        legend.key = element_rect(fill = "#304529", color = "#304529"),
        legend.title = element_blank(),
        legend.text = element_text(color = "white", size = 20,
                                   family = "Roboto Condensed"),
        legend.margin = margin(t = 0, b = 0),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(color = "white", face = "bold", size = 50,
                                  family = "Roboto Condensed"),
        plot.subtitle = element_textbox_simple(color = "white", size = 28, lineheight = 0.5,
                                     family = "Roboto Condensed"),
        plot.caption = element_textbox_simple(colour = 'white', size = 20,
                                              family = "Roboto Condensed", halign = 0.5,
                                              margin = margin(t = 0.5, b = 0.5, unit = "mm")))

# Save plot
ggsave(plot_output, width = 200, height = 112.4, units = "mm", filename = "Liverpool_Newcastle.jpeg")
