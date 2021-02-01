library(tidyverse)
library(ggalt)
library(ggtext)

# Import data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

# Data from 2020
df20<-plastics%>%
  filter(year == 2020)%>%
  group_by(country)%>%
  summarize(total_2020 = sum(grand_total))%>%
  na.omit()

# Data from 2019
df19<-plastics%>%
  filter(year == 2019)%>%
  filter(parent_company != "Grand Total")%>%
  group_by(country)%>%
  summarize(total_2019 = sum(grand_total))%>%
  na.omit()

# Combine both dataframes and keep top 10 countries with both years of data
combined_df <- right_join(df19, df20, by = "country")%>%
  na.omit()%>%
  mutate(difference = total_2020 - total_2019,
         diff_group = as.factor(ifelse(total_2020 - total_2019 > 0, "Positive", "Negative")))%>%
  top_n(abs(difference), n = 10)

# Visualisation
p<-ggplot(combined_df, aes(y = reorder(country, abs(difference)), x = total_2019, xend = total_2020,
                        colour = diff_group))+
  geom_dumbbell(size= 1.2,
                size_x = 3,
                size_xend = 3,
                colour_x = "#202c3d",
                colour_xend = "#ac2023")+
  scale_colour_manual(values = c("#b3c6ff", "#ffd966"))+
  scale_x_continuous(breaks = c(seq(0,60000, 10000)))+
  labs(title = "Change in Global Plastic Pollution",
       subtitle = "Top ten countries with the greatest change in plastic pollution count between 
<span style = 'color:#202c3d'><b>2019</b></span> and <span style = 'color:#ac2023'><b>2020</b></span>. <span style = 'color:#ffd966'><b>Increase</b></span> in pollution was more prevalent than <span style = 'color:#b3c6ff'><b>decline</b></span>.<br>",
       caption = "\nSource: Break Free From Plastic | Visualisation: @nxrunning")+
  theme(plot.background = element_rect(fill = "#517889", colour = "#517889"),
        panel.background = element_rect(fill = "#517889"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white", size = 10, face = "bold"),
        plot.title = element_text(colour = "#f3e843", size = 20, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(
          size = 11,
          color = "#ffffff",
          padding = margin(5,5,5,5),
          margin = margin(0,0,0,0)),
        plot.caption = element_text(colour = "white", size = 10, face = "bold", hjust = 0))
  
# Save the plot
ggsave(p, dpi = 600, filename = "Plasticpollution.jpeg")