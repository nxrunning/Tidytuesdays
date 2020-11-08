library(tidyverse)

# import data
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

# Visualisation
ikea%>%
  group_by(category)%>%
  summarise(avg_price_usd = mean(price)*0.27)%>% #convert saudi riyals to usd
  ggplot(aes(x = reorder(category, avg_price_usd), y = avg_price_usd))+
  geom_bar(stat = "identity", fill = "#FFDA1A")+
  geom_text(aes(label = paste0("$", round(avg_price_usd, 2))), color="#0051BA", size = 3, hjust = 1.1)+
  labs(title = "IKEA Furniture",
       subtitle = "Average price of each furniture category",
       y = "Price (USD)",
       x = "",
       caption = "Data source: Kaggle | Visualisation: @nxrunning")+
  coord_flip()+
  theme(
    plot.title = element_text(color="#FFDA1A", size=18, hjust=-1, margin = margin(5, 2, 5, 2)),
    plot.subtitle = element_text(color="white", size=12, hjust=-3.5, margin = margin(2, 2, 5, 2)),
    plot.caption = element_text(color = "#FFDA1A"),
    axis.text.y = element_text(size = 10, color = "white"),
    axis.ticks.y =  element_blank(),
    axis.text.x = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    panel.border = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
