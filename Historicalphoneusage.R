# Import libraries
library(tidyverse)
library(gganimate) #For animating ggplot
library(gifski) #For rendering the animation

# Import data
mobile <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-10/mobile.csv')

# Find the countries with NA data for gdp per cap
missing_data<-mobile%>%
  filter(is.na(gdp_per_cap))

# Create new dataframe without the countries with missing data
clean_data<-mobile%>%
  anti_join(missing_data, by = "entity")

# Some processing
clean_data$year <- as.integer(clean_data$year)
clean_data$gdp_per_cap <- as.numeric(format(clean_data$gdp_per_cap, scientific = F))
clean_data$year_label <- as.character(round(clean_data$year))

# Visualisation
p<-ggplot(data = clean_data, aes(x = gdp_per_cap, y = mobile_subs, colour = continent,
                                 label = year_label))+
  geom_point()+
  labs(title = "Global Mobile Phone Ownership",
       subtitle = "Growth of fixed mobile subscriptions between 1990-2017",
       y = "Mobile subscriptions (per 100 people)",
       x = "GDP per capita (int.-$)",
       colour = "Continent",
       caption = "Source: Our World in Data | Visualisation: @nxrunning")+
  scale_x_continuous(labels=scales::dollar_format(), breaks = seq(0, 125000, 25000))+
  theme_minimal()+
  theme(plot.title = element_text(size = 18, colour = "yellow1"),
        plot.subtitle = element_text(size = 12, colour = "white"),
        legend.text = element_text(size = 10, colour = "white"),
        legend.title = element_text(size = 12, colour = "white"),
        axis.title.x = element_text(colour = "white", size = 12),
        axis.title.y = element_text(colour = "white", size = 12),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.caption = element_text(colour = "yellow1"),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "black", color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  # Adding animation to the plot
  transition_time(year) +
  geom_text(x = 75000, y = 200, colour = "white", size = 10)+
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

# Animate the visualisation
animate(p, width = 450, height = 450, duration = 10)

# Save the animation as a gif  
anim_save("gifoutput.gif")
