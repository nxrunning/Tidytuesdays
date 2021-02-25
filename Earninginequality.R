library(tidyverse)
library(ggtext)

# Import data
earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

# Data Wrangling
ribbon_data_black<-earn%>%
  filter(race == "Black or African American")%>%
  filter(sex != "Both Sexes")%>%
  filter(age == "25 years and over")%>%
  group_by(sex, year)%>%
  summarise(earn = mean(median_weekly_earn))%>%
  pivot_wider(names_from = sex, values_from = earn)%>%
  mutate(pct_diff = ((Men - Women)/Women)*100)
  
ribbon_data_white<-earn%>%
  filter(race == "White")%>%
  filter(sex != "Both Sexes")%>%
  filter(age == "25 years and over")%>%
  group_by(sex, year)%>%
  summarise(earn = mean(median_weekly_earn))%>%
  pivot_wider(names_from = sex, values_from = earn)%>%
  mutate(pct_diff = ((Men - Women)/Women)*100)
  
line_data_black<-earn%>%
  filter(race == "Black or African American")%>%
  filter(sex != "Both Sexes")%>%
  filter(age == "25 years and over")%>%
  group_by(sex, year)%>%
  summarise(earn = mean(median_weekly_earn))

line_data_white<-earn%>%
  filter(race == "White")%>%
  filter(sex != "Both Sexes")%>%
  filter(age == "25 years and over")%>%
  group_by(sex, year)%>%
  summarise(earn = mean(median_weekly_earn))
  
# Visualisation
(p<-ggplot()+
  geom_ribbon(data = ribbon_data_black, aes(x = year, ymin = Women, ymax = Men), fill = "#3CAEA3")+
  geom_ribbon(data = ribbon_data_white, aes(x = year, ymin = Women, ymax = Men), fill = "#20639B")+
  geom_line(data = line_data_black, aes(x = year, y = earn, colour = sex), size = 1)+
  geom_line(data = line_data_white, aes(x = year, y = earn, colour = sex), size = 1)+
  scale_colour_manual(values = c("#F6D55C", "#ED553B"))+
  scale_y_continuous(labels = scales::dollar,
                     limits = c(600, 1350),
                     breaks = c(600, 800, 1000, 1200))+
  scale_x_continuous(labels = c(2010,2012,2014,2016,2018,2020),
                     limits = c(2008.5, 2021),
                     breaks = c(2010,2012,2014,2016,2018,2020))+
  annotate("segment", x = 2009.8, xend = 2009.8, y = 614.25, yend = 666, arrow = arrow(length = unit(0.1, "cm"), ends = "both" , type = "closed"), colour = "white")+
  annotate("segment", x = 2009.8, xend = 2009.8, y = 719, yend = 899.25, arrow = arrow(length = unit(0.1, "cm"), ends = "both" , type = "closed"), colour = "white")+
  annotate("segment", x = 2020.2, xend = 2020.2, y = 796.25, yend = 882.5, arrow = arrow(length = unit(0.1, "cm"), ends = "both" , type = "closed"), colour = "white")+
  annotate("segment", x = 2020.2, xend = 2020.2, y = 946, yend = 1166, arrow = arrow(length = unit(0.1, "cm"), ends = "both" , type = "closed"), colour = "white")+
  annotate("text", x = 2009, y = (614.25+666)/2, label = "8.4%", colour = "white")+
  annotate("text", x = 2009, y = (719+899.25)/2, label = "25.1%", colour = "white")+
  annotate("text", x = 2021, y = (796.25+882.5)/2, label = "10.8%", colour = "white")+
  annotate("text", x = 2021, y = (946+1166)/2, label = "23.3%", colour = "white")+
  annotate(GeomRichtext, x = 2008.5, y = 1350, label = "**GENDER X RACE INEQUALITY**",
           hjust = 0, label.colour = NA, size = 5, fill = "#173F5F", colour = "#FFBD1B")+
  annotate(GeomRichtext, x = 2008.5, y = 1330, label = "Gender and racial disparities in earnings persist over the last decade.<br>On average, median weekly earnings of <span style = 'color:#3CAEA3'>Blacks</span> (25 years and above)<br>were lower than their <span style = 'color:#20639B'>White</span> counterparts. <span style = 'color:#F6D55C'>Men</span> also generally earned<br>more than <span style = 'color:#ED553B'>women</span> in both races.",
           hjust = 0, vjust = 1 ,size = 3, label.colour = NA, fill = "#173F5F", colour = "white")+
  labs(y = "Median Weekly Earnings",
       x = "",
       caption = "Source: U.S. Bureau of labor statistics<br>Visualisation: @nxrunning")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#173F5F"),
        plot.background = element_rect(fill = "#173F5F"),
        legend.position = "none",
        axis.title = element_text(colour = "white"),
        axis.ticks = element_line(colour = "white"),
        axis.text = element_text(colour = "white"),
        plot.caption = element_markdown(size = 10, hjust =0.5, colour = "#FFBD1B")))

# Save the plot
ggsave(p, dpi = 600, width = 15.875, height = 8.86, units = "cm", filename = "Earninginequality.jpeg")
