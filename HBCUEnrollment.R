library(tidyverse)
library(ggtext)
library(Cairo)

# Import data
hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv')

# Data Wrangling
hbcu_data<- hbcu_all%>%
  select(Year, Males, Females)%>%
  pivot_longer(cols = c(Males, Females), names_to = "Gender", values_to = "Enrollment")%>%
  mutate(Year = as.factor(Year),
         Gender = as.factor(Gender))

# Add empty bars to dataframe to create space for "y" axis
empty_bar <- 2
empty_df <- data.frame(matrix(NA, empty_bar*2, 3))
colnames(empty_df) <- colnames(hbcu_data)
hbcu_data2<-rbind(hbcu_data, empty_df)
hbcu_data2$id <- rep(seq(1, nrow(hbcu_data2)/2), each = 2)

# Label data
label_data <- hbcu_data2%>% 
  group_by(id, Year)%>%
  summarize(total = sum(Enrollment))
angle <- 90 - 360 * (label_data$id-0.5)/nrow(label_data)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
label_data<-label_data%>%
  mutate(label = ifelse((Year %in% c(1976,1986,1990,1995,2000,2004,2008,2012,2015)),
                        as.character(Year), NA))


# Visualisation
p<-ggplot(hbcu_data2)+
  geom_col(aes(x = as.factor(id), y = Enrollment, fill = Gender))+
  # Edit negative numbers to adjust size of inner circle
  ylim(-150000, max(label_data$total, na.rm = TRUE))+
  # Create "y-axis" in the empty bar space
  geom_segment(aes(x = 32.5, y = 0, xend = 34.5, yend = 0), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE)+
  geom_segment(aes(x = 32.5, y = 100000, xend = 34.5, yend = 100000), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE)+
  geom_segment(aes(x = 32.5, y = 200000, xend = 34.5, yend = 200000), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE)+
  geom_segment(aes(x = 32.5, y = 300000, xend = 34.5, yend = 300000), 
               colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE)+
  # Label "y-axis"
  annotate("text", x = rep(34,4), y = c(0, 100000, 200000, 300000),
           label = c("0", "100", "200", "300"), color = "#111111", size = 6, angle = 0,
           fontface = "bold", hjust = 0.8)+
  # Label the years
  geom_text(data = label_data, aes(x = id, y = total+10000, label = label, hjust = hjust),
            angle = label_data$angle, size = 7, colour = "#555555", inherit.aes = FALSE)+
  # Base grid lines
  geom_segment(aes(x = 1, y = -5, xend = 32, yend = -5), 
               colour = "black", alpha=1, size=0.3 , inherit.aes = FALSE)+
  # Colour palette
  scale_fill_manual(values = c("#3b5998", "#8b9dc3"))+
  labs(title = "HBCU College Enrollment",
       subtitle = "Student enrollment in historically black colleges & universities (HBCU) from 1976-2015.Breakdown of <span style = 'color:#3b5998'><b>females</b></span> and <span style = 'color:#8b9dc3'><b>males</b></span>.<br>(numbers in thousands)",
       caption = "Source: Data.World | Visualisation: @nxrunning")+
  theme_minimal()+
  coord_polar()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "#dfe3ee", colour = "#dfe3ee"),
        plot.title = element_text(size = 30, face = "bold", colour = "#011f4b"),
        plot.caption = element_text(size = 15, hjust = 0.5, colour = "#2c2f33", face = "bold"),
        plot.subtitle = element_textbox_simple(
          size = 17,
          color = "#111111",
          padding = margin(5,0,5,0),
          margin = margin(0,0,0,0)))

# Save the plot  
ggsave(p, filename = "HBCUEnrollment.png", width = 18, height = 28, units = "cm", dpi = 300,
       type = "cairo-png")
