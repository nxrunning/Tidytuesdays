library(tidyverse)
library(ggfx)
library(ggtext)
library(showtext)

# Import data
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

# Dataframe with selected industries
df<-survey%>%
  filter(industry %in% c("Education (Higher Education)", "Computing or Tech", "Nonprofits"),
         annual_salary < quantile(annual_salary, 0.75)+ IQR(annual_salary))%>%
  mutate(industry = factor(industry, 
                           levels = c("Computing or Tech", "Nonprofits", "Education (Higher Education)")))

# Labels dataframe
labels <- data.frame(label = c("Education", "Tech", "Nonprofits"),
                 industry = c("Education (Higher Education)", "Computing or Tech", "Nonprofits"),
                 x = c(75000, 75000, 75000),
                 y = 0.000001)%>%
  mutate(industry = factor(industry, levels = c("Computing or Tech", "Nonprofits", "Education (Higher Education)")))

# Fonts
font_add_google("Fira Sans Condensed", "Fira")
font_add_google("Roboto Slab", "Roboto Slab")
showtext_auto()

# Plot
p1<-ggplot(df1)+
  as_reference(geom_text(data = labels, aes(x = x, y = y, label = label), family = "Roboto Slab", colour = "white", size = 60, hjust = 0.5, vjust = 0), 
               id = "text")+
  with_blend(geom_density(aes(x = annual_salary, fill = industry, colour = industry), show.legend = FALSE), 
             bg_layer = "text", blend_type = "xor")+
  labs(x = "Annual Salary ($)",
       y = NULL,
       title = "Salary distribution across industries",
       caption = "**Source**: Ask a Manager | **Visualisation**: @nxrunning")+
  facet_wrap(~industry, ncol = 1)+
  scale_fill_manual(values = c("#1E656D", "#F1F3CE", "#F62A00"))+
  scale_color_manual(values = c("#1E656D", "#F1F3CE", "#F62A00"))+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "white"),
        axis.text.x = element_text(color = "white", size = 50),
        axis.title.x = element_text(color = "white", size = 50),
        strip.text = element_blank(),
        plot.background = element_rect(fill = "#00293C"),
        panel.background = element_rect(fill = "#00293C"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_markdown(colour = "white", family = "Fira", size = 50),
        plot.title = element_markdown(hjust = 0.5, family = "Fira", colour = "white", size = 120))
  
# Save plot
ggsave(p1, height = 9, width = 14, units = 'in', filename="Askamanager.jpeg")
