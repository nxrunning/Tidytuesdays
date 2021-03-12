library(tidyverse)
library(ggtext)
library(waffle)
library(patchwork)
library(showtext)

# Import data
movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')

# Data wrangling
df<-movies%>%
  select(year, title, clean_test)%>%
  mutate(decades = case_when(
    year < 1980 ~ "1970s",
    year >= 1980 & year < 1990 ~ "1980s",
    year >= 1990 & year < 2000 ~ "1990s",
    year >= 2000 & year <= 2005 ~ "2000-2005",
    year > 2005 & year <= 2010 ~ "2006-2010",
    year > 2010 & year <= 2013 ~ "2011-2013"))%>%
  mutate(clean_test = as.factor(clean_test),
         clean_test = fct_relevel(clean_test,c("nowomen","notalk","men","dubious", "ok")))%>%
  count(decades, clean_test)

# Palette colours
palette <- c("ok" = "#0b3475", "dubious" = "#3c5c90", "men" = "#d6b3a9",
             "notalk" = "#df866d", "nowomen" = "#e05425")

# Fonts
font_add_google("Roboto Condensed")
font_add_google("Roboto")
showtext_auto()

# Visualisation
p<-ggplot(df, aes(fill = clean_test, values = n/5))+
  geom_waffle(colour = "white", size =.25, n_rows = 4, flip = TRUE)+
  facet_wrap(~decades, nrow = 1, strip.position = "bottom")+
  scale_x_discrete()+
  scale_y_discrete()+
  scale_fill_manual(values = palette)+
  theme_void()+
  coord_equal()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", colour = "white"),
        panel.background = element_rect(fill = "white", colour = "white"),
        strip.text.x = element_text(size = 25))+
  plot_annotation(
    title = "Women Representation in Movies Over Time",
    subtitle = "The Bechdel test is a metric of women representation in fiction based on three criteria.<br>Movies <span style = 'color:#0b3475'><b>pass the test</b></span> if all three critera are satisifed. They are considered <span style = 'color:#3c5c90'><b>dubious</b></span> if it's skeptical to pass.<br>Movies fail the test if:<br>1.<span style = 'color:#d6b3a9'><b>Women characters only talk about men</b></span><br>2. <span style = 'color:#df866d'><b>Women characters don't talk to each other</b></span><br>3. <span style = 'color:#e05425'><b>Fewer than two named women characters</b></span><br><span style = 'font-size:25pt;'>*Each square represents five films</span>",
    caption = "<b>Source = FiveThirtyEight | Visualisation: @nxrunning</b>",
    theme = theme(
      plot.margin = margin(0,0,0,5, unit = "cm"),
      plot.title = element_markdown(family = "Roboto Condensed", size = 80, face = "bold"),
      plot.subtitle = element_markdown(size = 30, family = "Roboto", lineheight = 0.5),
      plot.caption = element_markdown(size = 25, family = "Roboto", hjust = 0.5, lineheight = 0.5)))


# Save the plot
ggsave(p, width = 10, height = 5, units = "in", filename = "Bechdeltest.jpeg")

