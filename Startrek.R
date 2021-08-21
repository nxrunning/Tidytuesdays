# Import libraries
library(tidytext)
library(tidyverse)
library(patchwork)
library(showtext)
library(ggtext)

# Import data
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

# Import stop words data
data("stop_words")

# Dataframe on commands
command_df<-computer%>%
  select(line, type)%>%
  filter(type == "Command")%>%
  unnest_tokens(word, line)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  top_n(10)

# Dataframe on responses
response_df<-computer%>%
  select(line, type)%>%
  filter(type == "Response")%>%
  unnest_tokens(word, line)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE)%>%
  top_n(10)

# Font
font_add_google("Rambla", "rambla")
showtext_auto()

# Command plot
command_plot<-ggplot(command_df, aes(x = reorder(word, n), y = n))+
  geom_bar(stat = "identity", fill = "#ba1e68")+
  geom_text(aes(label = n), hjust = "right", size = 8, nudge_y = -5, colour = "#fcfbfe", fontface = "bold")+
  coord_flip()+
  labs(y = "",
       x = "")+
  theme(
    plot.background = element_rect(fill = "#1d1135", color = "#1d1135"),
    panel.background = element_rect(fill = "#1d1135", color = "#1d1135"),
    panel.border = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(colour = "#fcfbfe", face = "bold", size = 20),
    axis.text.x = element_text(colour = "#fcfbfe", size = 20),
    axis.ticks = element_blank())

# Response plot
response_plot<-ggplot(response_df, aes(x = reorder(word, n), y = n))+
  geom_bar(stat = "identity", fill = "#5643fd")+
  geom_text(aes(label = n), hjust = "right", size = 8, nudge_y = -1, colour = "#fcfbfe", fontface = "bold")+
  coord_flip()+
  labs(y = "",
       x = "")+
  theme(
    plot.background = element_rect(fill = "#1d1135", color = "#1d1135"),
    panel.background = element_rect(fill = "#1d1135", color = "#1d1135"),
    panel.border = element_rect(fill = NA, color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(colour = "#fcfbfe", face = "bold", size = 20),
    axis.text.x = element_text(colour = "#fcfbfe", size = 20),
    axis.ticks = element_blank())

# Combine both plots
combined_plot<-command_plot+response_plot+
  plot_layout(widths = c(2,1))+
  plot_annotation(title = "Frequency of top ten words in Star Trek",
                  subtitle = "<span style = 'color:#ba1e68'>Commands</span> vs <span style = 'color:#5643fd'>Responses</span>",
                  caption = "Source: Speechinteraction.org | Visualisation: @nxrunning | #TidyTuesday")&
  theme(plot.background = element_rect(fill = "#1d1135", color = "#1d1135"),
        plot.caption = element_text(colour = "#f7d967", hjust = 0.5, family = "rambla", size = 30),
        plot.title = element_text(colour = "#f7d967", family = "rambla", size = 50),
        plot.subtitle = element_markdown(size = 30, colour = "#fcfbfe"))

# Save plot
ggsave(combined_plot, width = 200, height = 112.4, units = "mm", filename = "Startrek.jpeg")
