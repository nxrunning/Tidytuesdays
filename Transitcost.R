# Import libraries
library(tidyverse)
library(ggstatsplot)

# Import data
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# Data Wrangling
df <- transit_cost%>%
  na.omit()%>%
  mutate(tunnel_percentage = parse_number(tunnel_per),
         real_cost = as.numeric(real_cost),
         tunnel_group = ifelse(tunnel_percentage >= 100, 2, ifelse(tunnel_percentage == 0, 0, 1)))%>%
  mutate(tunnel_group = factor(tunnel_group, levels = c(0,1,2),
                                  labels = c("100% above-ground", "Partial underground", 
                                             "100% underground")))

# Data Visualisation
p<-df%>%
  filter(between(real_cost, quantile(df$real_cost, 0.25) - 1.5* IQR(df$real_cost), 
                 quantile(df$real_cost, 0.75) + 1.5* IQR(df$real_cost)))%>%
  ggbetweenstats(x = tunnel_group, y = real_cost, type = "robust", k = 2, 
                 mean.point.args = list(size = 5, color = "darkblue"))+
  ylim(0, 15000)+
  labs(title = "Partially underground transit lines more costly than\ncompletely above-ground and underground lines",
       caption = "Source: TransitCosts.com | Visualisation: @nxrunning",
       x = "",
       y = "Cost (Millions of USD)")+
  scale_color_manual(values = c("#245023", "#342447", "#cc2517"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 8))

# Save the plot
ggsave(p, dpi = 600, filename = "Transitcosts.jpeg")


