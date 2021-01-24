#Import Libraries
library(tidyverse)
library(sf)
library(rKenyaCensus)
library(ggtext)
library(Cairo)

# Description of different datasets
data("DataCatalogue")

# Shapefiles of Kenya County boundaries from rKenyaCensus dataset
shp <- st_as_sf(rKenyaCensus::KenyaCounties_SHP)

# Distribution of population by age, sex and county
pop <- rKenyaCensus::V3_T2.3

# Calculate total population for respective agegroups for each County
# Omitting numbers not stated
age_grp_pop <- pop%>%
  filter(SubCounty == "ALL")%>%
  na.omit()%>%
  group_by(County, Age)%>%
  summarise(sum_peragegrp = sum(Total))%>%
  filter(Age != "Total")%>%
  filter(Age != "Not Stated")%>%
  separate(Age, into=c("Age", "column2"), sep = "-")%>%
  filter(is.na(column2))%>%
  select(-column2)%>%
  mutate(Age = ifelse(Age == "100+","100", Age))%>%
  mutate(Age = as.numeric(Age))%>%
  na.omit()%>%
  mutate(age_group = ifelse(Age >= 65, "Older_pop", "Younger_pop"))%>%
  group_by(County, age_group)%>%
  summarise(population = sum(sum_peragegrp))%>%
  pivot_wider(names_from = age_group, values_from = population)%>%
  mutate(total_pop = Older_pop + Younger_pop,
         older_perc = Older_pop/total_pop*100)

# Renaming certain county names to match the shp dataframe
age_grp_pop<-age_grp_pop%>%
  mutate(County = fct_recode(County, "ELGEYO/MARAKWET" = "ELGEYO-MARAKWET",
                             "NAIROBI CITY" = "NAIROBI",
                             "TAITA/TAVETA" = "TAITA/ TAVETA"))

# Join shp and age_grp_pop dataframes
plot_df <- st_as_sf(left_join(age_grp_pop, shp, by = "County"))

# Visualisation
p<-ggplot(plot_df) +
  geom_sf(aes(fill = older_perc), color = "black", size = .5) +
  scale_fill_gradient("Percentage of Older Adults (%)",low = "#ffd89b", high = "#19547b",
                      breaks=c(seq(0,9,1)))+
  labs(title = "Older Adults in Kenya",
       subtitle = "Kenya has about 1.87 million older adults in a population of approximately 
47.56 million. Among the 47 counties, Murang'a has the highest density 
of older adults, with 8.48% of the population aged 65 years and above.",
       caption = "Source: rKenyaCensus | Visualisation: @nxrunning")+
  theme_void()+
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "bottom",
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white"),
        plot.title = element_text(colour = "#ffd89b", size = 30),
        plot.subtitle = element_text(colour = "white", size = 14),
        plot.caption = element_text(colour = "white", size = 15, hjust = 0.5),
        plot.margin = margin(1,1,1,1, unit = "cm"))+
  guides(fill = guide_colorsteps(barwidth = 20, barheight = .5,
                                 title.position = "top", title.hjust = .5))

# Save the plot
ggsave(p, filename = "Kenyacensus.png", width = 18, height = 28, units = "cm", dpi = 300,
       type = "cairo-png")
