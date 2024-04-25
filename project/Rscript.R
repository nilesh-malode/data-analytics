install.packages("tidyverse")
library(tidyverse)

library(ggplot2)
library(dplyr)
library(sf)

data_right <- read.csv("/cloud/project/DataFiles/data_right_3.csv")
unicef_metadata <- read.csv("/cloud/project/DataFiles/unicef_metadata.csv")
unicef_indicator <- read.csv("/cloud/project/DataFiles/unicef_indicator_1 (1).csv")


#World Map
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)

# Filter data for 'Total' sex only
filtered_data <- unicef_indicator %>%
  filter(sex == "Total")

world <- ne_countries(scale = "medium", returnclass = "sf")

world_data <- merge(world, filtered_data, by.x = "name", by.y = "country", all.x = TRUE)

ggplot(data = world_data) +
  geom_sf(aes(fill = obs_value), color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Observation Value",
                       limits = c(min(world_data$obs_value, na.rm = TRUE), 
                                  max(world_data$obs_value, na.rm = TRUE)),
                       na.value = "grey50", guide = guide_colourbar(direction = "vertical")) +
  labs(title = "World Map Showing Out of School Rate",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#Scatter plot

african_countries_data <- merge(data_right, unicef_metadata, by = "country") %>%
  filter(continent == "Africa")  

ggplot(african_countries_data, aes(x = Military_expenditure, y = GDP_per_capita, color = country)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Military Expenditure vs GDP per Capita for African Countries",
       x = "Military Expenditure (% of GDP)",
       y = "GDP per Capita (constant 2015 US$)") +
  theme_minimal() 



#bar_graph



african_countries <- data_right %>%
  filter(continent == "Africa") %>%
  select(country) %>%
  distinct()


inflation_data <- unicef_metadata %>%
  filter(year == 2015, `country` %in% african_countries$country) %>%
  select(country, Inflation)


inflation_data$Inflation <- as.numeric(as.character(inflation_data$Inflation))


ggplot(inflation_data, aes(x = country, y = Inflation, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Inflation Rates for african Countries in 2015",
       x = "Country",
       y = "Inflation Rate (%)",
       caption = "Data source: UNICEF Metadata") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Print the plot print(gg) # Save the plot to a file ggsave("bar_graph.png", gg, width = 12, height = 6)


#Time series


life_expectancy_data <- unicef_metadata %>%
  filter(year == 2015, country %in% african_countries$country) %>%
  select(country, life_expectancy)


life_expectancy_data$life_expectancy <- as.numeric(as.character(life_expectancy_data$life_expectancy))

ggplot(life_expectancy_data, aes(x = country, y = life_expectancy, group = 1)) +
  geom_line() +  
  geom_point() +  
  theme_minimal() +
  labs(title = "Life Expectancy in african Countries in 2015",
       x = "Country",
       y = "Life Expectancy (years)",
       caption = "Data source: UNICEF Metadata") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
