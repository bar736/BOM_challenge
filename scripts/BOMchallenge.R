library(tidyverse)

read_csv('data/BOM_data.csv')
read_csv('data/BOM_stations.csv')

bom_data <- read_csv('data/BOM_data.csv')
bom_stations <- read_csv('data/BOM_stations.csv')

view(bom_data)

#For each station, how many days have a minimum temperature, 
#a maximum temperature and a rainfall measurement recorded?

separate(bom_data, Temp_min_max, into = c('min_temp', 'max_temp'), sep = "/")

data_bom_separated <- 
  separate(bom_data, Temp_min_max, into = c('min_temp', 'max_temp'), sep = "/")

filter(data_bom_separated, min_temp >=0, max_temp >=0, Rainfall >=0) %>% 
  group_by(Station_number) %>% 
  summarise(num_row = n())

Question1_answer <- filter(data_bom_separated, min_temp >=0, max_temp >=0, Rainfall >=0) %>% 
  group_by(Station_number) %>% 
  summarise(num_row = n())





