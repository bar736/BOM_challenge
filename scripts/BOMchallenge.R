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

#Question 2
#Which month saw the lowest average daily temperature difference?

month_average_temp_diff <-  filter(data_bom_separated, min_temp >=0, max_temp >=0) %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(temp_diff = max_temp - min_temp) %>% 
  group_by(Month) %>% 
  summarise(average = mean(temp_diff))
  

  




