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

answer1 <- filter(data_bom_separated, min_temp != "-", max_temp != "-", Rainfall != "-") %>% 
  group_by(Station_number) %>% 
  summarise(num_row = n())



  #answer is 20 stations with xxxx number is days


#Question 2
#Which month saw the lowest average daily temperature difference?

answer2 <-  filter(data_bom_separated, min_temp != "-", max_temp != "-") %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(temp_diff = max_temp - min_temp) %>% 
  group_by(Month) %>% 
  summarise(average = mean(temp_diff)) %>% 
  arrange(average) %>% 
  slice(1)

  

  #the answer is JUNE = 8.74


#Question 3  Which state saw the lowest average daily temperature difference?

tidy_bom_stations <- bom_stations %>% 
  gather(key = Station_number, value = ammount, -info) %>% 
  spread(key = info, value = ammount) %>% 
  mutate(Station_number = as.numeric(Station_number))

  
combined_data <- full_join(tidy_bom_stations, data_bom_separated, by= c("Station_number"="Station_number"))


answer3 <-  filter(combined_data, min_temp != "-", max_temp != "-") %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(temp_diff = max_temp - min_temp) %>% 
  group_by(state) %>% 
  summarise(average = mean(temp_diff)) %>% 
  arrange(average) %>% 
  slice(1)

    #the answer is QLD 7.36

