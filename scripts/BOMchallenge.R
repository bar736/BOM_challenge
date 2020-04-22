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

##also possible : combined_data <- tidy_bom_stations %>% full_join(data_bom_separated, by= c("Station_number"="Station_number"))

answer3 <-  filter(combined_data, min_temp != "-", max_temp != "-") %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(temp_diff = max_temp - min_temp) %>% 
  group_by(state) %>% 
  summarise(average = mean(temp_diff)) %>% 
  arrange(average) %>% 
  slice(1)

    #the answer is QLD 7.36

    #alternative script to challenge 3 using the whole dataset that produces NA and then
    #removing NA with na.rm

answer3 <-  combined_data %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(temp_diff = max_temp - min_temp) %>% 
  group_by(state) %>% 
  summarise(average = mean(temp_diff, na.rm = TRUE)) %>% 
  arrange(average) %>% 
  slice(1)

    #the answer is QLD 7.36



#Question 4 Does the westmost (lowest longitude) or eastmost (highest longitude) 
#weather station in our dataset have a higher average solar exposure?


answer4 <-  combined_data %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(lon = as.numeric(lon)) %>%
  group_by(Station_number,  lon) %>% 
  summarise(average_solar_exp = mean(Solar_exposure, na.rm = TRUE)) %>% 
  arrange(lon) %>% 
  ungroup() %>% 
  filter(lon==min(lon) | lon==max(lon))  #| menas "or"  also remember 1==2  FALSE
  
  #answer3:  top is Station 9194  lon 116.              ave solar 19.2
  #answer3:  top is Station 40043 lon 153.              ave solar 19.5

#alternative script to deal with challenge 4 using mutate instead of summarise

answer4 <-  combined_data %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(lon = as.numeric(lon)) %>%
  group_by(Station_number) %>% 
  mutate(average_solar_exp = mean(Solar_exposure, na.rm = TRUE)) %>% 
  arrange(lon) %>% 
  ungroup() %>% 
  filter(lon==min(lon) | lon==max(lon)) %>% 
  select(Station_number, lon, average_solar_exp) %>% 
  distinct()



#DATA VISUALIZATION EASTER CHALLENGE
#Q1: For the Perth station (ID 9225), produce three scatter plots 
#showing the relationship between the maximum temperature and 
#minimum temperature, rainfall and solar exposure.

Q1_plot1 <- combined_data %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(Rainfall = as.numeric (Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric (Solar_exposure)) %>% 
  filter(Station_number == "9225") %>%
  ggplot(aes(x=max_temp, y=min_temp))+ 
  geom_point()
#Q1_plot1 + labs(title = "Max temp vs Min temp in Perth")

Q1_plot2 <- combined_data %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(Rainfall = as.numeric (Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric (Solar_exposure)) %>% 
  filter(Station_number == "9225") %>%
  ggplot(aes(x=max_temp, y=Rainfall))+ 
  geom_point()
#Q1_plot2 + labs(title = "Max temp vs Rainfall in Perth")


Q1_plot3 <- combined_data %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(Rainfall = as.numeric (Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric (Solar_exposure)) %>% 
  filter(Station_number == "9225") %>%
  ggplot(aes(x=max_temp, y=Solar_exposure))+ 
  geom_point()
#Q1_plot3 + labs(title = "Max temp vs Solar exposure in Perth")



#Q2: Display these four measurements for the Perth station in a
#single scatter plot by using additional aesthetic mappings.


Q2_plot4 <- combined_data %>% 
  mutate(min_temp = as.numeric (min_temp)) %>% 
  mutate(max_temp = as.numeric (max_temp)) %>% 
  mutate(Rainfall = as.numeric (Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric (Solar_exposure)) %>% 
  filter(Station_number == "9225") %>%
  ggplot(aes(x=max_temp, y=min_temp, colour = Solar_exposure, size = Rainfall))+ 
  geom_point()
#Q2_plot4 + labs(title = "Temperature, Rainfall and Solar exposure in Perth")


#Q3: Take the four plots you have produced in Q1 and Q2 
#and save them as a multi-panel figure.

library(cowplot)

Q3_plot5 <- plot_grid(Q1_plot1, Q1_plot2, Q1_plot3, Q2_plot4, rel_heights = c(1, 1),
          rel_widths = c(1, 1), labels = "AUTO")

ggsave(filename = "Results/Q3plot.png", plot = Q3_plot5,
       width = 22, height = 18, dpi = 300, units = "cm")



#Q4: Using the entire BOM dataset, calculate the average monthly rainfall 
# for each station. Produce a lineplot to visualise this data and the state 
# each station is in.


rainfall_bymonth <- combined_data %>% 
  mutate(Rainfall = as.numeric(Rainfall)) %>%
  group_by(Station_number,  Month, state) %>% 
  summarise(averaged_monthly_rainfall = mean(Rainfall, na.rm = TRUE))

#facets
Q4_plot6_facets <- rainfall_bymonth %>%
  ggplot(aes(x=Month, y=averaged_monthly_rainfall, 
             group = Station_number))+ 
  geom_line(size=1)+
  facet_wrap( ~ state)

Q4_plot6_facets +    
  labs(title = "Average monthly rainfall in each station",
       caption = 'Data source: BOM data',
       x = "Month",
       y = "Average rainfall (mm)",
       colour = "State") +  
  scale_x_continuous(breaks = (1:12), labels = (month.abb))+
  scale_y_continuous(breaks = (0:10))+
  theme_bw()  +  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.background = element_blank(),
    panel.grid.major = element_line(size = 1),
    axis.title = element_text(size = 13, colour = "black"),
    legend.position = "right"
)
                     

#all together
Q4_plot6 <- rainfall_bymonth %>%
  ggplot(aes(x=Month, y=averaged_monthly_rainfall, 
             colour = state, group = Station_number))+ 
  geom_line(size=1)+
  scale_colour_brewer(palette = "Set1")


Q4_plot6 +    
  labs(title = "Average monthly rainfall in each station",
       caption = 'Data source: BOM data',
       x = "Month",
       y = "Average rainfall (mm)",
       colour = "State") +  
  scale_x_continuous(breaks = (1:12), labels = (month.abb))+
  scale_y_continuous(breaks = (0:10))+
  theme_bw()  +  
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.background = element_blank(),
    panel.grid.major = element_line(size = 1),
    axis.title = element_text(size = 13, colour = "black"),
    legend.position = "right"
  )
