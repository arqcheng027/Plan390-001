# load libraries
library(tidyverse)
library(lubridate)
# Read file
data = read_csv("data/restaurant_inspections.csv")
data = filter(data, !is.na(SCORE))
#Create histogram of overall score distribution
ggplot(data, aes(x=SCORE)) +
  geom_histogram() + xlim(c(75,100))

# Group by date of finding 
data$year = year(data$RESTAURANTOPENDATE)
score_by_year = group_by(data, year) %>%
  summarize(Score = mean(SCORE), na.rm=T)

ggplot(score_by_year, aes(x=year, y=Score)) +
  geom_bar(stat = "identity")  
            
# Group by city
data$City = str_to_upper(data$CITY)
data_city = group_by(data, City) %>%
  summarize(Score=mean(SCORE))
ggplot(data_city, aes(x=City, y=Score)) +
  geom_bar(stat = "identity")     

# Group by inspector
data$INSPECTOR = str_to_upper(data$INSPECTOR)
data_inspect = group_by(data, INSPECTOR) %>%
  summarize(Score=mean(SCORE))
ggplot(data_inspect, aes(x=INSPECTOR, y=Score)) +
  geom_bar(stat = "identity")     

# Find how many data from each inspector
data_inspect_number = group_by(data, INSPECTOR, SCORE) %>%
  summarize(Score= mean(SCORE)) %>%
  pivot_wider(names_from=INSPECTOR, values_from=SCORE) 

# Group by facility type
data_facility = group_by(data, FACILITYTYPE) %>%
  summarize(Score=mean(SCORE))
ggplot(data_facility, aes(x=FACILITYTYPE, y=Score)) +
  geom_bar(stat = "identity")   


# Repeat for restaurant only
data_rest = filter(data, FACILITYTYPE == "Restaurant")
ggplot(rest_data, aes(x=SCORE)) +
  geom_histogram() + xlim(c(75,100))

# Group by date of finding 
score_by_year_rest = group_by(data_rest, year) %>%
  summarize(Score = mean(SCORE))

ggplot(score_by_year_rest, aes(x=year, y=Score)) +
  geom_bar(stat = "identity")  

# Group by city
data_city_rest = group_by(data_rest, City) %>%
  summarize(Score=mean(SCORE))
ggplot(data_city_rest, aes(x=City, y=Score)) +
  geom_bar(stat = "identity")     

# Group by inspector
data_inspect_rest = group_by(data_rest, INSPECTOR) %>%
  summarize(Score=mean(SCORE))
ggplot(data_inspect_rest, aes(x=INSPECTOR, y=Score)) +
  geom_bar(stat = "identity")   

# Find how many data from each inspector
data_inspect_number_rest = group_by(data_rest, INSPECTOR, SCORE) %>%
  summarize(Score= mean(SCORE)) %>%
  pivot_wider(names_from=INSPECTOR, values_from=SCORE) 
