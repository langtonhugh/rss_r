# Load packages.
# install.packages("tidyverse")
library(tidyverse)
library(janitor)
library(lubridate)

# Load in data.
animals_df <- read_csv("data/animal_rescues.csv", na = c("NULL", "NA"))

class(animals_df)

nrow(animals_df)
head(animals_df)

# Exploring.
glimpse(animals_df)

# Cleaning.
animals_clean_df <- animals_df %>% 
  clean_names() %>% 
  select(incident_number, date_time_of_call, cal_year,
         animal_group_parent, special_service_type, borough) %>% 
  filter(cal_year != "2021") %>% 
  mutate(date_lub = dmy_hm(date_time_of_call),
         hour_round_lub = round_date(date_lub, "hour"),
         hour_lub       = hour(hour_round_lub),
         month_lub      = month(hour_round_lub, label = TRUE),
         week_day       = wday(hour_round_lub, label = TRUE))

# Frequency.
freq_df <- animals_clean_df %>% 
  group_by(month_lub) %>% 
  summarise(counts = n()) %>% 
  ungroup()

# Visual.
ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = month_lub))
  
  












