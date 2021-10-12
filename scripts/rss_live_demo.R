# Load packages
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(forcats)

# Load data
animals_df <- read_csv(file = "data/animal_rescues.csv", na = c("NULL", "NA"))

# Explore
glimpse(animals_df)
dim(animals_df)
head(animals_df)
tail(animals_df)
sum(is.na(animals_df$cal_year))

max(animals_df$cal_year)

# Clean data.
animals_clean_df <- animals_df %>% 
  clean_names() %>% 
  select(incident_number, date_time_of_call, cal_year, animal_group_parent,
         borough, borough_code) %>% 
  filter(cal_year != 2021) %>% 
  mutate(date_time_lub = dmy_hm(date_time_of_call),
         hour_lub      = hour(date_time_lub),
         month_lub     = month(date_time_lub, label = TRUE),
         week_day      = wday(date_time_lub, label = TRUE))

# Graphic.
ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = hour_lub))

ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = month_lub))

# Freq.
freq_df <- animals_clean_df %>% 
  group_by(animal_group_parent) %>% 
  summarise(counts = n())

# Recoding.
animals_clean_df <- animals_clean_df %>% 
  mutate(animal_recode = if_else(condition = animal_group_parent == "cat",
                                 true = "Cat",
                                 false = animal_group_parent),
         animal_recode2 = fct_lump(animal_recode, 5))

freq_df <- animals_clean_df %>% 
  group_by(animal_recode2) %>% 
  summarise(counts = n())

# Visualisation.
ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = month_lub, fill = animal_recode2))

