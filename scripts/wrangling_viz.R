# Load libraries.
library(readr)
library(dplyr)
library(janitor)
library(tidyr)
library(forcats)
library(stringr)
library(lubridate)
library(sf)
library(ggplot2)

# Load data.
animal_df <- read_csv("data/animal_rescues.csv", na = c("NULL","NA"))

# Initial checks.
dim(animal_df)
names(animal_df)
glimpse(animal_df)
head(animal_df)
tail(animal_df)
sum(is.na(animal_df)) 

# Initial cleaning, dealing with dates and time.
animal_clean_df <- animal_df %>% 
  clean_names() %>% 
  filter(cal_year != 2021) %>% 
  select(-type_of_incident) %>% 
  mutate(date_time_lub  = dmy_hm(date_time_of_call),
         date_time_lubr = round_date(date_time_lub, unit = "hour"),
         month_year     = round_date(date_time_lub, unit = "month"),
         time_lubr      = hour(date_time_lubr),
         week_day       = wday(date_time_lubr, label = TRUE, week_start = 1),
         month          = month(date_time_lubr, label = TRUE)) 

# Demonstrate.

# Time of day.
ggplot(data = animal_clean_df) +
  geom_bar(mapping = aes(x = time_lubr)) 

# Months.
ggplot(data = animal_clean_df) +
  geom_bar(mapping = aes(x = month))

# Explore animal types and frequencies.
freq_df <- animal_clean_df %>% 
  group_by(animal_group_parent) %>% 
  summarise(counts = n()) 

# Recode animal types. 
animal_clean_df <- animal_clean_df %>% 
  mutate(animal_group_parent = if_else(animal_group_parent == "cat", "Cat", animal_group_parent),
         animal_group_recode = fct_lump(animal_group_parent, 5))

# Demonstrate.

# Months with animal type.
ggplot(data = animal_clean_df) +
  geom_bar(mapping = aes(x = month, fill = animal_group_recode))

# Download shapefiles.
download.file(url = "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/08d31995-dd27-423c-a987-57fe8e952990/London-wards-2018.zip",
              destfile = "data/london_wards.zip")

# Unzip.
unzip(zipfile = "data/london_wards.zip", exdir = "data")

# Load in shapefiles.
wards_sf <- st_read("data/London-wards-2018_ESRI/London_Ward.shp")

# Check lengths.
length(unique(wards_sf$GSS_CODE))
length(unique(animal_clean_df$ward_code))

# Aggregate the data we want to map out.
animals_aggregate_df <- animal_clean_df %>% 
  group_by()
  



