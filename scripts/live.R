# Load packages.
library(readr)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(forcats)
library(stringr)
library(sf)
library(tidyr)

# Load data.
animals_df <- read_csv("data/animal_rescues.csv", na = c("NULL", "NA"))

# Explore data.
glimpse(animals_df)
head(animals_df)
tail(animals_df)
sum(is.na(animals_df))
max(animals_df$cal_year)

# Initial cleaning.
animals_clean_df <- animals_df %>% 
  clean_names() %>% 
  select(incident_number, date_time_of_call, animal_group_parent, 
         borough, cal_year, special_service_type, borough_code) %>% 
  filter(cal_year != 2021) %>% 
  mutate(date_time_lub = dmy_hm(date_time_of_call),
         date_time_rou = round_date(date_time_lub, "hour"),
         hour_lub      = hour(date_time_rou),
         month_lub     = month(date_time_lub, label = TRUE, abbr = FALSE),
         week_day      = wday(date_time_lub, label = TRUE, week_start = 1) )

# Check temporal pattering.
ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = hour_lub))

ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = week_day))

ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = month_lub)) +
  facet_wrap(~cal_year)

# Check animal categories.
sort(unique(animals_clean_df$animal_group_parent))

# Recode category.
animals_clean_df <- animals_clean_df %>% 
  mutate(animal_recode = if_else(condition = animal_group_parent == "cat",
                                 true = "Cat",
                                 false = animal_group_parent),
         animal_recode = fct_lump(animal_recode, 4))

# Check animal categories.
sort(unique(animals_clean_df$animal_recode))

animals_clean_df %>% 
  group_by(animal_recode) %>% 
  summarise(freq_counts = n())

# Extend previous graphs.
ggplot(data = animals_clean_df) +
  geom_bar(mapping = aes(x = month_lub, fill = animal_recode))

# String detect.
rescues_df <- animals_clean_df %>% 
  filter(str_detect(special_service_type, "Animal rescue"))

# Download/unzip data.

# Load in shapefile.
borough_sf <- st_read("data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

glimpse(borough_sf)
glimpse(rescues_df)

sort(unique(rescues_df$borough)) # not good.

# By code.
`%nin%` <- Negate(`%in%`)

bor_check <- rescues_df %>% 
  distinct(borough_code) %>% 
  filter(borough_code %nin% borough_sf$GSS_CODE)

# Subset incidents which are not in the 'official data'/
rescues_ldn_df <- rescues_df %>% 
  filter(borough_code %in% borough_sf$GSS_CODE)

length(unique(rescues_ldn_df$borough_code))

# Aggregate for join.
rescues_ldn_agg_df <- rescues_ldn_df %>% 
  group_by(animal_recode, borough_code) %>% 
  summarise(counts = n()) %>% 
  pivot_wider(id_cols = borough_code, names_from = animal_recode,
              values_from = counts)
  
# Join.
rescues_ldn_agg_sf <- left_join(borough_sf, rescues_ldn_agg_df,
                                by = c("GSS_CODE" = "borough_code"))

# Plot.
ggplot(data = rescues_ldn_agg_sf) +
  geom_sf(mapping = aes(fill = Bird))





