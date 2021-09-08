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

# Total months with animal type.
ggplot(data = animal_clean_df) +
  geom_bar(mapping = aes(x = month, fill = animal_group_recode))

# String tricks.

# Involving water?
water_df <- animal_clean_df %>% 
  filter(str_detect(special_service_type, "water|Water"))

animal_clean_df %>%
  mutate(water_or_not = if_else(str_detect(special_service_type, "water|Water"), "yes", "no")) %>% 
  group_by(water_or_not) %>% 
  summarise(counts = n())

# Splitting columns.
animal_clean_df <- animal_clean_df %>% 
  separate(col = special_service_type, into = c("desc1", "desc2"), sep = "-", remove = FALSE)

# Uniting columns.
animal_clean_df <- animal_clean_df %>% 
  unite(col = "new_date", month, cal_year, sep = " ", remove = FALSE)

# Download shapefiles.
download.file(url = "https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip",
              destfile = "data/london_gis.zip")

# Unzip.
unzip(zipfile = "data/london_gis.zip", exdir = "data")

# Load in shapefiles.
bor_sf <- st_read("data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")

# Check.
ggplot(data = bor_sf) +
  geom_sf()

# Check lengths.
length(unique(bor_sf$GSS_CODE))
length(unique(animal_clean_df$borough_code))

# Which ones?
`%nin%` <- Negate(`%in%`)

missbor_df <- animal_clean_df %>% 
  select(borough_code, borough) %>% 
  distinct(borough_code, borough) %>% 
  filter(borough_code %nin% bor_sf$GSS_CODE)

# Drop those incidents which occur outside of these, and tidy for join.
animal_clean_ldn_df <- animal_clean_df %>% 
  filter(borough_code %in% bor_sf$GSS_CODE) %>%
  group_by(borough_code, animal_group_recode) %>% 
  summarise(counts = n()) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = borough_code, values_from = counts, names_from = animal_group_recode)
  
# Make the join.
animal_clean_ldn_sf <- left_join(bor_sf, animal_clean_ldn_df,
                                 by = c("GSS_CODE" = "borough_code"))

# Basic map.
ggplot(data = animal_clean_ldn_sf) +
  geom_sf(mapping = aes(fill = Bird), colour = "transparent") +
  scale_fill_viridis_c() +
  labs(fill = NULL, title = "LFB bird incidents, 2009-2021") +
  theme_minimal() 