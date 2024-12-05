## Cleaning Sleep_Efficiency CSV file
library(tidyverse)
library(janitor)

# Load in the data
df <- read_csv("./csv/raw_data/Sleep_Efficiency.csv")

## Clean the data ####
# Clean the names of columns
df_clean <- clean_names(df)

# Combine REM, Light, and Deep sleep % together into 1 column.
df_clean <- df_clean %>% 
  pivot_longer(ends_with("sleep_percentage"),
               names_to = "sleep_type",
               values_to = "sleep_type_percentage") %>% 
  mutate(sleep_type = case_when(sleep_type == "rem_sleep_percentage" ~ "REM",
                                sleep_type == "deep_sleep_percentage" ~ "Deep",
                                sleep_type == "light_sleep_percentage" ~ "Light"))

# Fix the bedtime variable to only have time (not the date) 
df_clean$bedtime <- format(df_clean$bedtime, format = "%H:%M:%S")

# Separate out time into hour, minute, and second. Make a new column with only hour and minute
df_clean <- df_clean %>% 
  separate(bedtime, into = c("hour", "minute", "second"), sep = ":") %>% 
  mutate(bedtime = str_c(hour, minute)) %>% 
  select(-c("hour", "minute", "second", "wakeup_time")) # Remove unwanted columns. If I don't use more, I can add them.

# Change bedtime to numeric
df_clean$bedtime <- as.numeric(df_clean$bedtime)

# Add hours to the a.m. times so that it is continuous as though through the night and plots aren't off.
df_clean <- df_clean %>% 
  mutate(bedtime = if_else(bedtime < 1800, bedtime + 2400, bedtime))

## Save out to a csv file for easy use later ####
write_csv(df_clean, "./csv/clean_data/Sleep_Efficiency_Clean.csv")