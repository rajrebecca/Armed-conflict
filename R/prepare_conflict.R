# CHL8010 Week 3 In-Class Activity
# Author: Rebecca Raj
# Date: Mon. Sept. 23, 2024


### USING CONFLICT DATAFILE ####

library(here)

# Q2 Derive binary armed conflict variable 

raw_conflict <- read.csv(here("Original", "conflictdata.csv"), 
                     header = TRUE)

armed_conflict <- raw_conflict %>%
  group_by(ISO, year, conflict_id) %>% # Create a binary indicator for whether a country had a conflict that year
  summarize(armed_conflict = as.integer(any(best > 0, na.rm = TRUE)), .groups = 'drop') %>%
  mutate(year = year + 1)   # Adjust the year to account for the lag

# Outcome 1: Binary indicator of armed conflict (0 if <25, 1 if >= 25 battle related deaths) for each country-year

conflict %>%
  group_by(ISO, year) |>
  summarise(totdeath = sum(best)) |>
  mutate(armconf1 = ifelse(totdeath < 25, 0, 1)) |>
  ungroup() |>
  mutate(year = year + 1) -> confdata

confdata <- confdata %>% rename(Year = year)

# Outcome 2: Conflict intensity as categorical variable (0 = no, <25 battle-related deaths; 
#1 = minor conflict, 25-999 battle related deaths; 2 = war, >=1000 battle related deaths)

#mutate(confint = ifelse(totdeath < 25, 0, ifelse(totdeath < 1000, 1, 2)),
#       armconf2 = max(confint)) %>%
# dplyr::select(-confint, -best, -conflict_new_id) %>%
# slice(1) %>%
# ungroup -> confdata

table(confdata$armconf1)

head(confdata)
