# CHL8010 Week 2 In-Class Activity Con't Version Control
# Author: Rebecca Raj
# Date: Mon. Sept. 16, 2024


install.packages("here")
library(here)

#read in disaster data
rawdat <- read.csv(here("original", "disaster.csv"), header = TRUE)

#filter data to include only the years 2000-2019 and the disaster types "Earthquake" and "Drought"

library(tidyverse)

data2 <- rawdat %>%
  filter(Year >= 2000 & Year <= 2019, Disaster.Type %in% c("Earthquake", "Drought"))

# subset the data to only include year, ISO and disaster type

subset_data <- data2 %>%
  select(Year, ISO, Disaster.Type)

# create dummy variables for drought and earthquake

data3 <- subset_data %>%
  mutate(drought = ifelse(Disaster.Type == "Drought", 1, 0),
         earthquake = ifelse(Disaster.Type == "Earthquake", 1, 0))

# group data so that there's only one row for each country and year

data4 <- data3 %>%
  group_by(Year, ISO) %>%
  summarize(drought = max(drought),
            earthquake = max(earthquake)) %>%
  ungroup()


