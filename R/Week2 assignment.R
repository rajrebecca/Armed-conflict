# CHL8010 Week 2 In-Class Activity
# Author: Rebecca Raj
# Date: Mon. Sept. 16, 2024


install.packages("here")
library(here)

#read in maternalmortality data
rawdat <- read.csv(here("original", "maternalmortality.csv"), header = TRUE)

library(tidyverse)



subset <-rawdat %>%
  select (Country.Name, X2000:X2019)

#convert dataset into long format

data2 <- subset %>%
  pivot_longer (cols = X2000:X2019, #Select columns
                names_to = "Year",  #Rename selected columns to 'Year'
                names_prefix = "X", #Remove the X in the selected columns
                values_to = "MatMor") %>% #Change the 'count' to 'MatMor'
  mutate(Year = as.numeric(Year)) #store 'Year' as numeric

write.csv(data2, here("data", "cleandata.csv"), row.names = FALSE)

install.packages("usethis")

library(usethis) 



usethis::use_git_config(user.name = "rajrebecca", user.email = "rebecca.raj@mail.utoronto.ca")

# to confirm, generate a git situation-report, your user name and email should appear under Git config (global)
#usethis::git_sitrep()

#initiate and commit the files
usethis::use_git()

#add personal token
usethis::create_github_token()
gitcreds::gitcreds_set()

#usethis::use_github()

#usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)

#usethis::use_github()


## CHL8010: Special Topics 
# Week 3 In-Class Assignment
# Sept. 23, 2024
# Author: Rebecca Raj

library(here)

#Read in datasets
conflict <- read.csv(here("original", "conflictdata.csv"),
                   header = TRUE)

covariate <- read.csv(here("original", "covariates.csv"), header=TRUE)

disaster <- read.csv(here("original", "disaster.csv"), header=TRUE)

infantmort <- read.csv(here("original", "infantmortality.csv"), header=TRUE)

neonatmort <- read.csv(here("original", "neonatalmortality.csv"), header=TRUE)

under5mort <- read.csv(here("original", "under5mortality.csv"), header=TRUE)

maternalmort <-read.csv(here("original", "maternalmortality.csv"), header=TRUE)

#### FUNCTION TO SUBSET COLUMNS, CONVERT TO LONG FORM AND CHANGE YEAR TO NUMERIC ###

# Load required libraries
library(tidyverse)
library (rlang)

# Define the function to clean the dataset
clean_data <- function(x, mortality_type) {
  subset <-x %>%
    select (Country.Name, X2000:X2019) %>%
    pivot_longer (cols = X2000:X2019, #Select columns
                  names_to = "Year",  #Rename selected columns to 'Year'
                  names_prefix = "X", #Remove the X in the selected columns
                  values_to = "Mortality") %>% #Change the 'count' to 'Mortality'
    mutate(Year = as.numeric(Year)) #store 'Year' as numeric
  colnames(subset)[colnames(subset) == "Mortality"] <- paste0(mortality_type, "_Mortality")
  return(subset)
}


# Clean a dataset and rename 'Mortality' to 'MatMor'
clean_maternalmort <- clean_data(maternalmort, "Maternal")

# Clean another dataset and rename 'Mortality' to 'InfMor'
clean_infantmort <- clean_data(infantmort, "Infant")

# Clean another dataset and rename 'Mortality' to 'NeoMor'
clean_neonatmort <- clean_data(neonatmort, "Neonatal")

# Clean another dataset and rename 'Mortality' to 'Under5Mor'
clean_under5mort <- clean_data(under5mort, "Under5")


### d) Use the reduce() and full_join() functions to merge the four data sets to create one new data set 


# List of datasets to merge
mortality_datasets <- list(clean_maternalmort, clean_infantmort, clean_neonatmort, clean_under5mort)

# Use reduce() and full_join() to merge all datasets
merged_data <- reduce(mortality_datasets, full_join, by = c("Country.Name", "Year"))

# View the final merged dataset
print(merged_data)

## d.) Use the countrycode() function in the countrycode package to add the ISO-3
# country code variable to the new data set created in Step c.

install.packages(countrycode)

library(countrycode)
merged_data$ISO <- countrycode(merged_data$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")


##### Cleaned Disaster Data ###############


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

















