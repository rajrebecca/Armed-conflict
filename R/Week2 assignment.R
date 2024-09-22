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
usethis::git_sitrep()

#initiate and commit the files
usethis::use_git()

#add personal token
usethis::create_github_token()
gitcreds::gitcreds_set()

usethis::use_github()

usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)

usethis::use_github()

