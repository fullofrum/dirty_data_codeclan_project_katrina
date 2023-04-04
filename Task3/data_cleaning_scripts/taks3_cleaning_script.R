library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(here)
library(stringr)

seabirds <- read_excel(here::here("raw_data/seabirds.xls"))

excel_sheets("raw_data/seabirds.xls")


# importing all excel sheets
ship_data <- read_excel("raw_data/seabirds.xls",
                       sheet = 1)
bird_data <- read_excel("raw_data/seabirds.xls",
                       sheet = 2)
ship_codes <- read_excel("raw_data/seabirds.xls",
                       sheet = 3)
bird_codes <- read_excel("raw_data/seabirds.xls",
                         sheet = 4)


bird_data <- clean_names(bird_data)
ship_data <- clean_names(ship_data)


# keeping relevant column names

colnames(bird_data)

bird_data_remove <- subset(bird_data, select = c(2, 3, 4, 5, 10, 11, 13, 15, 19, 21, 23))  

colnames(ship_data)

ship_data_remove <- subset(ship_data, select = c(2, 5, 6))  


# renaming and removing rows with no birds recorded

bird_data_remove <- bird_data_remove %>% 
  rename(
    species_common_name = species_common_name_taxon_age_sex_plumage_phase,
    species_scientific_name = species_scientific_name_taxon_age_sex_plumage_phase
  ) %>% 
  filter(species_common_name != '[NO BIRDS RECORDED]') 
  

birds_joined <- left_join(bird_data_remove, ship_data_remove, by = "record_id")



write.csv(birds_joined, "clean_data/birds_joined.csv") #press tab to get the file path
 


