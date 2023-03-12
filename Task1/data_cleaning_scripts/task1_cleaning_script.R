library(tidyverse)
decathalon <- read_rds(here::here("raw_data/decathlon.rds"))
library(janitor)
library(tidyr)
library(stringr)



decathalon_first_clean <- clean_names(decathalon) # cleans file names


glimpse(decathalon_first_clean) # summary


d <- decathalon_first_clean
names <- rownames(d)
rownames(d) <- NULL
decathalon_row_remove <- cbind(names,d)  # change row names to columns 




decathalon_name_fixed <- decathalon_row_remove %>%
  select(names) %>%
  mutate(name_fixed = str_to_title(names))  #change uppercase names to title case

decathalon_join <- left_join(decathalon_name_fixed, decathalon_row_remove, by = "names")   #rejoin to table

drops <- c("names")
decathalon_second_clean <- decathalon_join[ , !(names(decathalon_join) %in% drops)] %>%  # removes untidy names column
  rename(name = name_fixed) %>%  #rename column to name
  arrange(names) # arrange alphabetically
  


write_csv(decathalon_second_clean, here::here("clean_data/decathalon_clean.csv"))

  
 
  
  

                  