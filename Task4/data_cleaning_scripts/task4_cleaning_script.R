library(tidyverse)
decathalon <- read_rds(here::here("raw_data/decathlon.rds"))
library(janitor)
library(tidyr)



decathalon_first_clean <- clean_names(decathalon) # cleans file names


glimpse(decathalon_first_clean) # summary


d <- decathalon_first_clean
names <- rownames(d)
rownames(d) <- NULL
decathalon_row_remove <- cbind(names,d)  # change row names to columns 


decathalon_name_fixed <- decathalon_row_remove %>%
  select(names) %>%
  mutate(name_fixed = str_to_title(names)) %>% 
  print("names", "long_jump", "pole_vault")
 

                  