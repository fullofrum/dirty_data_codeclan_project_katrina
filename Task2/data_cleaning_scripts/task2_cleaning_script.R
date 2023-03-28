library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(here)
library(stringr)


cakes <- read.csv(here::here("raw_data/cake-ingredients-1961.csv"))
cake_codes <- read_csv(here::here("raw_data/cake_ingredient_code.csv"))

cakes_pivot <- cakes %>% 
pivot_longer(cols = AE:ZH,
             names_to = "code",
             values_to = "measurement", as.numeric("measurement"))

cakes_clean <- cakes_pivot %>% 
  left_join(cake_codes, by = "code") %>%    # joins the two datasets
  na.omit() %>%                             # removes rows which contian NA
  mutate(cake = str_trim(Cake), .before = code) %>%   # removes whitespace in Cake column
  subset(select = - Cake )                  #removes old cake column
  


write.csv(cakes_clean, "clean_data/cakes.csv")
  

  


 