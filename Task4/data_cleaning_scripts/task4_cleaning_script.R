library(tidyverse)
library(janitor)
library(tidyr)
library(readxl)

boing_boing_candy_2015 <- read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
boing_boing_candy_2016 <- read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
boing_boing_candy_2017 <- read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))

first_clean_2015 <- clean_names(boing_boing_candy_2015)
first_clean_2016 <- clean_names(boing_boing_candy_2016)
first_clean_2017 <- clean_names(boing_boing_candy_2017)
