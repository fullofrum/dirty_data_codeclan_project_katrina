library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(here)


cakes <- read.csv(here::here("raw_data/cake-ingredients-1961.csv"))
cake_codes <- read_csv(here::here("raw_data/cake_ingredient_code.csv"))
