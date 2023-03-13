library(tidyverse)
library(janitor)
library(tidyr)
library(readxl)
library(lubridate)

boing_boing_candy_2015 <- read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
boing_boing_candy_2016 <- read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
boing_boing_candy_2017 <- read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))

first_clean_2015 <- clean_names(boing_boing_candy_2015)
first_clean_2016 <- clean_names(boing_boing_candy_2016)
first_clean_2017 <- clean_names(boing_boing_candy_2017)

glimpse(first_clean_2015)
glimpse(first_clean_2016)
glimpse(first_clean_2017)

col_names_2015 <- colnames(first_clean_2015)
col_names_2016 <- colnames(first_clean_2016)
col_names_2017 <- colnames(first_clean_2017)



## deleted irrelevant columns
second_clean_2015 <- first_clean_2015[ -c(16,18,23,26,33,38,41,45,57,69,71,82,85,88,93,94,95,97:114,116:124) ]
second_clean_2015  

second_clean_2016 <- first_clean_2016[ -c(12,21,22,26,27,31,43,46,48,49,71,79,90,94,101,102,104,105,107:122) ]
second_clean_2016

second_clean_2017 <- first_clean_2017[ -c(1,12,21,22,26,27,31,43,46,48,49,68,69,72,81,92,96,104,105,107,108,112:120) ]
second_clean_2017

## col_names_reduced_2015 80 columns remain
## col_names_reduced_2016 90 columns remain
## col_names_reduced_2017  91 columns remain



# extracts date from timestamp and moves to beginning of df

second_clean_2015$timestamp <- as.Date(second_clean_2015$timestamp)
second_clean_2015$year <- as.numeric(format(second_clean_2015$timestamp, "%Y"))
third_clean_2015 %>% select(timestamp, year, everything())



second_clean_2016$timestamp <- as.Date(second_clean_2016$timestamp)
second_clean_2016$year <- as.numeric(format(second_clean_2016$timestamp, "%Y"))
third_clean_2016 %>% select(timestamp, year, everything())


## standardising and simplifying column names

third_clean_2015 <- second_clean_2015 %>% 
  rename(
    age = how_old_are_you,
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
    bonkers_the_candy = bonkers, 
    brach_product = brach_products_not_including_candy_corn,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    hersheys_kisses = hershey_s_kissables,  
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor, 
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    toblerone = tolberone_something_or_other, 
    peanut_m_ms = peanut_m_m_s,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    odd_marshmallow_peanut = those_odd_marshmallow_circus_peanut_things
  )

third_clean_2016 <- second_clean_2016 %>% 
  rename(
    going_out = are_you_going_actually_going_trick_or_treating_yourself,
    gender = your_gender,
    age = how_old_are_you,
    country = which_country_do_you_live_in,
    state_province_county_etc = which_state_province_county_do_you_live_in,
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
    box_o_raisins = boxo_raisins,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor,
    peanut_m_ms = peanut_m_m_s,
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    odd_marshmallow_peanuts = those_odd_marshmallow_circus_peanut_things,
    toblerone = tolberone_something_or_other
  )

third_clean_2017 <- second_clean_2017 %>% 
  rename(
    mary_janes2 = anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes,
    box_o_raisins = boxo_raisins,
    free_restaurant_candy = candy_that_is_clearly_just_the_stuff_given_out_for_free_at_restaurants,
    chick_o_sticks = chick_o_sticks_we_don_t_know_what_that_is,
    hersheys_milk_chocolate = hershey_s_milk_chocolate,
    jolly_ranchers_bad_flavor = jolly_rancher_bad_flavor,
    peanut_m_ms = peanut_m_m_s,
    reeses_peanut_butter_cups = reese_s_peanut_butter_cups,
    boo_berry_crunch = sandwich_sized_bags_filled_with_boo_berry_crunch,
    sourpatch_kids = sourpatch_kids_i_e_abominations_of_nature,
    odd_marshmallow_peanuts = those_odd_marshmallow_circus_peanut_things,
    toblerone = tolberone_something_or_other
  )


  
  
  #convert age to from character to number
 third_clean_2015$age <- as.integer(third_clean_2015$age)
 third_clean_2016$age <- as.integer(third_clean_2016$age)
 third_clean_2017$age <- as.integer(second_clean_2017$age)
 
 third_clean_2016
 
 
 # remove the q1 etc from the 2017 column titles
 colnames(second_clean_2017) <- gsub("^.{3}", "", colnames(second_clean_2017))

 
 # searching for unique entries in country column to clean
 sort(unique(third_clean_2016$country))
 

 
# changed numbers in country column to NA
third_clean_2016$country <- gsub('[0-9.]', '', third_clean_2016$country) 

third_clean_2016 <- third_clean_2016 %>% 
   mutate(country = na_if(country, ""))


third_clean_2017$country <- gsub('[0-9.]', '', third_clean_2017$country) 

third_clean_2017 <- third_clean_2017 %>% 
  mutate(country = na_if(country, ""))


# removing other random answers from country - 2016

na_test <- third_clean_2016 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = case_when(
    country == "A tropical island south of the equator"  | 
      country == "Denial" |
      country == "god's country" |
      country == "Murica" |
      country == "Neverland" |
      country == "Not the USA or Canada" |
      country == "one of the best ones" |
      country == "See above" |
      country == "Somewhere" |
      country == "there isn't one for old men" |
      country == "this one" |
      country == "Trumpistan"  ~ NA_character_, 
    TRUE ~ country
  ))


 third_clean_2016 %>% 
   count(is.na(country))
 
 na_test %>% 
   count(is.na(country))

 

