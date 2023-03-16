library(tidyverse)
library(janitor)
library(tidyr)
library(readxl)
library(lubridate)
library(here)

boing_boing_candy_2015 <- read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))


first_clean_2015 <- clean_names(boing_boing_candy_2015)


glimpse(first_clean_2015)


col_names_2015 <- colnames(first_clean_2015)




## deleted irrelevant columns
second_clean_2015 <- first_clean_2015[ -c(16,18,23,26,33,38,41,45,57,69,71,82,85,88,93,94,95,97:114,116:124) ]
second_clean_2015 


# extracts date from timestamp and moves to beginning of df

second_clean_2015$timestamp <- as.Date(second_clean_2015$timestamp)
second_clean_2015$year <- as.numeric(format(second_clean_2015$timestamp, "%Y"))
second_clean_2015 <- second_clean_2015 %>% select(timestamp, year, everything())




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


#convert age to from character to number

third_clean_2015$age <- as.integer(third_clean_2015$age)



 # removing timestamp column

third_clean_2015 <- third_clean_2015[ -c(1)]


write.csv(third_clean_2015, "clean_data/candy_2015.csv", row.names = FALSE)


