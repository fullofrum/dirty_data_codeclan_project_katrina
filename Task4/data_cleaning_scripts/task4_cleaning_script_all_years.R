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



## 2015 Cleaning

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



## 2016 Cleaning

## deleted irrelevant columns

second_clean_2016 <- first_clean_2016[ -c(12,21,22,26,27,31,43,46,48,49,71,79,90,94,101,102,104,105,107:122) ]
second_clean_2016


# extracts date from timestamp and moves to beginning of df

second_clean_2016$timestamp <- as.Date(second_clean_2016$timestamp)
second_clean_2016$year <- as.numeric(format(second_clean_2016$timestamp, "%Y"))
second_clean_2016 <- second_clean_2016 %>% select(timestamp, year, everything())


## standardising and simplifying column names

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




#convert age to from character to number

third_clean_2016$age <- as.integer(third_clean_2016$age)



# searching for unique entries in 2016 country column to clean

sort(unique(third_clean_2016$country))



# changed numbers in country column to NA

third_clean_2016$country <- gsub('[0-9.]', '', third_clean_2016$country) 

third_clean_2016 <- third_clean_2016 %>% 
  mutate(country = na_if(country, ""))



# searching for unique entries in 2016 country column to clean

sort(unique(third_clean_2016$country))



# cleaning country column

third_clean_2016 <- third_clean_2016 %>% 
  mutate ( country = str_to_lower(third_clean_2016$country))

unique_names_country <- sort(unique(third_clean_2016$country))
unique_names_country

fourth_clean_2016 <- third_clean_2016 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = case_when(
    country == "a tropical island south of the equator"  |
      country == "cascadia" |
      country == "denial" |
      country == "god's country" |
      country == "there isn't one for old men" |
      country == "neverland" |
      country == "not the usa or canada" |
      country == "one of the best ones" |
      country == "see above" |
      country == "somewhere" |
      country == "the republic of cascadia" |
      country == "this one"
    ~ NA_character_, 
    TRUE ~ country
  ))


Belgium_names <- c("belgium")

Canada_names <- c("canada")

Croatia_names <- c("croatia")

UK_names <- c("england", "uk", "united kindom", "united kingdom")

Spain_names <- c("españa")

France_names <- c("france")

Germany_names <- c("germany")

Hungary_names <- c("hungary")

Kenya_names <- c("kenya")

Netherlands_names <- c("the netherlands","netherlands")

Sweden_names <- c("sweden")

Australia_names <- c("australia")

Austria_names <- c("austria")

Brasil_names <- c("brasil")

China_names <- c("china")

Finland_names <- c("finland")

Japan_names <- c("japan")

Korea_names <- c("korea")

Mexico_names <- c("mexico")

New_Zealand_names <- c("new zealand")

Panama_names <- c("panama")

Philippines_names <- c("philippines")

Portugal_names <- c("portugal")

South_Korea_names <- c("south korea")

Switzerland_names <- c("switzerland")



USA_names <-  c("the yoo ess of aaayyyyyy", "eua", "sub-canadian north america 'merica",                                                                        
                "trumpistan" ,                                                   
                "u.s.",  "merica",  "murica",
                "Sub-Canadian North America 'Merica",
                "united  states of america",                                   
                "america",  "the best one - usa",                                           
                "united sates",                                                                                                     
                "united states" ,                                                                                      
                "united stetes",                                                                                                  
                "us", "usa usa usa",                                                                                                              
                "usa usa usa usa",                                                                                                          
                "usa! usa!",                                                                                                     
                "usa!!!!!!",                                                                                                              
                "u.s.a.", "usa! usa! usa!", 
                "united state",
                "united states of america", 
                "units states",
                "usa", "usa!",
                "ussa","usa (i think but it's an election year so who can really tell)") 



fifth_clean_2016 <- fourth_clean_2016 %>% mutate(country = case_when(country %in% Belgium_names ~ "Belgium", 
                                                                     country %in% Canada_names ~ "Canada", 
                                                                     country %in% Croatia_names ~ "Croatia",
                                                                     country %in% UK_names ~ "UK",
                                                                     country %in% Spain_names ~ "Spain",
                                                                     country %in% France_names ~ "France", 
                                                                     country %in% Germany_names ~ "Germany", 
                                                                     country %in% Hungary_names ~ "Hungary",
                                                                     country %in% Kenya_names ~ "Kenya",
                                                                     country %in% Netherlands_names ~ "Netherlands", 
                                                                     country %in% Sweden_names ~ "Sweden", 
                                                                     country %in% Australia_names ~ "Australia",
                                                                     country %in% Austria_names ~ "Austria",
                                                                     country %in% Brasil_names ~ "Brasil",
                                                                     country %in% China_names ~ "China",
                                                                     country %in% Finland_names ~ "Finland",
                                                                     country %in% Japan_names ~ "Japan",
                                                                     country %in% Korea_names ~ "Korea",
                                                                     country %in% Mexico_names ~ "Mexico",
                                                                     country %in% New_Zealand_names ~ "New Zealand",
                                                                     country %in% Panama_names  ~ "Panama",
                                                                     country %in% Philippines_names ~ "Philippines",
                                                                     country %in% Portugal_names ~ "Portugal",
                                                                     country %in% South_Korea_names ~ "South Korea",
                                                                     country %in% Switzerland_names ~ "Switzerland",
                                                                     country %in% USA_names ~ "USA",
                                                                     TRUE  ~ as.character(country)))




third_clean_2016 %>% 
  count(is.na(age))





# removing columns we don't need for analysis

sixth_clean_2016 <-  select (fifth_clean_2016, -timestamp, -state_province_county_etc, -york_peppermint_patties_ignore)




## 2017 Cleaning

## deleted irrelevant columns

second_clean_2017 <- first_clean_2017[ -c(1,12,21,22,26,27,31,43,46,48,49,68,69,72,81,92,96,104,105,107,108,112:120) ]
second_clean_2017



# remove the q1 etc from the 2017 column titles

colnames(second_clean_2017) <- gsub("^.{3}", "", colnames(second_clean_2017))



## standardising and simplifying column names

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



glimpse(third_clean_2017)


#convert age to from character to number

third_clean_2017$age <- as.integer(third_clean_2017$age)



# changed numbers in country column to NA

third_clean_2017$country <- gsub('[0-9.]', '', third_clean_2017$country) 

third_clean_2017 <- third_clean_2017 %>% 
  mutate(country = na_if(country, ""))



# searching for unique entries in 2017 country column to clean

sort(unique(third_clean_2017$country))



## cleaning country column

third_clean_2017 <- third_clean_2017 %>% 
  mutate ( country = str_to_lower(third_clean_2017$country))

unique_names_country <- sort(unique(third_clean_2017$country))
unique_names_country


#changing random data into NA

fourth_clean_2017 <- third_clean_2017 %>% 
  mutate(country = as.character(country)) %>% 
  mutate(country = case_when(
    country == "a"  | 
      country == "atlantis" |
      country == "canae" |
      country == "cascadia" |
      country == "earth"|
      country == "europe"|
      country ==  "fear and loathing"|
      country ==  "i don't know anymore"|
      country ==  "insanity lately"|
      country ==  "narnia"|
      country ==  "subscribe to dmuz on youtube"|
      country ==  "ud"|
      country ==  "unhinged states" ~ NA_character_, 
    TRUE ~ country
  ))


sort(unique(fourth_clean_2017$country))


# Standardising countries so the case and spelling are uniform

Australia_names <- c("australia")

Canada_names <- c("canada","canada`", "soviet canuckistan", "can")

Finland_names <- c("finland")

France_names <- c("france")

Germany_names <- c("germany")

Hong_Kong_names <- c("hong kong")

Netherlands_names <- c("the netherlands","netherlands")

South_Africa_names <- c("south africa")

Spain_names <- c("spain")

UK_names <- c("uk","united kingdom","scotland","endland","england")

USA_names <-  c("alaska","'merica","ahemamerca","america","california",
                "i pretend to be from canada, but i am really from the united states",
                "murica","murrika","n america","new jersey","new york","north carolina",
                "pittsburgh","the united states","the united states of america","trumpistan",
                "u s","u s a","unied states","unite states","united sates","united staes",
                "united state","united statea","united stated","united states","united states of america",
                "united statss","united ststes","unites states","us","us of a",
                "usa","usa usa usa!!!!","usa! usa! usa!","usa? hard to tell anymore",
                "usaa","usas","usausausa","ussa","u.s.a.")

# Capitalising country names which were affected by str_to_lower command

China_names <- c("china")
Costa_Rica_names <- c("costa rica")
Denmark_names <- c("denmark")
Greece_names <- c("greece")
Iceland_names <- c("iceland")
Indonesia_names <- c("indonesia")
Ireland_names <- c("ireland")
Japan_names <- c("japan")
Mexico_names <- c("mexico")
Singapore_names <- c("singapore")
South_Korea_names <- c("korea","south korea")
Sweden_names <- c("sweden")
Switzerland_names <- c("switzerland")
Taiwan_names <- c("taiwan")
UAE_names <- c("uae")

fifth_clean_2017 <- fourth_clean_2017 %>% 
  mutate(country = case_when(country %in% Australia_names ~ "Australia",
                             country %in% Canada_names ~ "Canada", 
                             country %in% Finland_names ~ "Finland",
                             country %in% France_names ~ "France", 
                             country %in% Germany_names ~ "Germany", 
                             country %in% Hong_Kong_names ~ "Hong Kong",
                             country %in% Netherlands_names ~ "Netherlands", 
                             country %in% South_Africa_names ~ "South Africa", 
                             country %in% Spain_names ~ "Spain",
                             country %in% UK_names ~ "UK", 
                             country %in% USA_names ~ "USA",
                             country %in% China_names ~ "China",
                             country %in% Costa_Rica_names ~ "Costa Rica",
                             country %in% Denmark_names ~ "Denmark",
                             country %in% Greece_names ~ "Greece",
                             country %in% Iceland_names ~ "Iceland",
                             country %in% Indonesia_names ~ "Indonesia",
                             country %in% Ireland_names ~ "Ireland", 
                             country %in% Japan_names ~ "Japan", 
                             country %in% Mexico_names ~ "Mexico", 
                             country %in% Singapore_names ~ "Singapore", 
                             country %in% South_Korea_names ~ "South Korea", 
                             country %in% Sweden_names ~ "Sweden", 
                             country %in% Switzerland_names ~ "Switzerland", 
                             country %in% Taiwan_names ~ "Taiwan", 
                             country %in% UAE_names ~ "UAE", 
                             TRUE  ~ as.character(country)))


sort(unique(fifth_clean_2017$country))


fifth_clean_2017 %>% 
  count(is.na(age))

second_clean_2017 %>% 
  count(is.na(age))

# removes columns not relevant to analysis

sixth_clean_2017 <-  select (fifth_clean_2017, -state_province_county_etc, -joy_other, - despair_other)



write.csv(third_clean_2015, "clean_data/candy_2015.csv", row.names = FALSE)
write.csv(third_clean_2015, "clean_data/candy_2015.csv", row.names = FALSE)
write.csv(third_clean_2015, "clean_data/candy_2015.csv", row.names = FALSE)







