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


sort(unique(fifth_clean_2016_test$country))



# removing columns we don't need for analysis

sixth_clean_2016 <-  select (fifth_clean_2016, -timestamp, -state_province_county_etc, -york_peppermint_patties_ignore)







fifth_clean_2016_test %>% 
  count(is.na(age))





#######  changing code to make it cleaner???

fifth_clean_2016 <- fifth_clean_2016 %>% 
  mutate (country =  str_to_title(country)) # capitalise
10:59
fifth_clean_2016 <- fifth_clean_2016 %>% 
  mutate(country = case_when(country == "Usa" ~ "USA",
                             country == "Uk" ~ "UK",
                             TRUE ~ country)) # to capitalise USA










