
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















    
  
    
    






