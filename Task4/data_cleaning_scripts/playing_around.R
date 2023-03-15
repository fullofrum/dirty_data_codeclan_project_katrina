library(tidyverse)
library(dplyr)

# declaring a data frame
test_data = data.frame(Rank = c(5:8) ,
                        Time = c("2021-05-05 01:04:34",
                                 "2021-03-06 03:14:44",
                                 "2021-03-11 07:22:48",
                                 "2021-02-02 11:54:56"))

print ("Original dataframe")
test_data

# data_frame$year <- as.Date(data_frame$Time, "%Y")
# data_frame

test_data$Time <- as.Date(test_data$Time)

print("Extract year")
# extract the year and convert to numeric format
test_data$year <- as.numeric(format(test_data$Time, "%Y"))
data_frame


mtcars

mtcars = data.frame(table(unlist(mtcars)))

mtcars$prop = prop.table(DF$Freq)


mtcars
mtcars %>% select(wt, gear, everything())

sort(unique(mtcars$cyl))

mtcars_test <- na_if(mtcars$mpg, "21")
mtcars %>%  mutate('cyl', na_if, "21")

df %>%
  mutate(across(c(x, y, z), na_if, "6"))
mtcars %>% 
  mutate(across(c(mpg, cyl), na_if, "6"))

glimpse(iris)



df <- structure(list(ColA = c("And", "34", "545"), ColB = c("dff", 
                                                                 "dfa", "dfd")), class = "data.frame", row.names = c(NA, -3L))
 
df$ColA <- gsub('[0-9.]', '', df$ColA)


3/"hello"

iris %>%
  mutate(Species = na_if(Species, "setosa" | Species, "versicolor")) 


iris %>% 
  mutate(Species = as.character(Species)) %>% 
  mutate(Species = case_when(
    Species == "setosa" | Species == "versicolor" ~ NA_character_, 
    TRUE ~ Species
  ))

iris %>% 
  mutate(Species = as.character(Species)) %>% 
  mutate(Species = case_when(
    Species == "setosa" | Species == "versicolor" ~ 'change', 
    TRUE ~ Species
  ))


dummy_iris <- c("virginica", "setosa")


ifelse(dummy_iris %in% c("virginica", "setosa"), "boo", factor(NA))

iris %>% if_else(dummy_iris == "virginica" | dummy_iris == "setosa") {
  print("boo")
  
  
dummy_usa <- c("america",
               "America",
               "EUA",
               "Merica",
               "Murica",
               "Sub-Canadian North America 'Merica",
               "the best one - usa",
               "The Yoo Ess of Aaayyyyyy",
               "Trumpistan",
               "United  States of America")

iris %>% 
table(iris("Species"))

iris %>% 
  group_by(Species == 'virginica')

# Create example data frame
dt <- read.table(text = "student_num    major          dept
123            child          education
                 124            child          education
                 125            special        education
                 126            justice        administration
                 127            justice        administration
                 128            justice        administration
                 129            police         administration
                 130            police         administration",
header = TRUE, stringsAsFactors = FALSE)


starwars



# Select columns
dt <- starwars[, c("skin_color", "sex")]

# Unlist the data frame
dt_vec <- unlist(dt)

# Count the number
table(dt_vec)

dt_vec
administration          child      education        justice         police 
5              2              3              3              2 
special 
1

full_candy %>% 
  str_count("JOY") %>% 
  
  df = ToothGrowth

table(full_candy$mary_janes)

full_candy %>% count("JOY")

full_candy %>% 
  pivot_longer(names_to = "candy", values_to = "feelings")

apply(candies_only, 2, table)


candies_only_long  <- pivot_longer(candies_only,
                                   cols = c(1:97),
                                   names_to = "candy",
                                   values_to = "feelings")

# Select columns
dt <- candies_only_long[, c("feelings", "candy")] %>% 
  filter(feelings == "JOY")

# Unlist the data frame
dt_vec <- unlist(dt)

# Count the number
table(dt_vec)



starwars

library(data.table)
df <- data.table(starwars)
v <- c("male","female")
setnames(df, v, paste0("x",v), skip_absent=TRUE)

g <- melt(df, id.vars = "name",
          measure.vars = patterns(values = "x" ,
                                  sex = "sex",
                                  homeworld = "homeworld"),
          variable.name = "names")

g[names==1, names := "male" ]
g[names==2, names := "female" ]




dat <- tribble(
  ~group,  ~BP,  ~HS,  ~BB, ~lowerBP, ~upperBP, ~lowerHS, ~upperHS, ~lowerBB, ~upperBB,
  "1", 0.51, 0.15, 0.05,     0.16,     0.18,      0.5,     0.52,     0.14,     0.16,
  "2.1", 0.67, 0.09, 0.06,     0.09,     0.11,     0.66,     0.68,     0.08,      0.1,
  "2.2", 0.36, 0.13, 0.07,     0.12,     0.15,     0.34,     0.38,     0.12,     0.14,
  "2.3", 0.09, 0.17, 0.09,     0.13,     0.16,     0.08,     0.11,     0.15,     0.18,
  "2.4", 0.68, 0.12, 0.07,     0.12,     0.14,     0.66,     0.69,     0.11,     0.13,
  "3", 0.53, 0.15, 0.06
group names values lower upper
1:     1    BP   0.51  0.16  0.18
2:   2.1    BP   0.67  0.09  0.11
3:   2.2    BP   0.36  0.12  0.15
4:   2.3    BP   0.09  0.13  0.16
5:   2.4    BP   0.68  0.12  0.14
6:     3    BP   0.53  0.14  0.16
7:     1    HS   0.15  0.50  0.52
8:   2.1    HS   0.09  0.66  0.68
9:   2.2    HS   0.13  0.34  0.38
