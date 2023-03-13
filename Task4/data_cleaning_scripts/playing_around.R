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

