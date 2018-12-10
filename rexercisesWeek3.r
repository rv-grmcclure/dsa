#12.2.1 #2-3
library(tidyverse)
#2
#Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
  
  #Extract the number of TB cases per country per year.
#Extract the matching population per country per year.
#Divide cases by population, and multiply by 10000.
#Store back in the appropriate place.
#Which representation is easiest to work with? Which is hardest? Why?

t2_cases <- filter(table2, type == "cases") %>%
  rename(cases = count) %>%
  arrange(country, year)

t2_population <- filter(table2, type == "population") %>%
  rename(population = count) %>%
  arrange(country, year)

t2_total <- t2_cases %>% mutate(pop = t2_population$population)

t2_total <- t2_total %>% mutate(rate = cases/pop *10000)

table4c <-
  tibble(country = table4a$country,
         `1999` = table4a[["1999"]] / table4b[["1999"]] * 10000,
         `2000` = table4a[["2000"]] / table4b[["2000"]] * 10000)
table4c

#i think the second way was a lot harder since they live in different tables
#3
library(ggplot2)
ggplot(t2_total, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#12.3.3 #1-4
#1
#Why are gather() and spread() not perfectly symmetrical?
#after one of them has been used, the data types are thrown away per se. So even though they started as a numeric,
#they were strings after running spread and gather

#2 Why does this code fail?
table4a %>% 
  gather(1999, 2000, key = "year", value = "cases")
#the columns of table4a are strings, not integers

#3 Why does spreading this tibble fail? How could you add a new column to fix the problem?
people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
people
#because there are two rows for phillip woods and age, it doesn't know which one to choose
#add a different column to say the first one or something, and use that value

#4 Tidy the simple tibble below. Do you need to spread or gather it? What are the variables?
preg <- tribble(
~pregnant, ~male, ~female,
"yes",     NA,    10,
"no",      20,    12
)

preg_tidy <- preg %>%
  gather(male, female, key = "sex", value = "count")
preg_tidy

#12.5.1 1-2
#1 Compare and contrast the fill arguments to spread() and complete().
#spread - the NA's are replaced by the fill arguments
#complete - same thing, but uses a list to allow to specificy values for differnet variables

#2
#What does the direction argument to fill() do?
?fill
#Direction in which to fill missing values, either down or up