#4.4 1-3

#1) 
my_variable <- 10
my_varıable

#incorrect spelling is causing the error

#2) fix the following commands
#library(tidyverse)
#ggplot(data = mpg) + 
  #geom_point(mapping = aes(x = displ, y = hwy))
#fliter(mpg, cyl = 8)
#filter(diamond, carat > 3)
install.packages("tidyverse")
library(tidyverse)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl == 8)

filter(diamonds, carat > 3)

#3) alt  + shift + k shows the keyboard shortcuts. Also can go "Tools" --> "Keyboard Shortcuts Help"

#5.2.4 1-4
install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
# Find all flights that
flights <- flights
# Had an arrival delay of two or more hours
filter(flights, arr_delay > 120)
# Flew to Houston (IAH or HOU)
filter(flights, dest == 'IAH' | dest == 'HOU')
# Were operated by United, American, or Delta
filter(flights, carrier == 'UA' | carrier == 'AA' | carrier == 'DL')
# Departed in summer (July, August, and September)
filter(flights, month == 7 | month == 8 | month == 9)
# Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay > 120 & dep_delay <= 0)
# Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay > 60 & dep_delay - arr_delay >= 30)
# Departed between midnight and 6am (inclusive)
filter(flights, dep_time <= 0600)
# Another useful dplyr filtering helper is between(). What does it do? 
#Can you use it to simplify the code needed to answer the previous challenges?
#Returns true if value is between the limits you provide
filter(flights, between(dep_time,0000, 0600))
#   How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
filter(flights, is.na(dep_time))
#8245 rows. Also missing dep_delay, arr_time, arr_delay, etc. Most likely cancelled flights.
#   Why is NA ^ 0 not missing? Why is NA | TRUE not missing? 
#Why is FALSE & NA not missing? Can you figure out the general rule? 
#(NA * 0 is a tricky counterexample!)
#NA is just a placeholder. Since anything to the 0th power is 1, so is NA^0. 
#For NA | TRUE, no matter if NA is true or false the result is true.

#5.3.1 1-4
# How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).
arrange(flights, !is.na(desc(dep_time)))
# Sort flights to find the most delayed flights. Find the flights that left earliest.
arrange(flights, desc(dep_delay))
arrange(flights, dep_delay)
# Sort flights to find the fastest flights.
arrange(flights, air_time)
# Which flights travelled the longest? Which travelled the shortest?
arrange(flights, desc(distance))
arrange(flights, distance)

#5.4.1 2-4
# What happens if you include the name of a variable multiple times in a select() call?
select(flights, dep_delay, dep_delay)
# just shows it once
#   What does the one_of() function do? Why might it be helpful in conjunction with this vector?
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))
#can put in a list of columns to select from
# Does the result of running the following code surprise you? How do the select helpers deal with case by default? How can you change that default?
select(flights, contains("TIME"))
#not case sensitive. Allow you to not know the exact name of every column.
#include the second argument that says ignore.case = FALSE
