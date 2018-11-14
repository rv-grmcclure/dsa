#### WEEK 3 ######
install.packages("tidyverse")
library(tidyverse)

install.packages("nycflights13")
library(nycflights13)
flights <- flights

#5.5.2 1-5

#1)Currently dep_time and sched_dep_time are convenient to look at, 
#but hard to compute with because they’re not really continuous numbers. 
#Convert them to a more convenient representation of number of minutes since midnight.
flights_goodTimes<- mutate(flights,
                        dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100),
                        sched_dep_time_mins = (sched_dep_time %/% 100 * 60 + 
                                                 sched_dep_time %% 100)
)

#2) Compare air_time with arr_time - dep_time. What do you expect to see? 
#What do you see? What do you need to do to fix it?

#would expect air_time = arr_time - dep_time
flights_air_time <- 
  mutate(flights,
         dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100),
         arr_time_min = (arr_time %/% 100 * 60 + arr_time %% 100),
         air_time_diff = air_time - arr_time + dep_time)

filter(flights_air_time, air_time_diff != 0)
#messed up by time zones + flights that go past midnight
  
#3) Compare dep_time, sched_dep_time, and dep_delay. 
#How would you expect those three numbers to be related?

#I would expect dep_time - sched_dep_time = dep_delay
  
#4) Find the 10 most delayed flights using a ranking function. 
#How do you want to handle ties? Carefully read the documentation for min_rank().

delayedFlights <- filter(flights,min_rank(-dep_delay)<=10)
arrange(delayedFlights,dep_delay)

#5) What does 1:3 + 1:10 return? Why?
1:3 + 1:10
#tries to sum each value of the first vector with each value of the second vector.
#if same length, values at same index are added together.
#since the second vector length does not divide the first, we get a warning. tries to cycle through

#5.6.7 2-6

#2) Come up with another approach that will give you the same output 
#as not_cancelled %>% count(dest) 
#and not_cancelled %>% count(tailnum, wt = distance) (without using count()).
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% count(dest) 

not_cancelled %>%
  group_by(dest) %>%
  summarise(n = length(dest))

not_cancelled %>% count(tailnum, wt = distance)

not_cancelled %>%
  group_by(tailnum) %>%
  tally(distance)

#3) Our definition of cancelled flights (is.na(dep_delay) | is.na(arr_delay) ) is slightly suboptimal. 
#Why? Which is the most important column?

#A flight can't arrive if it doesn't depart, and can depart without arriving (crash...) Depart is more important

#4) Look at the number of cancelled flights per day. Is there a pattern? 
#Is the proportion of cancelled flights related to the average delay?

canceled_per_day <-
  flights %>%
  mutate(canceled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(prop_canceled = mean(canceled),
            avg_dep_delay = mean(dep_delay, na.rm = TRUE))

ggplot(canceled_delayed, aes(x = avg_dep_delay, prop_canceled)) +
  geom_point() +
  geom_smooth()

#There's a pattern. The higher the average dep_delay, the higher the proportion of cancelled flights.
  
#5) Which carrier has the worst delays? 
#Challenge: can you disentangle the effects of bad airports vs. bad carriers? 
#Why/why not? (Hint: think about flights %>% group_by(carrier, dest) %>% summarise(n()))
flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))
#To start, we could compare each flight's delay to the destination airport's delay.

#6) What does the sort argument to count() do. When might you use it?
#Sorts the result of the count in the order you decide
#useful if you were going to arrange the result of the count but want to do it in one step.

#5.7.1 1-8

#1) Refer back to the lists of useful mutate and filtering functions. 
#Describe how each operation changes when you combine it with grouping.

#THey perform those operations on the grouping rather than the entire data set,
#similar in sql to counting after having a group by

#2) Which plane (tailnum) has the worst on-time record?

flights %>%
  group_by(tailnum) %>%
  summarise(arr_delay = mean(arr_delay)) %>%
  filter(min_rank(desc(arr_delay)) <= 1)
  
#3) What time of day should you fly if you want to avoid delays as much as possible?
flights %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)

#early in the morning is best
  
#4) For each destination, compute the total minutes of delay. 
#For each flight, compute the proportion of the total delay for its destination.
flights %>%
  filter(!is.na(arr_delay), arr_delay > 0) %>% #get rid of on time flights
  group_by(dest) %>%
  mutate(arr_delay_total = sum(arr_delay),
         arr_delay_prop = arr_delay / arr_delay_total)

#5) Delays are typically temporally correlated: even once the problem that caused 
#the initial delay has been resolved, later flights are delayed to allow earlier flights to leave. 
#Using lag(), explore how the delay of a flight is related to the delay of the 
#immediately preceding flight.

#same airport, departure delay of the previous flight
lag_delay <- 
  flights %>%
  arrange(origin, year, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

lag_delay %>%
  summarise(delay_diff = mean(dep_delay - dep_delay_lag), na.rm = TRUE)

#6) Look at each destination. Can you find flights that are suspiciously fast? 
#(i.e. flights that represent a potential data entry error). 
#Compute the air time a flight relative to the shortest flight to that destination. 
#Which flights were most delayed in the air?

#suspiciously fast
flights %>%
  group_by(dest) %>%
  arrange(air_time) %>%
  slice(1:5) %>%
  select(tailnum, sched_dep_time, sched_arr_time, air_time) %>%
  arrange(air_time)

#most delayed in the air
flights %>%
  group_by(dest) %>%
  mutate(shortest = air_time - min(air_time, na.rm = TRUE)) %>%
  top_n(1, air_time) %>%
  arrange(-air_time) %>%
  select(tailnum, sched_dep_time, sched_arr_time, shortest)
  
#7) Find all destinations that are flown by at least two carriers. 
#Use that information to rank the carriers.

flights %>%
  group_by(dest) %>%
  filter(n_distinct(carrier) > 2) %>%
  group_by(carrier) %>%
  summarise(n = n_distinct(dest)) %>%
  arrange(-n)

#8) For each plane, count the number of flights before the first delay of greater than 1 hour.
flights %>%
  arrange(tailnum, year, month, day) %>%
  group_by(tailnum) %>%
  mutate(delay_oneHr = dep_delay > 60) %>%
  mutate(before_delay = cumsum(delay_oneHr)) %>%
  filter(before_delay < 1) %>%
  count(sort = TRUE)

#19.3.1 1-4

#1) Read the source code for each of the following three functions, puzzle out what they do, 
#and then brainstorm better names.

#f1 checks whether each element of the vector starts with the provided prefix
#better name --> beginsWith

#f2 eliminates the last element of the vector x
#better name --> dropItLikeItsHot

#f3 repeats y once for each element of the vector x
#better name --> addToEnd

#2) Take a function that you’ve written recently and spend 5 minutes brainstorming a better name for it and its arguments.
#getContent(x) --> getTag(articleName)

#3) Compare and contrast rnorm() and MASS::mvrnorm(). How could you make them more consistent?
#univariate vs multivariate normal distribution. Make them more consistent by having the arguments be the same  

#4) Make a case for why norm_r(), norm_d() etc would be better than rnorm(), dnorm(). Make a case for the opposite.
#norm_r() and norm_d() makes more sense because the distribution is consistent between the 2
#rnorm() and dnorm() stays conistent with other r functions like runif() and dunif()

#19.4.4 #1-6

#1) What’s the difference between if and ifelse()? 
#Carefully read the help and construct three examples that illustrate the key differences.

#if takes one argument while ifelse tests every element
#example: x <- c(6:-4)
#sqrt(x)  #- gives warning
#sqrt(ifelse(x >= 0, x, NA)) returns vector of same length as x

#2) Write a greeting function that says “good morning”, “good afternoon”, or “good evening”, 
#depending on the time of day. (Hint: use a time argument that defaults to lubridate::now(). 
                               #That will make it easier to test your function.)


greet <- function(time = lubridate::now()) {
  hr <- lubridate::hour(time)
  if (4 < hr && hr < 12) {
    print("good morning")
  } 
  else if (12 < hr && hr < 17) {
    print("good afternoon")
  } 
  else {
    print("good evening")
  }
}

greet()

#3) Implement a fizzbuzz function. It takes a single number as input. 
#If the number is divisible by three, it returns “fizz”. If it’s divisible by five it returns “buzz”. 
#If it’s divisible by three and five, it returns “fizzbuzz”. Otherwise, it returns the number. 
#Make sure you first write working code before you create the function.

fizzbuzz <- function(x) {
  if (x%%3 == 0) {
    if(x%%5 == 0) {
      "fizzbuzz"
    }
    else {
      "fizz"
    }
  }
  else if (x%%5 == 0) {
    "buzz"
  }
  else {
    x
  }
}
fizzbuzz(155)

#4) How could you use cut() to simplify this set of nested if-else statements?
temp <- seq(-10, 50, by = 5)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), right = TRUE,
    labels = c("freezing", "cold", "cool", "warm", "hot"))
#right = FALSE to be inclusive

#5) What happens if you use switch() with numeric values?
#siwtch(n,...)will take the nth argument of ...

#6) What does this switch() call do? What happens if x is “e”?
#switch(x, 
 #      a = ,
  #     b = "ab",
   #    c = ,
    #   d = "cd"
#)
#switch() returns the first non-missing argument value for the first name it matches. 
#Since e isn't in th list of arguments, it would return null.