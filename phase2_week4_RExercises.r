install.packages("maps")
install.packages("fueleconomy")
library(maps)
library(tidyverse)
library(nycflights13)
library(fueleconomy)
#13.4.6 #1-4
airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

dest_delays <-
  flights %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c(dest = "faa"))

dest_delays %>%
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point() +
  coord_quickmap()
#2

airport_locations <- airports %>%
  select(faa, lat, lon)

flights %>%
  select(year:day, hour, origin, dest) %>%
  left_join(airport_locations,by = c("origin" = "faa")) %>%
  left_join(airport_locations, by = c("dest" = "faa"))

#3
planeAgeAndDelays <- flights %>%
  mutate(tot_delay = arr_delay + dep_delay) %>%
  group_by(tailnum) %>%
  summarize(avg_delay = mean(tot_delay, na.rm = TRUE)) %>%
  left_join(select(planes, tailnum, year), by = "tailnum") %>%
  mutate(year = 2013 - year) %>% arrange(desc(avg_delay))

planeAgeAndDelays %>% ggplot(aes(year,avg_delay)) + geom_line()
#there doesn't seem to be much of a connection between plane age and delay time

#4
flight_weather <-
  flights %>%
  inner_join(weather, by = c("origin" = "origin",
                             "year" = "year",
                             "month" = "month",
                             "day" = "day",
                             "hour" = "hour"))
weatherAndDelays <- flight_weather %>%
  group_by(precip) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(delay))

weatherAndDelays  %>% ggplot(aes(precip,delay)) + geom_line()

#precipitation correlated with delay

#13.5.1 #1-6

#1
flights %>%
  anti_join(planes, by = "tailnum") %>%
  count(carrier, sort = TRUE)

#two carriers don't have tail nums

#2
flights %>%
  semi_join(count(flights, tailnum) %>% filter(n >= 100))

#3

fueleconomy::vehicles %>% 
  distinct(model, make) %>% 
  group_by(model) %>%
  filter(n() > 10) %>%
  arrange(model) %>%
  head()

fueleconomy::common %>% 
  distinct(model, make) %>% 
  group_by(model) %>%
  filter(n() > 10) %>%
  arrange(model) %>%
  head()

#4
fortyEight <-
  flights %>%
  group_by(month, day) %>%
  summarize(avg_delay = sum(arr_delay + dep_delay, na.rm = TRUE)) %>%
  mutate(twoday_delay = avg_delay + lag(avg_delay)) %>%
  arrange(-twoday_delay)
weather_temp <-
  weather %>%
  group_by(month, day) %>%
  summarize_at(vars(humid, precip, temp), mean, na.rm = TRUE)
fortyEight %>%
  left_join(weather_temp) %>% ggplot(aes(temp,twoday_delay)) + geom_line()
#not much of a patter

#5
#anti_join(flights, airports, by = c("dest" = "faa")) --> flights that go to a non FAA airport 
#anti_join(airports, flights, by = c("faa" = "dest")) --> US airports where there was no flights from 
#New york in 2013

#6
flights %>%
  select(tailnum,carrier) %>%
  distinct(tailnum,carrier) %>%   
  group_by(tailnum) %>% count() %>% filter(n > 1)
#I was surprised to see that there are a lot of tailnums with multiple carriers

#15.3.1 #1-3

#1
gss_cat %>% count(rincome)
ggplot(gss_cat,aes(rincome)) + geom_bar()
#better labeling along the x axis would improve the plot

#2
gss_cat %>%
  count(relig) %>%
  arrange(desc(n)) %>%
  head(1)

gss_cat %>%
  count(partyid) %>%
  arrange(desc(n)) %>%
  head(1)
#3
gss_cat %>%
  filter(!denom %in% c("No answer", "Other", "Don't know", "Not applicable",
                       "No denomination")) %>%
  count(relig)

gss_cat %>% count(relig,denom) %>% ggplot(aes(x=relig,y=denom,size=n)) + geom_point()

#15.4.1 #1-3
#1
summary(gss_cat[["tvhours"]])

gss_cat %>%
  filter(!is.na(tvhours)) %>%
  ggplot(aes(x = tvhours)) + geom_histogram(binwidth = 1)
#mean doesn't look too off here

#2
#For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
head(gss_cat)
#So marital, race, rincome, partyid, relig, and denom are factors
levels(gss_cat$marital) 
#Arbitrary
levels(gss_cat$race) 
#Arbitrary
levels(gss_cat$rincome) 
#Principled
levels(gss_cat$partyid) 
#Principled 
levels(gss_cat$relig) 
#Arbitrary
levels(gss_cat$denom) 
#Principled
#3
#NA gets a value of 1