library(tidyverse)
library(nycflights13)

#####nycflights13######
#1. You would need to map the origin and destination airpots to their latitude and longitude
# variables: origin, dest, lat, lon (flights and airports)

#2.airports connects to weather since the weather is marked by EWR (faa code)

#3. If it contained all airports, it would connect to dest

#4.data frame with vars: Holiday (char), month, day
#this would than connected to all tables with month and day vars (weather and flights)

#####Keys#####
#1. 
flights <- flights %>%
  mutate(row_num = row_number())

#2.
install.packages("Lahman")
library("Lahman")
?Lahman::Batting
(Batting %>% count(playerID, yearID, stint) %>% filter(n>1))
#key: (playerID, yearID, stint)

install.packages("babynames")
library("babynames")
babynames::babynames
(babynames %>% count(year, sex, name) %>% filter(nn >1)) #idk why count is creating an nn col and removing the n col, but it works for my purposes
#key: (year, sex, name)

install.packages("nasaweather")
library("nasaweather")
nasaweather::atmos
(atmos %>% count(lat, long, year, month) %>% filter(n>1))
#key: (lat, long, year, month)

install.packages("fueleconomy")
library("fueleconomy")
fueleconomy::vehicles
(vehicles %>% count(id) %>% filter(n>1))
#key: id

ggplot2::diamonds
(diamonds %>% count(price, carat, cut, color, clarity, x, y, z, depth, table) %>% filter(n>1))
#key: none, there are duplicate rows

#3. Diagram drawn in notebook. Please let me know how I should get it to you (picture over email, show in person, etc)

#####Mutating Joins#####
#1.
flights2 <- flights %>% group_by(dest) %>% mutate(av_delay = mean(arr_delay, na.rm = TRUE)) %>% inner_join(airports, by = c(dest = "faa"))

install.packages("maps")
flights2 %>%
  ggplot(aes(lon, lat, color = av_delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()

#2. 
airports <- airports %>% select("lat", "lon", "faa")
flights %>% select("origin", "dest", "year", "month", "day") %>% left_join(airports, by = c("origin" = "faa")) %>% left_join(airports, by = c("dest" = "faa"))


#3.
flights2 <- flights %>% group_by(tailnum) %>% summarise(ave_delay = mean(arr_delay, na.rm = TRUE))

planes2 <- planes %>% select("tailnum", "year")

flights2 <- flights2 %>% left_join(planes2) %>% group_by(year) %>% mutate(n = n())

ggplot(flights2, aes(x = year, y = ave_delay, size = n)) +
  geom_point(alpha = .5)

#there doesn't appear to be a relationship since very old planes are probably taken out of service so there are fewer of them

#4. 
flights2 <- flights %>% 
  filter(!is.na(arr_delay)) %>% #takes out points that didn't ever arrive since they aren't important to my analysis
  inner_join(weather, by = c("year", "month", "day", "hour")) %>%
  group_by(precip) %>%
  mutate(ave_delay = mean(arr_delay, na.rm = TRUE))

ggplot(flights2, aes(x = precip, y = ave_delay)) +
  geom_line()
#rough correlation between precip and delay, but its the strongest (sharp increase from 0 to non-zero)

#5.
flights %>% filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  mutate(delay = mean(arr_delay, na.rm = TRUE)) %>%
  ggplot(aes(x=lon, y=lat, size = delay, color = delay)) +
  borders("state") +
  geom_point(alpha = .5) +
  coord_quickmap()

#looks like the southeast had a lot of delays
#after a quick google search, yes! Two derechos (series of thunder storms) occured on the East coast.

#####Filtering Joins#####

#1. 
no_tailnum = flights %>% anti_join(planes, by = "tailnum") %>%
  arrange(carrier)

#certain airlines just don't report tailnumbers

#2.
over100 <- flights %>%
  group_by(tailnum) %>%
  count() %>%
  filter(n > 100)

flights <- flights %>%
  semi_join(over100, by = "tailnum")

#3.
common_vehicles <- vehicles %>% 
  semi_join(common, by = "model")

#4.
worst_48_hours <- flights %>%
  group_by(year, month, day, hour) %>%
  mutate(ave_delay = mean(dep_delay), row = row_number()) %>%
  arrange(desc(ave_delay)) %>%
  filter(row >= 48)

worst_hour_weather <- weather %>%
  semi_join(worst_48_hours, by = c("year", "month", "day", "hour"))

ggplot(worst_hour_weather, aes(precip)) +
  geom_bar()


#5.
#flights that aren't in the airports data
#airports that didn't ever have any incoming flights

#6.
plane_carrier_combinations <-
  flights %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum, carrier) %>%
  count(tailnum) %>%
  filter(n>1)

#confirmed, plane_carrier_combinations ends with 0 obs


