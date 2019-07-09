library(dplyr)
library(tidyverse)
library(nycflights13)


# The filter() function 
# 
# #Question 1) Find all flights that:
# # a) Had an arrival delay of 2+ hours
#   b) Flew to Houston
#   c) Were operated by United, American, or Delta
#   d) Departed in summer(July, August, and September)
#   e) Arrived 2+ hours late, but didn't leave late
#   f) Were delayed by at least an hour, but made up over 30 minutes in flight
#   g) Departed between midnight and 6 a.m. (inclusive)

# 1a)

delayed_flight <- filter(flights, arr_delay >= 120)

print(delayed_flight)

# 1b)

hou_dest <- filter(flights, dest == 'IAH' | dest == 'HOU')

print(hou_dest)

# 1c)

carriers <- filter(flights, carrier == 'UA' | carrier == 'AA' | carrier == 'DL')

print(carriers)

# 1d)

dep_month <- filter(flights, month == 9 | month == 8 | month == 7)

print(dep_month)

# 1e)

arr_late <- filter(flights, arr_delay > 120 & dep_delay <= 0)

print(arr_late)

# 1f)

del_madeUp <- filter(flights, dep_delay >= 60 & arr_delay <= dep_delay - 30)

print(del_madeUp)

# 1g)

dep_night <- filter(flights, dep_time > 0 & dep_time < 600)

print(dep_night)

# Question 2) Another useful dplyr filtering helper is between(), What does it do? Can you use it to simplify the code needed to answer the previous challenges?

dep_night <- filter(flights, between(dep_time, 0, 600))

print(dep_night)

# Answer 2) between() takes 3 parameters: the first one is the value being checked, the second is the left bound, and the third is the right bound.
# between() checks if the first parameter is between the second and third parameters. It can be useful as shown above in the code.

# Question 3) How many flights have a missing dep_time? What other variable are missing? What might these rows represent?

no_dep <- filter(flights, is.na(dep_time))

print(no_dep)

# Answer 3) The departure delay, arrival time, and arrival delay are also missing from these rows. These rows probably represent cancelled or stopped flights.

# Question 4) Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing?

# Answer 4) Since any number to the power of 0 = 1, NA ^ 0 is not missing. Anything or true will always be true and anything and false will always be false.



# Arrange Rows with arrange()

# Question 1) How could you use arrange() to sort all missing values to the start?

# Answer 1)

arrange(flights, desc(is.na(dep_time)), dep_time)
arrange(flights, desc(is.na(dep_delay)), dep_delay)
arrange(flights, desc(is.na(arr_time)), arr_time)
arrange(flights, desc(is.na(arr_delay)), dep_time)

# Question 2) Sort flights to find the most delayed flights. Find the flights that left earliest.

# Answer 2)

arrange(flights, desc(dep_delay)) # this shows the most delayed flights

arrange(flights, dep_delay) # this shows the most early flights compared to the scheduled depart time

# Question 3) Sort flights to find the fastest flights.

arrange(flights, distance * 60/air_time)# this shows the fastest flights by speed

# Question 4) Which flights traveled the longest? Which traveled the shortest?

arrange(flights, desc(distance)) #longest
arrange(flights, distance) #shortest


# Select() 

# Question 1) Brainstorm as many ways as possible to select dep_time, dep_delay, arr_time, and arr_delay fro flights.

# Answer 1)

select(flights, dep_time, dep_delay, arr_time, arr_delay) # Column Names

select(flights, 4, 6, 7, 9) # Column numbers

# Question 2) What happens if you include the name of a variable multiple times in a select() call?

# Answer 2) It will only include the variable once.

# Question 3) What does the one_of() function do? Why might it be helpful in conjunction with this vector?

# Answer 3) The one_of() function selects variables with a c() vector rather than variable names. 
# This function is useful because it is easier to use more than once. We can use a vector multiple times rather than typing the variable names each time.
# You can use the one_of() function here since vars may also be a variable name in the data frame

# Question 4) Does the result of running the following code surprise you? 
# How do the select helpers deal with case by default? How can you change that default?
# select(flights, contains("TIME"))

select(flights, contains("TIME"))

# Answer 4) It doesn't involve uppercase letters. It treats them all the same. 
# We can use "ignore.case = FALSE" as a second parameter in the contains() function to change this default.


# mutate() Function

# Question 1) 

# Answer 1)

flights_times <- mutate(flights, dep_time_new = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440, sched_dep_time_new= (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440)


#Question 2) 



flights_airtime <- mutate(flights, dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440, arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440, air_time_diff = air_time - arr_time + dep_time)

nrow(filter(flights_airtime, air_time_diff != 0)) # Many rows where air_time is not equal to arr_time - dep_time


# Sometimes the arrival time is after midnight which resets the clock. Also, the time zone issue comes into play
# because the flight travels through different time zones. Setting all the time to a standard time (for instance EST) could fix
# this problem. Also, for the midnight problems, we can add 24 hours to the arrival time.


# Question 3)

# dep_time - sched_dep_time = dep_delay

# Question 4)

# Answer 4) 

flights_delay <- mutate(flights, dep_delay_min_rank = min_rank(desc(dep_delay)), dep_delay_row_number = row_number(desc(dep_delay)), dep_delay_dense_rank = dense_rank(desc(dep_delay)))
flights_delay <- filter(flights_delay, !(dep_delay_min_rank > 10 | dep_delay_row_number > 10 | dep_delay_dense_rank > 10))
flights_delay <- arrange(flights_delay, dep_delay_min_rank)


# Question 5)

# Answer 5) It will return 2  4  6  5  7  9  8 10 12 11, because it goes like this: 1+1, 2+2, 3+3, 1+4, 2+5, 3+6, ...


# Question 6)

# Answer 6) cos(x), sin(x), tan(x), acos(x), asin(x), atan(x),...


# Grouped summaries with summarize()

# Question 1)

# Arrival delay is more important in most cases because passengers may have connecting flights or plans when they are scheduled to arrive.
# A flight being 10 minutes late is better than having a 50% chance of being 30 min early and a 50% chance of being 30 min late, because people can plan for being late by 10 min


# Question 2) 

not_cancelled %>% group_by(dest) %>% summarize(n = n())

# Question 3)

# The most important column is arr_delay or arr_time, because it indicates the amount of delay in arrival and if the flight arrived. 
#Some flights may have departed but crashed, which would only show up arr_delay or arr_time.

# Question 4)

day_cancellation <- mutate(flights, cancelled = (is.na(dep_delay) | is.na(arr_delay))) %>% group_by(year, month, day) %>% 
  summarize(cancelled_flights = sum(cancelled), flights_num = n())

ggplot(day_cancellation) +
  geom_point(aes(x = flights_num, y = cancelled_flights))

candd <-
 flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
  summarise(
    cancelled_prop = mean(cancelled),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE),
    avg_arr_delay = mean(arr_delay, na.rm = TRUE)
  )
  
ggplot(candd) +
  geom_point(aes(x = avg_dep_delay, y = cancelled_prop))

# There is a relationship between cancelled flights and average delay

# Question 5)

flights %>% group_by(carrier) %>% summarize(arr_delay = mean(arr_delay, na.rm = TRUE)) %>% arrange(desc(arr_delay))

# Question 6)

# The sort argument to count() sorts the results in order of n. 


# Grouping with filter() and mutate()

# Question 1)

# Summary functions (mean()), offset functions (lead(), lag()), ranking functions (min_rank(), row_number()), operate within each group when used with group_by() in mutate() or filter(). Arithmetic operators (+, -), logical operators (<, ==), modular arithmetic operators (%%, %/%), logarithmic functions (log) are not affected by group_by.

# Question 2)

flights %>%
  group_by(tailnum) %>%
  summarize(arr_delay = mean(arr_delay), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(desc(arr_delay)) == 1)

# Question 3)

flights %>%
  group_by(hour) %>%
  summarize(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

# Question 4) 

flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(
    dest, month, day, dep_time, carrier, flight,
    arr_delay, arr_delay_prop
  ) %>%
  arrange(dest, desc(arr_delay_prop))



















