require("tidyverse")
require("nycflights13")

#####filter#####
(delayed <- filter(flights, arr_delay >=120))

(delayed) <- filter(flights, arr_delay >= 120)

(Houston <- filter(flights, dest %in% c("IAH","HOU")))

(Airlines <- filter(flights, carrier %in% c("DL", "AA", "UA")))

(summer <- filter(flights, month %in% c(7,8,9)))

(weird <- filter(flights, arr_delay>=120 & dep_delay<1))

(fast_pilot <- filter(flights, dep_delay>60 & arr_delay < dep_delay-30))

(red_eye <- filter(flights, dep_time >= 0 & dep_time <= 600))

?between #essentially number <= x <= number

(red_eye <- filter(flights, between(dep_time, 0, 600))) #equal to last line

(huh <- filter(flights, is.na(dep_time))) #arr_time is also na, so I think these flights were cancelled

#4. if an expression is true no matter what you replace NA with, than it will evaluate to the "always true" answer

#####arrange#####
(arrange(flights, desc(is.na(dep_time))))

(arrange(flights, desc(dep_delay))) #most delayed flights first
(arrange(flights, dep_delay)) #least delayed flights first

(arrange(flights, desc(air_time))) 

(arrange(flights, distance))
(arrange(flights, desc(distance)))

#####select#####
select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, 4, 6, 7, 9)
variables <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, one_of(variables))
select(flights, starts_with("dep_"), starts_with("arr_"))

(select(flights, year, year, year)) #only selects one column, does not duplicate

#3. used above, basically a shortcut "or" function

select(flights, contains("TIME")) #works even though case is different, use ignore.case to change that

#####mutate#####
transmute(flights, min_since_midnight_dep = 60 * dep_time %/% 100 + dep_time %%100)

flights_airtime <-
  mutate(flights,
         dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440, #alternate way for making time into min
         arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_diff = air_time - arr_time + dep_time
  )

#the diff is not = 0, maybe because of time zones?
#nope, reading the documentation, it turns out the air_time is not computer from dep_time and arr_time

flights_deptime <-
  mutate(flights,
         dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_time_min = (sched_dep_time %/% 100 * 60 +
                                 sched_dep_time %% 100) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
  )

#i would expect dep_delay_diff to be 0, but it isn't. 

flights_delayed <- mutate(flights,
                          dep_delay_min_rank = min_rank(desc(dep_delay))
)

(1:3 + 1:10) #each item is added with corresponding item

?Trig
#sin, cos, and tan

#####summarize#####
#arrival delay is worse because it could make you late for a connection, whereas departure delay is fine if it doesn't disrupt other plans

(not_cancelled %>% count(dest)) #equals:
(not_cancelled %>% group_by(dest) %>% summarise(n = n()))

(not_cancelled %>% count(tailnum, wt = distance))
(not_cancelled %>% group_by(tailnum) %>% summarise(n = sum(distance)))

#3. That's bad if there is no delay for a non-cancelled flight. I would use dep_time instead

ggplot(cancelled_per_day) +
  geom_point(aes(x = flights_num, y = cancelled_num)) #more flights generally leads to more cancellations

(num_cancelled <- flights %>% count(day, wt = is.na(dep_time)))
flights_per_day <- flights %>% group_by(day) %>% summarise(n=n())
num_cancelled <- num_cancelled %>% mutate(prop = n/flights_per_day$n)

num_delayed <- flights %>% count(day, wt = dep_delay > 0)
num_delayed <- num_delayed %>% mutate(prop = n/flights_per_day$n)

ggplot(num_cancelled) +
  geom_point(mapping = aes(x=day, y = prop)) +
  geom_point(data = num_delayed, mapping = aes(x=day, y=prop), color = "blue")
#general correlation, it's difficult to see

flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))
#F9 is the worst

#sorts the results in order of n, shortcuts using arrange

#####grouping#####
#summary functions compute within groups when using groupings
#arithmetic op are not affected by grouping

flights %>%
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) #575 is the most delayed flights for a plane

flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay) #leave at 7am for the least delay

flights %>%
  group_by(dest) %>%
  summarise(arr_delay = sum(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay)) #Atlanta is the worst

lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

lagged_delays %>%
  group_by(dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")

flights %>%
  group_by(dest) %>%
  mutate(n_carriers = n_distinct(carrier)) %>%
  filter(n_carriers > 1) %>%
  group_by(carrier) %>%
  summarize(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))
