#####Variation#####
summary(select(diamonds, x, y, z))

ggplot(diamonds, aes(x = x, y = y)) +
  geom_point()

ggplot(diamonds, aes(x = x, y = z)) +
  geom_point()

ggplot(diamonds, aes(x = y, y = z)) +
  geom_point()

#depth is the shortest, which makes sense with the typical "diamond shape"

ggplot(filter(diamonds, price < 2500), aes(x = price)) +
  geom_histogram(binwidth = 10, center = 0) #there's a hole at 1500...

diamonds %>%
  filter(carat >= 0.99, carat <= 1) %>%
  count(carat) #way more 1 carat diamonds, relatively few .99
#people like round numbers

#coord_cartesian zooms in on already-made plot
#x and ylim happen before plot is calculated, so values outside of those are dropped
#R will choose a default for binwidth

#####Missing Values#####
#1. a histogram removes NAs, a bar makes it its own bar (since it takes categorical variables)

#2. it keeps the NAs from "spreading." It ignores NAs instead of making the result NA

#####Co-variation: 1 cont, 1 categ.#####
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot() +
  geom_boxplot(mapping = aes(y = sched_dep_time, x = cancelled))
#boxplot >>> freqplot

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1))) #best indicator
#so, lower quality diamonds are more expensive bc more lower-quality diamonds are bigger

install.packages("lvplot")
library("lvplot")
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv()

ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) +
  geom_violin()

ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram() +
  facet_wrap(~cut, ncol = 3)

# geom_quasirandom() - somewhere in between violin and jitter
# geom_beeswarm() - like violin combined with geom_points

#####Co-variation: 2 categ. #####

diamonds %>%
  count(cut, color) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

flights %>%
  group_by(month, dest) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(month), y = dest, fill = dep_delay)) +
  geom_tile() #very messy looking, but idk what to do about it

#3. to keep labels from overlapping

#####Co-variation: 2 cont. #####
ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

#dist of large is more variable which makes sense since large diamonds are likely more rare and therefore unique

ggplot(diamonds, aes(x = cut, y = price, colour = cut)) +
  geom_boxplot()

#a binned plot would not show outliers that are outliers from the line, not in their own categories. In other words, binned plots hide relational outliers



