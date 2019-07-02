library(tidyverse)

(mtcars)
as_tibble(iris) #tibbles are labelled with "A tibble: row x col"

df <- data.frame(abc = 1, xyz = "a") #"a" is stored as a factor, not as a char
tibble <- tibble(abc = 1, xyz = "a")

df$x #will extract columns with a partial match
tibble$xyz

df[, "xyz"] # returns vector
tibble[, "xyz"] # returns tibble

df[, c("abc", "xyz")] #returns data frame
tibble[, c("abc", "xyz")] #still returns tibble

#3. you use double brackets[[]], because using $ would pull a column
df[[var]]

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

annoying[[1]]

ggplot(annoying, aes(x = `1`, y = `2`)) +
  geom_point()

annoying <- mutate(annoying, `3` = `2` / `1`)

annoying <- rename(annoying, one = `1`, two = `2`, three = `3`)

?enframe() #converts vectors/lists to two-col data frames, use if passed a named vector

#6. n_extra determines how many extra cols are printed
