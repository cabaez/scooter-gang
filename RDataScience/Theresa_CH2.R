#1. because the second call to my_variable doesn't have an i, it has a different character

library("tidyverse")

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

filter(mpg, cyl = 8)
filter(diamonds, carat > 3)

#3. lists all of the keyboard shortcuts. Can also use Tools -> Keyboard Shortcuts Help