#####First Steps#####
library(tidyverse)
mpg

ggplot(data = mpg) #it's blank

#32 rows, 11 columns
nrow(mtcars)
ncol(mtcars)

?mpg #drv describes what kind of drive the car is

ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = hwy))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = class)) #drv and class are both un-ordered, so it doesn't show any meaningful relationship


#####Aesthetic Mappings#####

#1. because, if you are changing the color of all points, that should go outside of the aes() function
#2. categorical: manufacturer, model, trans, drv, fl, class || continuous: displ, year, cyl, cty, hwy
mpg # you can se which are "char" and which are "int" or "dbl"

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = cty)) #doesn't work bc cty is a continuous var

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cty, size = cty)) #color becomes a gradient

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, stroke = cty)) #changes the "size of the stroke" (can see with non-circular shapes)

?geom_point

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = cty > 5)) #color is determined by boolean, true is one color, false is a different color

#####Common Problems #####
#no questions in this section#

#####Facets#####
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~ cty, nrow = 2) #it rounds to integers

ggplot(data = mpg) +
  geom_point(mapping = aes(x = cyl, y = drv)) 
#the empty cells are where there are no points with that combination of features (no rear drive w/5 cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .) # the period indicates to ignore that dimension when faceting

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

#4. Less overlap, but comparisons are more difficult. With more points, faceting makes more sense than the color aethetic

?facet_wrap
#nrow and ncol set the number of rows and columns, 
#scales sets the scale of the plots, 
#as.table determines where the highest values are, 
#strip.position determines where labels are

?facet_grid
#nrow and ncol would be redundant since the grid is determined by the categories of the faceting parameters

#6. there is space for more columns in landscape orientation

##### Geometric Objects #####
#1. line, boxplot, histogram, and area (the geom name is the same as the plot name)

ggplot(
  data = mpg,
  mapping = aes(x = displ, y = hwy, color = drv)
) +
  geom_point() +
  geom_smooth(se = FALSE) #se = standard errors

#3. show_legend controls if the legend box is shown (duh), the default is TRUE, so removing it will cause R to print a legend box,
# probably removed to keep the plots the same size

#4, see above

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = mpg,
    mapping = aes(x = displ, y = hwy)
  ) +
  geom_smooth(
    data = mpg,
    mapping = aes(x = displ, y = hwy)
  )
#they look the same because the only difference is where the aes() is defined

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point() +
  geom_smooth(color = "blue", se = FALSE)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point() +
  geom_smooth(aes(group = drv), color = "blue", se = FALSE)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy, color = drv))+
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(aes(color = drv)) +
  geom_smooth(color = "blue", se = FALSE)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy))+
  geom_point(aes(color = drv)) +
  geom_smooth(aes(group = drv, linetype = drv), color = "blue", se = FALSE)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(size = 4, color = "white") +
  geom_point(aes(colour = drv))

#####Statistical Transformations#####
?stat_summary #default geom is "pointrange"

ggplot(data = diamonds) +
  geom_pointrange(mapping = aes(x=cut, y=depth, ymin = 0, ymax = 58.9))

?geom_col #geom_bar makes bars = count, geom_col will represent values already in data

#3. I made a list elsewhere, most have names in common, but not all (ex. geom_histogram, stat_bin)

?stat_smooth #computes y(predicted value) ymin+max(confidence interval) and se (std error)

#because, without group = 1, all bars are the same height since prop is computed within groups
ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, y = ..prop..) #needs group = 1
  )

ggplot(data = diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = color, y = ..prop..) #all bars are the same height
  )

##### Position Adjustments #####

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter() #adds randomness so density of points can be seen

#2. width and height (do exactly what is implied)

#3. both can be used for overlapping points, jitter slightly changes the position of the points, where as count changes the size of the points with overlap

#4. The default position for geom_boxplot() is "dodge2," which moves geoms horizontally so they don't overlap

ggplot(data = mpg, aes(x = drv, y = hwy, colour = class)) +
  geom_boxplot() #the different classes within the drv groups are moved to not overlap

#####coordinate systems#####
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = factor(1), fill = cut), width = 1) +
  coord_polar(theta = "y") 

#2. labs() adds titles and captions to a plot

?coord_map
?coord_quickmap #quick map uses a less exact, but faster projection, while map uses mercator by default

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()
#highway mileage is always > city
#coor_fixed keeps the plot a 1:1
#abline creates a line with default y=x


