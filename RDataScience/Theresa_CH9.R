#####Tidy Data#####
#1.
# table 1: each observation is a row, each variable is a column
# table 2: the variable "type" is now a column, with the count of the "type" was a column
# table 3: the variables cases and population have been combined into one variable: rate
# table 4a and 4b: the two tables represent the count for a certain variable (cases/population)

#2.
table2

table2_cases <- filter(table2, type == "cases") %>%
  rename(cases = count)

table2_population <- filter(table2, type == "population") %>%
  rename(population = count)

rate <- tibble(
  year = table2_cases$year, #the choice to use cases is arbitrary
  country = table2_cases$country,
  cases = table2_cases$cases,
  population = table2_population$population,
  rate = cases/population * 10000
) %>% select(country, year, rate)

rate <- rate %>%
  mutate(type = "rate") %>%
  rename(count = rate)

bind_rows(table2, rate) %>%
  arrange(country, year, type) #to keep the years and countries together instead of adding all the rates at the end

table4a
table4b

rate <- tibble(
  country = table4a$country,
  `1999` = table4a$`1999`/table4b$`1999` * 10000,
  `2000` = table4a$`2000`/table4b$`2000` * 10000
) #this stays its own table to keep the formatting of the tables (one per variable) consistant

#3.
table2

table2_cases <- table2 %>% filter(type == "cases") #have to separate out cases first

ggplot(data=table2_cases, mapping = aes(x=year, y=count)) +
  geom_line(aes(group = country), color = "grey50") +
  geom_point(aes(color = country))

#####Spreading and Gathering#####

#1. because R will interpret the headers `2015` and `2016` as strings, not as numbers

#2. because you must refer to numeric column names with `1999`, not just 1999

#3. It fails because Phillip has 2 age columns, so spread can't "choose" which value to use in the age column
# one solution would be to add another column which counts which "observation" it is (to differentiate between the first and second ages)

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people_but_better <- people %>%
  group_by(name, key) %>% #means row_number will only compare the first and second ages, and ignor the height (since it's in its own group)
  mutate(observation = row_number()) %>%
  spread(key, value)

#4. variables: pregnant(boolean), gender(male, female), and count. Since we are making col names into observations, we should use gather.
preg <- tribble(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

preg <- preg %>%
  gather(male, female, key = gender, value = count, na.rm = TRUE) #removes male + pregnant row


#####Separating and Pull#####
?separate
#1. extra= what happens if sep is a character and there are more characters to seperate at than columns to separate into?
#fill = what happens if sep is a chaacter and there are not enough to separate into the number of columns

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>%
  separate(x, c("one", "two", "three"), extra = "merge")

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>%
  separate(x, c("one", "two", "three"), fill = "left")

#2.
?unite
#remove: removes the input column(s) from output
#you would set it to false if the original column is still relevant

#3.
?separate
?extract

#both turn one column into multiple columns
#separate uses a character or position to separate, whereas extract uses a "regular expression with capturing groups"
#there's only one unite because there is only one way to add together columns

#####Missing Values#####
#1. 
?fill #takes data and automaticly fills in missing values using the last non-missing value
?spread #takes data, key, and value to spread across columns
?complete #takes data, and turns implicitly missing values into explicitly missing ones

#2. it determines whether to take the previous non-missing value, or the next

#####Case Study#####

#1. the missing cases are probably for years for which there is no data
# I think this is reasonable because it just turned the explicit missing values into implicit ones
# NA = unknown, 0 = no cases

#2. than the separate step would not have worked for cols with newrel (since there is no separator)

#3. iso2 and iso3 are ISO country codes, so yes, they are redundant
?who

#4.
who <- who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(
    code = stringr::str_replace(code, "newrel", "new_rel")
  ) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1) #taken from the book directly

who_country <- who %>% group_by(country, year, sex) %>%
  mutate(count = sum(value))

ggplot(who_country, mapping = aes(x=year, y=count, color = country)) +
  geom_line(mapping = aes(linetype = sex), show.legend = FALSE) # this is the best I can do with so many countries

