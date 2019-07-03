library(jsonlite)
library(tidyverse)
library(lubridate)

setwd("/Users/tlim/Desktop/Scooter_RMP_Project/Data") #Data is the folder I'm using to store all the files

get_data <- function(city, company, link) { #function to import each company
  file <- jsonlite::fromJSON(link) #downloads JSON and transforms it into vector
  data <- file$data$bikes %>% as_tibble()#extracts the dataframe from vector + transforms it into tibble
  
  full_date <- Sys.time() #retrieve date from system
  
  data <- data %>% mutate(
    year = year(full_date),
    month = month(full_date),
    day_of_week = date() %>% substr(1,3), #date() is the only function which returns the day of the week
    day = day(full_date),
    hour = hour(full_date),
    minute = minute(full_date)
  )
  
  if(company == "Jump"){ #standardize the names of columns for jump scooters
    data <- data %>% rename(
      battery_level = jump_ebike_battery_level,
      vehicle_type = jump_vehicle_type
    ) %>%
    mutate(battery_level = parse_number(battery_level)) #remove % sign
  }
  
  if(company =="Lyft"){ #same as above but for Lyft scooters
    data <- data %>% rename(
      vehicle_type = type
    )
  }
  
  if(company == "Bird"){ #standardize name and obj type (boolean -> integer)
    data <- data %>% 
      rename(is_reserved = reserved, is_disabled = disabled) %>%
      mutate(is_reserved = as.integer(is_reserved), is_disabled = as.integer(is_disabled))
  }
  
  date <- full_date %>% #I use full date here because the functions year(), month(), etc wont return leading zeros ("7" instead of "07")
    substr(1,10) %>% #the "date" part of the date
    str_remove_all(pattern = "-") 
  
  time <- full_date %>% #I am keeping seconds just in case cronjob messes up and there are two readings from the same minute
    substr(12,19) %>% #the "time" part of the date
    str_remove_all(pattern = ":") 
  
  write_csv(data, paste(city, company, date, time, sep = "_") %>% paste(".csv", sep = "")) #exports file (2 paste functions because diff sep char)
}

#vectors of companies and links for convenience in the for loop below
companies <- c("Bird", "Jump", "Lime", "Lyft", "Spin") #skip is excluded because the link leads to an empty page
link <- c("https://gbfs.bird.co/dc",
          "https://dc.jumpbikes.com/opendata/free_bike_status.json",
          "https://lime.bike/api/partners/v1/gbfs/free_bike_status.json",
          "https://s3.amazonaws.com/lyft-lastmile-production-iad/lbs/dca/free_bike_status.json",
          "https://web.spin.pm/api/gbfs/v1/washington_dc/free_bike_status"
          )

for(i in 1:5){ #loops through each company and corr. link
  get_data("Washington D.C.", companies[[i]], link[[i]])
}

#crontab command:
# * * * * * /usr/local/bin/RScript /Users/tlim/Desktop/Scooter_RMP_Project/All_company_import.R