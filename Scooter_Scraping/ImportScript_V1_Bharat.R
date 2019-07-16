library(lubridate)
library(httr)
library(tidyverse)
library(jsonlite)

collectData <- function(company, link) {
  
  json_file <- fromJSON(link)
  
  
  sc_data <- json_file$data$bikes
  
  Systime <- with_tz(Sys.time(), "US/Eastern")
  
  time <- substr(Systime, start = 12, stop = 19)
  date <- substr(Systime, start = 1, stop = 10)
  
  year <- substr(Systime, start = 1, stop = 4)
  month <- substr(Systime, start = 6, stop = 7)
  day <- substr(Systime, start = 9, stop = 10)
  
  dayOfWeek <- wday(date, label = TRUE, abbr = FALSE)
  
  hour = substr(time, start = 1, stop = 2)
  minute = substr(time, start = 4, stop = 5)
  second = substr(time, start = 7, stop = 8)
  
  sc_data <- mutate(sc_data, year = year, month = month, day = day, dayOfWeek = dayOfWeek, hour = hour, minute = minute, second = second)
  
  write.csv(sc_data, file = paste(company, "Data", date, time, dayOfWeek, ".csv", sep = " "))
  
}

company <- c("Bird", "Jump")
link <- c("https://gbfs.bird.co/dc", "https://dc.jumpbikes.com/opendata/free_bike_status.json")

for(i in 1:2){
  collectData(company[[i]], link[[i]])
}


