library(jsonlite)
library(tidyverse)

get_data <- function(city, company, link) {
  file <- jsonlite::fromJSON(link)
  data <- file$data$bikes
  
  date <- Sys.Date() %>%
    str_remove_all("-")
  
  time <- Sys.time() %>%
    substr(start = 12, stop = 19)
  
  write.csv(data, file = paste(city, company, date, time, sep = "_") %>% paste(".csv", sep = ""))
}

companies <- c("Bird", "Jump", "Lime", "Lyft", "Spin") #skip is excluded because the link leads to an empty page
link <- c("https://gbfs.bird.co/dc",
          "https://dc.jumpbikes.com/opendata/free_bike_status.json",
          "https://lime.bike/api/partners/v1/gbfs/free_bike_status.json",
          "https://s3.amazonaws.com/lyft-lastmile-production-iad/lbs/dca/free_bike_status.json",
          "https://web.spin.pm/api/gbfs/v1/washington_dc/free_bike_status"
          )

for(i in 1:5){
  get_data("Washington D.C.", companies[[i]], link[[i]])
}