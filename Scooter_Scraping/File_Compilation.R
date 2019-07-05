setwd("/Users/tlim/Desktop/Scooter_RMP_Project/Data/")
library("geosphere")
library("zoo")
options(digits = 22)

dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){ #lifted from Stack Overflow
  radians <- pi/180
  lat_to <- lat_to * radians
  lat_from <- lat_from * radians
  lon_to <- lon_to * radians
  lon_from <- lon_from * radians
  dLat <- (lat_to - lat_from)
  dLon <- (lon_to - lon_from)
  a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
  return(2 * atan2(sqrt(a), sqrt(1 - a)) * r) #distance in meters
}

batch_read <- function(path, extension) { #I got this from github
  file_names <- list.files(path)
  data_list <- lapply(paste0(path, file_names), "read_csv")
  data_frame <- bind_rows(data_list)
  data_frame 
}

df <- batch_read("/Users/tlim/Desktop/Scooter_RMP_Project/Data/") %>%
  select(-"name") %>% #remove the name column, which is redundant
  group_by(bike_id) %>%
  mutate(trejectory_pt = row_number()) %>%
  arrange(bike_id, date_time, company) %>%
  mutate(
    dist_from = dt.haversine(lag(lat), lag(lon), lat, lon),
    dist_to = dt.haversine(lat, lon, lead(lat), lead(lon))
  ) %>%
  filter(dist_from >80 | dist_to >80)%>%
  mutate(
    time_from = (date_time - lag(date_time))/60, #time difference in minutes
    time_to = (lead(date_time) - date_time)/60
  )

