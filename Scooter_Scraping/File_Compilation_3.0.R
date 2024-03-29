setwd("/Users/tlim/Desktop/Scooter_RMP_Project/Data/")
library("geosphere")
library("zoo")
library("tidyverse")
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
  as_tibble() %>%
  select(-"name", -"vehicle_type", -'#setup') %>% #remove the name column, which is redundant
  group_by(bike_id) %>%
  mutate(
    trejectory_pt = row_number(), #observation number (1 = first obs for a bike)
    origin_pt = row_number() - 1 #observation number for the join
    ) %>% 
  arrange(bike_id, date_time, company)

trej_df <- full_join(df, df, by = c("bike_id" = "bike_id", "trejectory_pt" = "origin_pt", "company" = "company"), suffix = c("_O", "_D")) %>% 
  dplyr::filter(!is.na(lat_D)) %>%
  mutate(
    crow_dist = dt.haversine(lat_O, lon_O, lat_D, lon_D),
    time_diff = difftime(date_time_D, date_time_O, units = "mins"),
    battery_diff = battery_level_D - battery_level_O,
    is_trip = crow_dist > 80, #tests if scooter moved over 80 meters
    is_relocation = (crow_dist/time_diff) > 402, #tests if ave speed is greater than 402 meters per min (15 miles per hour)
    is_recharge = time_diff > 120 #tests if trip is over 2 hours long
  )

write_csv(trej_df, "/Users/tlim/Desktop/Scooter_RMP_Project/sample_trejectory_data")
