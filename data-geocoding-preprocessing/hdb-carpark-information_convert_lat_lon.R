library(sf)
library(tidyverse)
library(ggmap)
library(dplyr)
library(stringr)
hdb_info <- read.csv("data/data-original/hdb-carpark-information.csv")

my_sf_df <- st_as_sf(hdb_info, coords = c("x_coord", "y_coord"), crs = 3414) 

my_latlon_df <- st_transform(my_sf_df, crs = 4326) 
hdb_info <- my_latlon_df %>%
  mutate(lon = st_coordinates(my_latlon_df)[,1], lat = st_coordinates(my_latlon_df)[,2])

final_hdb <- hdb_info %>% as.data.frame() %>% select(1:10,12:13)

for(i in 1:nrow(final_hdb)){
  final_hdb$address[i] <- str_to_title(final_hdb$address[i])
  final_hdb$car_park_type[i] <- str_to_title(final_hdb$car_park_type[i])
  final_hdb$type_of_parking_system[i] <- str_to_title(final_hdb$type_of_parking_system[i])
  final_hdb$short_term_parking[i] <- str_to_title(final_hdb$short_term_parking[i])
  final_hdb$free_parking[i] <- str_to_title(final_hdb$free_parking[i])
  final_hdb$night_parking[i] <- str_to_title(final_hdb$night_parking[i])
}

final_hdb$free_parking <- gsub("Ph Fr", "PH from", final_hdb$free_parking)

write.csv(final_hdb, "data/data-processed/hdb_info.csv", row.names = FALSE)
