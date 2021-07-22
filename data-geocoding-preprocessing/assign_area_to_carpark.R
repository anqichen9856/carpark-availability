hdbcarparks <- read.csv("data/data-processed/hdb_info.csv", stringsAsFactors = FALSE)

# Historical weather API
query_date_time <- Sys.time() %>% substr(1, 19) %>% gsub(" ", "T", .)
url <- paste0("https://api.data.gov.sg/v1/environment/rainfall?datetime=",
              query_date_time)
data <- fromJSON(url)
weather1 <- as.data.frame(data$metadata$stations) %>% select(id, location)
weather2 <- as.data.frame(data$items$readings) %>% select(value)
data_weather1 <- cbind(weather1, weather2) 

# Weather forecast API
url <- paste0("https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date_time=", 
            query_date_time)
data <- fromJSON(url)
weather1 <- as.data.frame(data$items$forecasts)
weather2 <- as.data.frame(data$area_metadata$label_location)
data_weather2 <- cbind(weather1, weather2) 

# Assign area_ids to carparks
areas <- data_weather1 %>% select(id, location)
areas$location <- areas$location[, c(2,1)]
area_col <- c()
for (n in 1:nrow(hdbcarparks)) {
  carpark_coord <- hdbcarparks[n, c("lon", "lat")]
  min_idx <- 0
  min_dist <- 999999999999
  for (i in 1:nrow(areas)) {
    dist <- distm(areas[i, 2], carpark_coord, fun=distGeo)[1,]
    if (dist < min_dist) {
      min_dist <- dist
      min_idx <- i
    }
  }
  area_col <- c(area_col, areas$id[min_idx])
}
hdbcarparks$area_id <- area_col

# Assign area_names to carparks
areas <- data_weather2 %>% select(area, longitude, latitude)
area_col <- c()
for (n in 1:nrow(hdbcarparks)) {
  carpark_coord <- hdbcarparks[n, c("lon", "lat")]
  min_idx <- 0
  min_dist <- 999999999999
  for (i in 1:nrow(areas)) {
    dist <- distm(areas[i, c(2,3)], carpark_coord, fun=distGeo)[1,]
    if (dist < min_dist) {
      min_dist <- dist
      min_idx <- i
    }
  }
  area_col <- c(area_col, areas$area[min_idx])
}
hdbcarparks$area <- area_col

write.csv(hdbcarparks, "data/data-processed/hdb_info.csv", row.names = F)