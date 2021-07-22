# Load libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(geosphere)
library(ggmap)
library(stringr)
library(plyr)
library(dplyr)
library(rvest)
library(jsonlite)
library(httr)
library(gmapsdistance)
library(lubridate)
library(leaflet.extras)
library(erer)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(maptools)
library(sp)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA')

# Read files
locations <- read.csv("data/data-processed/locations.csv", stringsAsFactors = FALSE)
hdbcarparks <- read.csv("data/data-processed/hdb_info.csv", stringsAsFactors = FALSE)
carpark_rates <- read.csv("data/data-processed/carpark_rates.csv", stringsAsFactors = FALSE)
attractions <- read.csv("data/data-processed/attractions.csv", stringsAsFactors = FALSE)
bus <- read.csv("data/data-processed/bus.csv", stringsAsFactors = FALSE)
condominiums <- read.csv("data/data-processed/condominiums.csv", stringsAsFactors = FALSE)
hos_clinics <- read.csv("data/data-processed/hospitals_clinics.csv", stringsAsFactors = FALSE)
hawkercenters <- read.csv("data/data-processed/hawker_centers.csv", stringsAsFactors = FALSE)
hdbflats <- read.csv("data/data-processed/hdb.csv", stringsAsFactors = FALSE)
hotels <- read.csv("data/data-processed/hotels.csv", stringsAsFactors = FALSE)
malls <- read.csv("data/data-processed/malls.csv", stringsAsFactors = FALSE)
mrt_lrt <- read.csv("data/data-processed/mrt_lrt.csv", stringsAsFactors = FALSE)
schools <- read.csv("data/data-processed/schools.csv", stringsAsFactors = FALSE)
sports <- read.csv("data/data-processed/sport_facilities.csv", stringsAsFactors = FALSE)
supermarkets <- read.csv("data/data-processed/supermarkets.csv", stringsAsFactors = FALSE)
about <- paste(readLines("about.txt"), collapse=' ')
shape <- readOGR(dsn = "data/singapore-towns", layer = "Area")

# Get capitaland real time lots available
getCapitalandRealTimeLots <- function() {
  url <- paste("https://justpark.capitaland.com/LotsAvail")
  capitaland <- read_html(url)
  mallnames <- html_nodes(capitaland, ".mall .heading") %>% gsub("<.*?>", "", .)
  lots <- html_nodes(capitaland, ".lotscount span") %>% gsub("<.*?>", "", .)
  lotsavailable <- data.frame(mall = mallnames, lots_available = lots)
  # Mall name consistency
  lotsavailable[1,1] = "Bugis+"
  lotsavailable[2,1] = "Clarke Quay Central"
  lotsavailable[10,1] = "Lot One"
  lotsavailable[11,1] = "IMM"
  return(lotsavailable)
}

# Get predicted weather at destination from 2 hour weather forecast API
# (4 columns SG Towns e.g. Bedok, Lat, Lon, Forecast e.g. Thunderstorm)
getPredictedWeather <- function(datetime) {
  query_date_time <- datetime %>% substr(1, 19) %>% gsub(" ", "T", .)
  url <- paste0("https://api.data.gov.sg/v1/environment/2-hour-weather-forecast?date_time=", 
                query_date_time)
  data <- fromJSON(url)
  weather1 <- as.data.frame(data$items$forecasts)
  weather2 <- as.data.frame(data$area_metadata$label_location)
  data_weather <- cbind(weather1, weather2) #SG Town locations, forecast, lat, lon
  return(data_weather)
}

# Get historical weather data for extrapolation
getHistoricalWeather <- function(datetime) {
  query_date_time <- datetime %>% substr(1, 19) %>% gsub(" ", "T", .)
  url <- paste0("https://api.data.gov.sg/v1/environment/rainfall?datetime=",
                query_date_time)
  data <- fromJSON(url)
  weather1 <- as.data.frame(data$metadata$stations) %>% select(id, location)
  weather2 <- as.data.frame(data$items$readings) %>% select(value)
  data_weather <- cbind(weather1, weather2) 
  return(data_weather)
}

# Categorizes predicted weather (got from weather forecast API) into three categories
categorizeWeather <- function(str) {
  no_rain <- c("Fair", "Fair & Warm", "Partly Cloudy", "Cloudy", 
               "Hazy", "Slightly Hazy", "Windy", "Mist")
  light_rain <- c("Light Rain", "Passing Showers", "Light Showers", "Moderate Rain", "Showers")
  heavy_rain <- c("Heavy Rain", "Heavy Showers", "Thundery Showers", "Heavy Thundery Showers", "Heavy Thundery Showers with Gusty Winds")
  if (str %in% no_rain) {
    return("no rain")
  }
  if (str %in% light_rain) {
    return("light rain")
  }
  if (str %in% heavy_rain) {
    return("heavy rain")
  }
}

# Categorizes rainfall (got from historical weather API) into three categories
categorizeRainfall <- function(x) {
  if (x==0) {
    return("no rain")
  }
  if (x > 0 & x <= 4) {
    return("light rain")
  }
  if (x > 4) {
    return("heavy rain")
  }
}

# Get HDB carparks real time lots, for cars
getRealTimeCarparkAvailability <- function(datetime) {
  query_date_time <- datetime %>% substr(1, 19) %>% gsub(" ", "T", .)
  url <- paste0("https://api.data.gov.sg/v1/transport/carpark-availability?date_time=", query_date_time)
  availability <- as.data.frame(fromJSON(url))
  availability1 <- availability$items.carpark_data[[1]]
  availability2 <- data.frame(total_lots = NA,
                              lot_type = NA,
                              lots_available = NA, 
                              carpark_number = availability1[[2]], 
                              update_datetime = availability1[[3]])
  
  for(i in 1:nrow(availability2)){
    carpark_info <- availability1[[1]][[i]]
    carpark_info <- carpark_info[carpark_info$lot_type == "C"]
    if (length(carpark_info) == 3){
      availability2$total_lots[[i]] <- carpark_info$total_lots[carpark_info$lot_type == "C"]
      availability2$lot_type[[i]] <- carpark_info$lot_type[carpark_info$lot_type == "C"]
      availability2$lots_available[[i]] <- carpark_info$lots_available[carpark_info$lot_type == "C"]
    }
  }
  avail_final <- availability2[complete.cases(availability2),]
  return(avail_final)
}

# Get predicted carpark availability at destination, extrapolated from historical availability & weather
getPredictedCarparkAvailability <- function(datetime, mins, carpark_name) {
  arrival_time <- datetime + minutes(mins)
  area_name <- (hdbcarparks %>% filter(car_park_no==carpark_name))$area
  area_id <- (hdbcarparks %>% filter(car_park_no==carpark_name))$area_id
  # Predicted weather at destination
  arrival_weather <- (getPredictedWeather(arrival_time) %>% filter(area==area_name))$forecast %>% categorizeWeather()
  
  i <- 1
  counter <- 0
  lots <- c()
  while(TRUE) {
    query_time <- arrival_time - weeks(i) # Same day of the week
    # Get historical weather at the same time and the same day of the week
    weather <- (getHistoricalWeather(query_time) %>% filter(id==area_id))$value %>% categorizeRainfall()
    if (weather==arrival_weather) { # Only take historical data that has the same weather condition
      avail <- getRealTimeCarparkAvailability(query_time) %>% filter(carpark_number==carpark_name)
      if (nrow(avail) == 0) {
        return("")
      }
      availlots <- avail$lots_available[avail$carpark_number==carpark_name]
      lots <- c(lots, as.numeric(availlots))
      counter <- counter + 1
      # Extrapolate using four past samples
      if (counter==4) {
        break
      }
    }
    i <- i + 1
  }
  meanlots <- round(mean(lots))
  return(meanlots)
}

# For finding nearest ...
findNearest <- function(input_loc, cat_data) {
  input_coord <- input_loc %>% select(lon,lat)
  cat_coord <- cat_data %>% select(lon,lat)
  distances <- distm(input_coord, cat_coord, fun=distGeo)[1,]
  min_dist_id <- which.min(distances)
  min_dist <- min(distances)
  nearest <- data.frame(name=str_to_title(cat_data$name[min_dist_id]), 
                        dist=as.integer(round(min_dist, -1)), # Round to nearest 10m
                        lon=cat_data$lon[min_dist_id], 
                        lat=cat_data$lat[min_dist_id])
  return(nearest)
}

# For displaying nearest ... and distance label
displayNearest <- function(input_loc, nearest, icon_str, map) { 
  input_coord <- input_loc %>% select(lon,lat)
  nearest_coord <- nearest %>% select(lon, lat)
  display_pair <- rbind(input_coord, nearest_coord)
  dist_label <- paste0(nearest[1, 'dist'], "m")
  popuplabel <- paste0("<style>span{text-align: center;}</style><span><b>", nearest$name, "</b></span>")
  map <- map %>% addMarkers(data=nearest, lng=~lon, lat=~lat, popup=popuplabel, icon=~icons[icon_str])
  map <- map %>% addPolylines(data=display_pair, lng=~lon, lat=~lat, label=dist_label, color="green",
                      labelOptions = labelOptions(noHide = T, direction = "middle", 
                      style = list("font-size" = "15px",
                                   "box-shadow" = "2px 2px rgba(0,0,0,0.25)", "padding" = "1px")))
  return(map)
}

# Nearest 3 HDB Carparks (for tab 1)
findNearestHDBCarparks <- function(input_loc){
  input_coord <- input_loc %>% select(lon,lat)
  cat_coord <- hdbcarparks %>% select(lon,lat)
  distances <- distm(input_coord, cat_coord, fun=distGeo)[1,] 
  distances_sorted <- sort(distances)
  nearest_3 <- data.frame(name=NULL, dist=NULL, lon=NULL, lat=NULL)
  for (i in 1:3) {
    curr_dist <- distances_sorted[i]
    dist_id <- which(distances==curr_dist)
    curr <- data.frame(name=hdbcarparks$car_park_no[dist_id], 
                          dist=as.integer(round(curr_dist, -1)), # Round to nearest 10m
                          lon=hdbcarparks$lon[dist_id], 
                          lat=hdbcarparks$lat[dist_id])
    nearest_3 <- rbind(nearest_3, curr)
  }
  return(nearest_3)
}

# Nearest HDB Carpark (for tab 2 demo speed)
findNearestHDBCarpark <- function(input_loc){
  input_coord <- input_loc %>% select(lon,lat)
  cat_coord <- hdbcarparks %>% select(lon,lat)
  distances <- distm(input_coord, cat_coord, fun=distGeo)[1,]
  min_dist_id <- which.min(distances)
  min_dist <- min(distances)
  nearest <- data.frame(name=hdbcarparks$car_park_no[min_dist_id], 
                        dist=as.integer(round(min_dist, -1)), # Round to nearest 10m
                        lon=hdbcarparks$lon[min_dist_id], 
                        lat=hdbcarparks$lat[min_dist_id])
  return(nearest)
}

# Nearest 3 Mall/Hotel/Attraction Carparks (for tab 1)
findNearestMallCarparks <- function(input_loc){
  input_coord <- input_loc %>% select(lon,lat)
  cat_coord <- carpark_rates %>% select(lon,lat)
  distances <- distm(input_coord, cat_coord, fun=distGeo)[1,]
  distances_sorted <- sort(distances)
  nearest_3 <- data.frame(name=NULL, dist=NULL, lon=NULL, lat=NULL)
  for (i in 1:3) {
    curr_dist <- distances_sorted[i]
    dist_id <- which(distances==curr_dist)
    curr <- data.frame(name=carpark_rates$carpark[dist_id], 
                       dist=as.integer(round(curr_dist, -1)), # Round to nearest 10m
                       lon=carpark_rates$lon[dist_id], 
                       lat=carpark_rates$lat[dist_id])
    nearest_3 <- rbind(nearest_3, curr)
  }
  return(nearest_3)
}

# Nearest Mall/Hotel/Attraction Carpark (for tab2 demo speed)
findNearestMallCarpark <- function(input_loc){
  input_coord <- input_loc %>% select(lon,lat)
  cat_coord <- carpark_rates %>% select(lon,lat)
  distances <- distm(input_coord, cat_coord, fun=distGeo)[1,]
  min_dist_id <- which.min(distances)
  min_dist <- min(distances)
  nearest <- data.frame(name=carpark_rates$carpark[min_dist_id], 
                        dist=as.integer(round(min_dist, -1)), # Round to nearest 10m
                        lon=carpark_rates$lon[min_dist_id], 
                        lat=carpark_rates$lat[min_dist_id])
  return(nearest)
}

# Compute distance between start and end point
findDistance <- function(input_loc, start_loc){
  input_coord <- input_loc %>% select(lon,lat)
  departure_loc <- as.data.frame(geocode(paste("Singapore", start_loc)))
  mylocation <- paste0(departure_loc$lat, "+", departure_loc$lon)
  destination_coord <- paste0(input_coord$lat, "+", input_coord$lon)
  drivetime <- gmapsdistance(origin = mylocation, destination = destination_coord, 
                             mode = "driving", key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA')
  return(drivetime)
}


# Create dataframe of weather icon types
weather_cat <- c("Fair", "Fair & Warm", "Partly Cloudy", "Cloudy", 
                 "Hazy", "Slightly Hazy", "Windy", "Mist", "Light Rain",
                 "Moderate Rain", "Heavy Rain", "Passing Showers", "Light Showers", 
                 "Showers", "Heavy Showers", "Thundery Showers", "Heavy Thundery Showers", 
                 "Heavy Thundery Showers with Gusty Winds")

weathericonnames <- c("fair", "fairwarm", "partlycloudy", "cloudy", 
                      "hazy", "slightlyhazy", "windy", "mist", "lightrain",
                      "moderaterain", "heavyrain", "passingshowers", "lightshowers", 
                      "showers", "heavyshowers", "thunderyshowers", "heavythunderyshowers", 
                      "heavythunderyshowerswithgustywinds")

weathericons_df <- data.frame(category = weather_cat,
                              weathericon_name = weathericonnames)

# Generate icons for map
icons <- iconList(
  bus = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1042/1042263.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  clinic = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3010/3010979.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  condominium = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2188/2188540.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  departure = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1301/1301472.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  mall = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2080/2080201.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  mallcarpark = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/608/608690.svg",
    iconWidth=35, iconHeight=35, iconAnchorX=0, iconAnchorY=0
  ),
  hawkercenter = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/918/918318.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  hdbcarpark = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/898/898315.svg",
    iconWidth=35, iconHeight=35, iconAnchorX=0, iconAnchorY=0
  ),
  hdbflat = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2101/2101155.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  hotel = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/897/897061.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  school = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2883/2883921.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  sport = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3043/3043918.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  supermarket = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3050/3050228.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  attraction = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3504/3504251.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  train = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2855/2855645.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  fair = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/869/869767.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  fairwarm = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2917/2917249.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  partlycloudy = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/899/899683.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  cloudy = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/899/899681.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  hazy = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1281/1281186.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  slightlyhazy = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1197/1197102.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  windy = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/899/899685.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  mist = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3226/3226478.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  lightrain = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3351/3351979.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  moderaterain = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3217/3217126.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  heavyrain = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3721/3721920.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  passingshowers = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1004/1004482.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  lightshowers = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/899/899690.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  showers = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1779/1779907.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  heavyshowers = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1809/1809714.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  thunderyshowers = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/3026/3026385.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  heavythunderyshowers = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/2676/2676030.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  ),
  heavythunderyshowerswithgustywinds = makeIcon(
    iconUrl = "https://www.flaticon.com/svg/static/icons/svg/1779/1779963.svg",
    iconWidth=30, iconHeight=30, iconAnchorX=0, iconAnchorY=0
  )
)

# Create dataframe of icons type
icons_df <- data.frame(category=unique(locations$category), 
                       icon_name=c("attraction", "condominium", "hawkercenter",
                                   "hdbflat", "clinic", "hotel", "mall", "train", "bus", 
                                   "school", "sport", "supermarket"))

function(input, output) {
  
  # First tab: Leaflet
  map_reaching <- reactive({
    # When destination is selected
    if (length(input$dest_reaching) > 0) {
      # Generate base map
      map <- leaflet() %>% addTiles() 

      # Selected destination
      input_loc <- locations[which(locations$name == input$dest_reaching),]

      if ('HDB Carparks' %in% input$carpark_type) {
        input_coord <- input_loc %>% select(lon,lat)
        nearest_3 <- findNearestHDBCarparks(input_loc)
        for (i in 1:3) {
            nearest <- nearest_3[i,]
            nearest_coord <- nearest %>% select(lon, lat)
            display_pair <- rbind(input_coord, nearest_coord)
            dist_label <- paste0(nearest[1, 'dist'], "m")
            avail_final <- getRealTimeCarparkAvailability(Sys.time())
            
            if (nearest$name %in% avail_final$carpark_number){
              # Lots availability
              availlots <- avail_final$lots_available[avail_final$carpark_number==nearest$name]
              totallots <- avail_final$total_lots[avail_final$carpark_number==nearest$name]
              popupdetails <- paste0("<b>", nearest$name, "</b><br>", availlots, " out of ", 
                                     totallots, " lots available")
            } else{
              popupdetails <- paste0("<b>", nearest$name, "</b>")
            }
            
            map <- map %>% addMarkers(data=nearest, lng=~lon, lat=~lat, popup=popupdetails, icon=~icons['hdbcarpark'])
            map <- map %>% addPolylines(data=display_pair, lng=~lon, lat=~lat, label=dist_label, 
                      labelOptions = labelOptions(noHide = T, direction = "middle", 
                      style = list("font-size" = "12px", "box-shadow" = "2px 2px rgba(0,0,0,0.25)", "padding" = "2px")))
        }
      }
      
      if ('Shopping Mall/Hotel/Tourist Attraction Carparks' %in% input$carpark_type) {
        input_coord <- input_loc %>% select(lon,lat)
        nearest_3 <- findNearestMallCarparks(input_loc)
        for (i in 1:3) {
          nearest <- nearest_3[i,]
          nearest_coord <- nearest %>% select(lon, lat)
          display_pair <- rbind(input_coord, nearest_coord)
          dist_label <- paste0(nearest[1, 'dist'], "m")
          popupdetails <- paste0("<b>", nearest$name, "</b>")
          map <- map %>% addMarkers(data=nearest, lng=~lon, lat=~lat, popup=popupdetails, icon=~icons['mallcarpark'])
          map <- map %>% addPolylines(data=display_pair, lng=~lon, lat=~lat, label=dist_label, color="red",
                      labelOptions = labelOptions(noHide = T, direction = "middle", 
                      style = list("font-size" = "12px", "box-shadow" = "2px 2px rgba(0,0,0,0.25)", "padding" = "1px")))
        }
      }
      
      # Display neareset categories
      if ('Hawker Centers' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hawkercenters)
        map <- displayNearest(input_loc, nearest, 'hawkercenter', map)
      }
      if ('Hospitals & Clinics' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hos_clinics)
        map <- displayNearest(input_loc, nearest, 'clinic', map)
      }
      if ('Hotels' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hotels)
        map <- displayNearest(input_loc, nearest, 'hotel', map)
      }
      if ('Schools' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, schools)
        map <- displayNearest(input_loc, nearest, 'school', map)
      }
      if ('Shopping Malls' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, malls)
        map <- displayNearest(input_loc, nearest, 'mall', map)
      }
      if ('Sports Facilities' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, sports)
        map <- displayNearest(input_loc, nearest, 'sport', map)
      }
      if ('Supermarkets' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, supermarkets)
        map <- displayNearest(input_loc, nearest, 'supermarket', map)
      }
      if ('Tourist Attractions' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, attractions)
        map <- displayNearest(input_loc, nearest, 'attraction', map)
      }
      if ('Condominiums' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, condominiums)
        map <- displayNearest(input_loc, nearest, 'condominium', map)
      }
      if ('HDB Flats' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hdbflats)
        map <- displayNearest(input_loc, nearest, 'hdbflat', map)
      }
      if ('Bus Stations' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, bus)
        map <- displayNearest(input_loc, nearest, 'bus', map)
      }
      if ('MRT/LRT Stations' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, mrt_lrt)
        map <- displayNearest(input_loc, nearest, 'train', map)
      }

      # Show selected destination on map
      input_loc$iconname <- (icons_df %>% filter(category==input_loc$category))$icon_name
      popuplabel <- paste0('<b>', input_loc$name, "</b>")
      map <- map %>% addMarkers(data=input_loc, lng=~lon, lat=~lat, popup = popuplabel, 
                  icon = ~icons[iconname]) %>% setView(input_loc$lon, input_loc$lat, zoom=15)
      return(map)
    }
    else{
     map <- leaflet() %>% setView(lat=1.36, lng=103.82, zoom=11) %>% addTiles()
    }
  }
)
 output$map_reaching <- renderLeaflet(map_reaching())

  # First tab: Display relevant info
  output$result_text <- renderUI({

    if (length(input$dest_reaching) > 0) {
      
      # Index of selected destination mall
      loc_id <- which(locations$name == input$dest_reaching)
      input_loc <- locations[loc_id,]

      # Generate string to contain distance results
      result_string <- character()

      if ('HDB Carparks' %in% input$carpark_type) {
        result_string <- paste0(result_string, '<span style="font-size:16px">', '<b>Nearest HDB Carparks: </b></span><br>')
        nearest_3 <- findNearestHDBCarparks(input_loc)
        for (i in 1:3) {
          nearest <- nearest_3[i,]
          result_string <- paste0(result_string, '<b>', nearest$name, ' </b>(', nearest$dist, 'm)<br>')
          cp_loc <- which(hdbcarparks$car_park_no == nearest$name)
          cp_typeofparking <- hdbcarparks[cp_loc, 4]
          cp_freeparking <- hdbcarparks[cp_loc, 6]
          cp_nightparking <- hdbcarparks[cp_loc, 7]
          result_string <- paste0(result_string,'<span style="font-size:15px;color:#e67300">',
                                  '<b> Type of Parking System: </b>', cp_typeofparking, '<br>',
                                  '<b>Free Parking: </b>', cp_freeparking, '<br>',
                                  '<b>Night Parking: </b>', cp_nightparking, "</span>")
          
          # Lots availability
          avail_final <- getRealTimeCarparkAvailability(Sys.time())
          if (nearest$name %in% avail_final$carpark_number){
            availlots <- avail_final$lots_available[avail_final$carpark_number == nearest$name]
            totallots <- avail_final$total_lots[avail_final$carpark_number == nearest$name]
            result_string <- paste0(result_string, '<p style="font-size:15px;color:#0000FF">',
                                    '<b>Lots Availability: </b>', availlots, " out of ", 
                                    totallots, " lots available", "</p>")
          } else {
            result_string <- paste0(result_string, '<br><br>')
          }
        }
      }
    
      # Nearest Shopping Mall/Hotel/Tourist Attraction Carpark distance
      if ('Shopping Mall/Hotel/Tourist Attraction Carparks' %in% input$carpark_type) {
        result_string <- paste0(result_string, '<span style="font-size:16px">', '<b>Nearest Shopping Mall/Hotel/Tourist Attraction Carparks: </b></span><br>')
        nearest_3 <- findNearestMallCarparks(input_loc)
        for (i in 1:3) {
          nearest <- nearest_3[i,]
          result_string <- paste0(result_string, '<b>', nearest$name, '</b> (', nearest$dist, 'm)<br>')
          
          cp_rate_loc <- which(carpark_rates$carpark == nearest$name)
          cp_rate_wkday <- ifelse(carpark_rates[cp_rate_loc,3]==carpark_rates[cp_rate_loc,4], 
                                  carpark_rates[cp_rate_loc,3], 
                                  paste(carpark_rates[cp_rate_loc, 3], carpark_rates[cp_rate_loc, 4], sep = ", "))
          cp_rate_sat <- carpark_rates[cp_rate_loc, 5]
          cp_rate_sun <- carpark_rates[cp_rate_loc, 6]
          
          if (cp_rate_wkday == cp_rate_sat){
            if (cp_rate_wkday == cp_rate_sun){
              result_string <- paste0(result_string, '<p style="font-size:15px;color:#e67300">', 
                                      '<b>Daily Rates: </b>', cp_rate_wkday, "</p>")
            }
          } else{
            result_string <- paste0(result_string, '<p style="font-size:15px;color:#e67300">', 
                                    '<b>Weekdays: </b>', cp_rate_wkday, '<br>', '<b>Saturdays: </b>',
                                    cp_rate_sat, '<br>', '<b>Sundays: </b>',
                                    cp_rate_sun, "</p>")
          }
        }
      }
      
      if ('Bus Stations' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, bus)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest Bus Station</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Condominiums' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, condominiums)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest Condominium</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Hawker Centers' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hawkercenters)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest Hawker Center</b>: ',
                               nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('HDB Flats' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hdbflats)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest HDB Flat</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Hospitals & Clinics' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hos_clinics)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest Hospital/Clinic</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Hotels' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, hotels)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest Hotel</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('MRT/LRT Stations' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, mrt_lrt)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest MRT/LRT Station</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Schools' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, schools)
        result_string <- paste0(result_string, '<span style="font-size:15px">','<b>Nearest School</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Shopping Malls' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, malls)
        result_string <- paste0(result_string, '<span style="font-size:15px">', '<b>Nearest Shopping Mall</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Sports Facilities' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, sports)
        result_string <- paste0(result_string, '<span style="font-size:15px">', 
                                '<b>Nearest Sports Facility</b>: ', nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Supermarkets' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, supermarkets)
        result_string <- paste0(result_string, '<span style="font-size:15px">', 
                                '<b>Nearest Supermarket</b>: ', nearest$name, ' (', nearest$dist, 'm)<br>','</span>')
      }
      if ('Tourist Attractions' %in% input$relevant_info) {
        nearest <- findNearest(input_loc, attractions)
        result_string <- paste0(result_string, '<span style="font-size:15px">', '<b>Nearest Tourist Attraction</b>: ',
                                nearest$name, ' (', nearest$dist, 'm)<br>', '</span>')
      }
      HTML(result_string)
    }
  })

  #First tab: Display capitaland real time lots 
  output$capitaland_lots <- renderUI({
    
    if (length(input$dest_reaching) > 0) {
      # Index of selected destination mall
      loc_id <- which(locations$name == input$dest_reaching)
      #Get name of mall
      name <- locations[loc_id, 1]

      # Generate string to contain lots availability results
      result_string <- character()
      
      lotsavailable <- getCapitalandRealTimeLots()

      if(name %in% lotsavailable$mall){
        lotsno <- lotsavailable[lotsavailable$mall==name, 2]
        result_string <- paste(result_string,'<p style="font-size:15px;color:#008000">', 
                               '<b>Destination Real Time Available Lots</b>: ', lotsno)
      }
      HTML(result_string)
    }})
    
  
  # Second tab: Leaflet
  map_planning <- reactive({
    if (input$start_loc != ""){
      map <- leaflet() %>% addTiles() 
      departure_loc <- as.data.frame(geocode(paste("Singapore", input$start_loc)))
      # Show starting point
      popuplabel <- paste0('<b>', "Singapore ", input$start_loc, "</b>")
      map <- map %>% addMarkers(data=departure_loc, lng=~lon, lat=~lat, popup = popuplabel, 
                                icon = ~icons['departure']) %>% setView(departure_loc$lon, departure_loc$lat, zoom=15)
      
      if(length(input$dest_planning)>0){
        # Distance between the two points
        input_loc <- locations[which(locations$name == input$dest_planning),]
        drivetime <- findDistance(input_loc, input$start_loc)
        
        # Distance popup label 
        input_coord <- input_loc %>% select(lon,lat)
        display_pair <- rbind(input_coord, departure_loc)
        distancelabel <- paste0(round(drivetime$Distance/1000, 2), 'km')
        
        # Icon of destination
        input_loc$iconname <- (icons_df %>% filter(category==input_loc$category))$icon_name
        
        # Show destination on map and plot distance
        map <- map %>% addMarkers(data=input_loc, lng=~lon, lat=~lat, popup=~name, icon=~icons[iconname])
        map <- map %>% addPolylines(data=display_pair, lng=~lon, lat=~lat, label=distancelabel, color="green",
                            labelOptions = labelOptions(noHide = T, direction = "middle", 
                                                        style = list("font-size" = "12px", "box-shadow" = "2px 2px rgba(0,0,0,0.25)", "padding" = "2px")))
        
        if ('HDB Carparks' %in% input$carpark_type2) {
          input_coord <- input_loc %>% select(lon,lat)
          nearest <- findNearestHDBCarpark(input_loc)
          nearest_coord <- nearest %>% select(lon, lat)
          display_pair <- rbind(input_coord, nearest_coord)
          dist_label <- paste0(nearest[1, 'dist'], "m")
        
          mins <- round(drivetime$Time/60)
          availlots <- getPredictedCarparkAvailability(Sys.time(), mins, nearest$name)
          if (availlots != "") {
            avail_final <- getRealTimeCarparkAvailability(Sys.time())
            totallots <- avail_final$total_lots[avail_final$carpark_number==nearest$name]
            popupdetails <- paste0("<b>", nearest$name, "</b><br>Predicted: ", availlots, " out of ", 
                                   totallots, " lots available")
          } else {
            popupdetails <- paste0("<b>", nearest$name, "</b>")
          }
          
          map <- map %>% addMarkers(data=nearest, lng=~lon, lat=~lat, popup=popupdetails, icon=~icons['hdbcarpark'])
          map <- map %>% addPolylines(data=display_pair, lng=~lon, lat=~lat, label=dist_label, 
                              labelOptions = labelOptions(noHide = T, direction = "middle", 
                                                          style = list("font-size" = "12px", "box-shadow" = "2px 2px rgba(0,0,0,0.25)", "padding" = "2px")))
        }
        
        if ('Shopping Mall/Hotel/Tourist Attraction Carparks' %in% input$carpark_type2) {
          input_coord <- input_loc %>% select(lon,lat)
          nearest <- findNearestMallCarpark(input_loc)
          nearest_coord <- nearest %>% select(lon, lat)
          display_pair <- rbind(input_coord, nearest_coord)
          dist_label <- paste0(nearest[1, 'dist'], "m")
          popupdetails <- paste0("<b>", nearest$name, "</b>")
          map <- map %>% addMarkers(data=nearest, lng=~lon, lat=~lat, popup=popupdetails, icon=~icons['mallcarpark'])
          map <- map %>% addPolylines(data=display_pair, lng=~lon, lat=~lat, label=dist_label, color="red",
                              labelOptions = labelOptions(noHide = T, direction = "middle", 
                                                          style = list("font-size" = "12px", "box-shadow" = "2px 2px rgba(0,0,0,0.25)", "padding" = "1px")))
        }
        
      } 
      return(map)
    } else{
      map <- leaflet() %>% setView(lat=1.36, lng=103.82, zoom=11) %>% addTiles()
    }
  })
  output$map_planning <- renderLeaflet(map_planning())
  
  # Second tab: Display relevant info
  output$result_text2 <- renderUI({
    
    if (length(input$dest_planning) > 0) {
      
      # Index of selected destination mall
      loc_id <- which(locations$name == input$dest_planning)
      input_loc <- locations[loc_id,]
      drivetime <- findDistance(input_loc, input$start_loc)
      
      # Generate string to contain distance results
      result_string <- character()
      
      if ('HDB Carparks' %in% input$carpark_type2) {
        nearest <- findNearestHDBCarpark(input_loc)
        result_string <- paste0(result_string, '<span style="font-size:16px">', '<b>Nearest HDB Carparks: </b></span><br>',
                                '<b>', nearest$name, ' </b>(', nearest$dist, 'm)<br>')
        cp_loc <- which(hdbcarparks$car_park_no == nearest$name)
        cp_typeofparking <- hdbcarparks[cp_loc, 4]
        cp_freeparking <- hdbcarparks[cp_loc, 6]
        cp_nightparking <- hdbcarparks[cp_loc, 7]
        result_string <- paste0(result_string,'<span style="font-size:15px;color:#e67300">',
                                '<b> Type of Parking System: </b>', cp_typeofparking, '<br>',
                                '<b>Free Parking: </b>', cp_freeparking, '<br>',
                                '<b>Night Parking: </b>', cp_nightparking, "</span>")
        
        # Lots availability
        mins <- round(drivetime$Time/60)
        availlots <- getPredictedCarparkAvailability(Sys.time(), mins, nearest$name)
        if (availlots != "") {
          avail_final <- getRealTimeCarparkAvailability(Sys.time())
          totallots <- avail_final$total_lots[avail_final$carpark_number==nearest$name]
          result_string <- paste0(result_string, '<p style="font-size:15px;color:#0000FF">',
                                  '<b>Predicted Lots Availabile If Depart Now: </b>', availlots, " out of ", 
                                  totallots, " lots available", "</p>")
        } 
      }
      
      # Nearest Shopping Mall/Hotel/Tourist Attraction Carpark distance
      if ('Shopping Mall/Hotel/Tourist Attraction Carparks' %in% input$carpark_type2) {
        nearest <- findNearestMallCarpark(input_loc)
        result_string <- paste0(result_string, '<span style="font-size:15px">', '<b>Nearest Shopping Mall/Hotel/Tourist Attraction Carpark: </b></span><br>',
                                '<b>', nearest$name, ' </b>(', nearest$dist, 'm)<br>')
        
        cp_rate_loc <- which(carpark_rates$carpark == nearest$name)
        cp_rate_wkday <- ifelse(carpark_rates[cp_rate_loc,3]==carpark_rates[cp_rate_loc,4], 
                                carpark_rates[cp_rate_loc,3], 
                                paste(carpark_rates[cp_rate_loc, 3], carpark_rates[cp_rate_loc, 4], sep = ", "))
        cp_rate_sat <- carpark_rates[cp_rate_loc, 5]
        cp_rate_sun <- carpark_rates[cp_rate_loc, 6]
        
        if (cp_rate_wkday == cp_rate_sat){
          if (cp_rate_wkday == cp_rate_sun){
            result_string <- paste0(result_string, '<p style="font-size:15px;color:#e67300">', 
                                    '<b>Daily Rates: </b>', cp_rate_wkday, "</p>")
          }
        } else{
          result_string <- paste0(result_string, '<p style="font-size:15px;color:#e67300">', 
                                  '<b>Weekdays: </b>', cp_rate_wkday, '<br>', '<b>Saturdays: </b>',
                                  cp_rate_sat, '<br>', '<b>Sundays: </b>',
                                  cp_rate_sun, "</p>")
        }
      }
      HTML(result_string)
    }
  })

  output$capitaland_lots2 <- renderUI({
    if (length(input$dest_planning) > 0) {
      # Index of selected destination mall
      loc_id <- which(locations$name == input$dest_planning)
      #Get name of mall
      name <- locations[loc_id, 1]
      
      # Generate string to contain lots availability results
      result_string <- character()
      
      lotsavailable <- getCapitalandRealTimeLots() 
      
      if(name %in% lotsavailable$mall){
        lotsno <- lotsavailable[lotsavailable$mall==name,2]
        result_string <- paste(result_string,'<p style="font-size:15px;color:#008000">', '<b>Destination Real Time Available lots</b>: ', lotsno,"</p>")
      }
      HTML(result_string)
    }
  })

  # Second tab: Get the time taken to drive from user location to destination
  output$timetaken <- renderUI({
    if (length(input$dest_planning) > 0) {
      # Generate string to contain distance results
      result_string <- character()
      
      # Distance between the two points
      input_loc <- locations[which(locations$name == input$dest_planning),]
      drivetime <- findDistance(input_loc, input$start_loc)
      mins <- round(drivetime$Time/60)
      distance <- round(drivetime$Distance/1000, 1)
      result_string <- paste0(result_string, '<p style="font-size:17px;color:#0000FF">','<b>Travel Time</b>:<br>Around ', mins, 'min',
                               "</p>", '<p style="font-size:17px;color:#0000FF">','<b>Distance</b>:<br>', distance, 'km</p>')
      HTML(result_string)
    }
  })

  # Third tab: 2 Hour Weather Forecast  
  map_forecast <- reactive({
     
    # Generate base map
    map <- leaflet() %>% addTiles()
    data_weather <- getPredictedWeather(Sys.time())
     
    # When destination is selected
    if (length(input$forecast) > 0) {

      # Row number of selected area
      row <- which(data_weather$area == input$forecast)
      weather_table <- data_weather[row,]  #1 row of the area selected
       
      # Show weather forecast data on map
      categoryrow <- which(weathericons_df$category==weather_table$forecast)
      weathericon_name <- weathericons_df[categoryrow,2]
      weather_table$iconname <- weathericon_name
      map <- map %>%
         addMarkers(data=weather_table, lng=~longitude, lat=~latitude,
                    popup = paste0('<center><b>', data_weather[row,]$forecast, '</b>', '</center>'),
                    icon = ~icons[iconname]) %>%
         setView(weather_table$longitude, weather_table$latitude, zoom=15)
     } else {
       data_weather$iconname <- weathericons_df$weathericon_name[match(data_weather$forecast, weathericons_df$category)]
       shapeData <- spTransform(shape, CRS("+proj=longlat +ellps=WGS84")) #Get Singapore maps by Region polygons
       pal <- colorFactor(palette = sample(colors(), length(shapeData$PLN_AREA_N)), domain = shapeData$PLN_AREA_N)
       map <- leaflet() %>% setView(lat=1.36, lng=103.82, zoom=11) %>% addTiles() %>%
         addPolygons(data=shapeData, weight = 2, stroke = TRUE, smoothFactor = 0.1, fillOpacity = 0.65, color = ~pal(shapeData$PLN_AREA_N)) %>%
         addMarkers(data=data_weather, lng=~longitude, lat=~latitude,
                    popup = paste0('<center><b>', data_weather$area, '</b>', '</center>'),
                    icon = ~icons[iconname])
     }
   }
   )
   output$map_forecast <- renderLeaflet(map_forecast())  
  
  #Third tab: Display weather forecast info
  output$weathertext <- renderUI({
    data_weather <- getPredictedWeather(Sys.time())
    
    if (length(input$forecast) > 0) {
      # Index of selected area
      weather_id <- which(data_weather$area == input$forecast)
      #Get name of area
      name <- data_weather[weather_id, 1]

      # Generate string to contain forecast results
      result_string3 <- character()

      if(name %in% data_weather$area){
        forecast <- data_weather[data_weather$area==name, 2]
        result_string3 <- paste(result_string3,'<p style="font-size:16px">The weather forecast for the next 2 hours at', name, 'is: </p><p style="font-size:18px;color:#e67300"><b>', forecast, "</b></p>")
      }
      HTML(result_string3)
    }})
   
   # Fourth tab: About This App
   output$about <- renderUI(HTML(about))
}
