library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

data <- read.csv("data/data-original/list-of-government-markets-hawker-centres.csv")
names(data)[1] <- "name"
data <- data %>% mutate(address=paste("Singapore", name, ",", location_of_centre)) %>% mutate_geocode(address) %>% select(c(name, lon, lat)) %>% na.omit()
write.csv(data, "data/data-processed/hawker_centers.csv", row.names = F)
View(read.csv("data/data-processed/hawker_centers.csv"))
