library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

hotels <- read.csv("data/data-original/hotels-kml.csv") %>% select(c(Name, X, Y))
names(hotels) <- c("name", "lon", "lat")
write.csv(hotels, "data/data-processed/hotels.csv", row.names = F)

View(read.csv("data/data-processed/hotels.csv"))