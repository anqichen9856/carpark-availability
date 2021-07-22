library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

data1 <- read.csv("data/data-original/AQUATICSG.csv") %>% select(c(Name, X, Y))
names(data1) <- c("name", "lon", "lat")
data2 <- read.csv("data/data-original/PLAYSG.csv") %>% filter(grepl("sport", Name, ignore.case=T)) %>% select(c(Name, X, Y))
names(data2) <- c("name", "lon", "lat")
sport <- rbind(data1, data2)
write.csv(sport, "data/data-processed/sport_facilities.csv", row.names = F)

View(read.csv("data/data-processed/sport_facilities.csv"))
