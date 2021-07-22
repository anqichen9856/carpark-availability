library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

url <- "https://www.makemytrip.com/travel-guide/singapore/places-to-visit.html"
attractions <- read_html(url) %>% html_nodes(".titleClass") %>% html_text()
attractions_df <- data.frame(name=attractions, address=paste("Singapore", attractions)) %>% mutate_geocode(address) %>% select(-address)
write.csv(attractions_df, "data/data-processed/attractions.csv", row.names = F)

View(read.csv("data/data-processed/attractions.csv"))