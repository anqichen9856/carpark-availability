library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

url <- "http://www.hospitals.sg/hospitals#community-hospitals"
hospitals <- read_html(url) %>% html_nodes("h3+ ul a") %>% html_text()
hospitals_df <- data.frame(name=hospitals, address=paste("Singapore", hospitals)) %>% mutate_geocode(address) %>% select(-address)

clinics <- read.csv("data/data-original/chas-clinics-kml.csv") %>% select(c(HCI_NAME, X, Y))
names(clinics) <- c("name", "lon", "lat")

hospitals_clinics <- rbind(hospitals_df, clinics)
write.csv(hospitals_clinics, "data/data-processed/hospitals_clinics.csv", row.names = F)

View(read.csv("data/data-processed/hospitals_clinics.csv"))

