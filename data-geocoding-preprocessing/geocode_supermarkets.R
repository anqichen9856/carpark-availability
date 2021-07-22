library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

supermarkets <- read.csv("data/data-original/supermarkets-kml.csv") %>% mutate(LIC_NAME=paste(LIC_NAME, STR_NAME)) %>% select(c(LIC_NAME, X, Y))
names(supermarkets) <- c("name", "lon", "lat")
supermarkets <- supermarkets %>% mutate(name=gsub("PTE. LTD. |PTE LTD |CO-OPERATIVE LTD |PRIVATE LIMITED ", "", name))
write.csv(supermarkets, "data/data-processed/supermarkets.csv", row.names = F)

View(read.csv("data/data-processed/supermarkets.csv"))