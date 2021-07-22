library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

hdb <- read.csv("data/data-original/hdb-property-information.csv") %>% mutate(name=paste0("HDB Block ", blk_no, ", ", street)) %>% select(name)
hdb <- hdb %>% mutate(address=paste("Singapore", name)) %>% mutate_geocode(address) %>% select(-address) %>% na.omit()
write.csv(hdb, "data/data-processed/hdb.csv", row.names = F)

View(read.csv("data/data-processed/hdb.csv"))