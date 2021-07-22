library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

mrt_page <- read_html("https://en.wikipedia.org/wiki/List_of_Singapore_MRT_stations")
mrt <- mrt_page %>% html_nodes("h2+ .wikitable td:nth-child(3)") %>% html_text() %>% gsub("\\s•.*", "", .) %>% gsub("\\[.*?\\]", "", .)
mrt <- mrt[!grepl("reserved for a possible future station", mrt, fixed=T)]
mrt <- paste(mrt, "MRT Station")
mrt_df <- data.frame(name=mrt, address=paste("Singapore", mrt)) %>% mutate_geocode(address) %>% select(-address) %>% na.omit()

lrt_page <- read_html("https://en.wikipedia.org/wiki/List_of_Singapore_LRT_stations")
lrt <- paste(lrt_page %>% html_nodes("td:nth-child(3) a , .wikitable td:nth-child(2) a") %>% html_text(), "LRT Station")
lrt <- lrt[lrt != " LRT Station"]
lrt_df <- data.frame(name=lrt, address=paste("Singapore", lrt)) %>% mutate_geocode(address) %>% select(-address)

bus_page <- read_html("https://en.wikipedia.org/wiki/List_of_bus_stations_in_Singapore#Depots_and_bus_parks")
bus_interchange <- bus_page %>% html_nodes(".column-width:nth-child(15) a") %>% html_text()
bus_interchange_df <- data.frame(name=bus_interchange, address=paste("Singapore", bus_interchange)) %>% mutate_geocode(address) %>% select(-address)

bus_terminal <- bus_page %>% html_nodes("p+ .column-width li") %>% html_text()
bus_terminal_df <- data.frame(name=bus_terminal, address=paste("Singapore", bus_terminal)) %>% mutate_geocode(address) %>% select(-address)

mrt_lrt <- rbind(mrt_df, lrt_df)
write.csv(mrt_lrt, "data/data-processed/mrt_lrt.csv", row.names = F)

bus <- rbind(bus_interchange_df, bus_terminal_df)
write.csv(bus, "data/data-processed/bus.csv", row.names = F)

View(read.csv("data/data-processed/mrt_lrt.csv"))
View(read.csv("data/data-processed/bus.csv"))
