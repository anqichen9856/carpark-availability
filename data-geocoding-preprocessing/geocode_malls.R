library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

url <- "https://en.wikipedia.org/wiki/List_of_shopping_malls_in_Singapore"
malls <- read_html(url) %>% html_nodes(".column-width li , h2+ ul li") %>% html_text()
malls_df <- data.frame(name=malls, address=paste("Singapore", malls)) %>% mutate_geocode(address) %>% select(-address)
write.csv(malls_df, "data/data-processed/malls.csv", row.names = F)

View(read.csv("data/data-processed/malls.csv"))
