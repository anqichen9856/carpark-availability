library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

primary_sec <- read.csv("data/data-original/general-information-of-schools.csv") %>% select(c(school_name, address))
names(primary_sec)[1] <- "name"
primary_sec <- primary_sec %>% mutate(address=paste("Singapore", name, ",", address))
primary_sec <- primary_sec %>% mutate_geocode(address) %>% select(-address) %>% na.omit()

polytechnic_page <- read_html("https://en.wikipedia.org/wiki/Category:Polytechnics_in_Singapore")
polytechnic <- polytechnic_page %>% html_nodes("#mw-subcategories a , #mw-pages .mw-content-ltr a") %>% html_text()
polytechnic_df <- data.frame(name=polytechnic, address=paste("Singapore", polytechnic)) %>% mutate_geocode(address) %>% select(-address)

university_page <- read_html("https://en.wikipedia.org/wiki/List_of_universities_in_Singapore")
university <- university_page %>% html_nodes("p+ ul li , td:nth-child(1) > a") %>% html_text()
university_df <- data.frame(name=university, address=paste("Singapore", university)) %>% mutate_geocode(address) %>% select(-address)

schools <- rbind(primary_sec, polytechnic_df, university_df)
write.csv(schools, "data/data-processed/schools.csv", row.names = F)

View(read.csv("data/data-processed/schools.csv"))