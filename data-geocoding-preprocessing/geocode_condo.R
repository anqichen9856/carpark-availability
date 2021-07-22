library(rvest)
library(dplyr)
library(ggmap)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

url <- "https://www.srx.com.sg/condo/search/params?selectedDistrictIds=1,2,3,4,5,6,7,8,9,10,11,21,12,13,14,15,16,17,18,19,20,22,23,24,25,26,27,28&maxResults=20"
total_pages <- read_html(url) %>% html_nodes("strong:nth-child(5)") %>% html_text()

condo <- c()
for (i in 1:total_pages) {
  page <- read_html(paste0(url, "&page=", i))
  names <- page %>% html_nodes(".condo-result-name") %>% html_text()
  names <- names %>% gsub("\\s\\(.*?\\)", "", .) %>% gsub("\\s•.*", "", .) %>% gsub("\n|\t", "", .)
  condo <- c(condo, names)
}
condo_df <- data.frame(name=condo, address=paste("Singapore", condo)) %>% mutate_geocode(address) %>% select(-address) %>% na.omit()
condo_df <- condo_df %>% filter(lat>=1 & lat<=1.5 & lon>=103 & lon<=105)
write.csv(condo_df, "data/data-processed/condominiums.csv", row.names = F)

View(read.csv("data/data-processed/condominiums.csv"))
