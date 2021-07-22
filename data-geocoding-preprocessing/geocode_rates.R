library(dplyr)
library(ggmap)
library(xml2)
library(rvest)

ggmap::register_google(key = 'AIzaSyCmxg0cwxF7stNPFUhhaoenc_V8plhi4rA') 

# Dataset from data.gov.sg
carpark_rates <- read.csv("data/data-original/carpark-rates.csv")

for(i in 1:nrow(carpark_rates)){
  if (carpark_rates$weekdays_rate_2[i] == "-"){
    carpark_rates$weekdays_rate_2[i] <- carpark_rates$weekdays_rate_1[i]
  }
  
  if (carpark_rates$saturday_rate[i] == "-"){
    carpark_rates$saturday_rate[i] <- carpark_rates$weekdays_rate_2[i]
  }
  
  if(carpark_rates$sunday_publicholiday_rate[i] == "-"){
    carpark_rates$sunday_publicholiday_rate[i] <- carpark_rates$saturday_rate[i]
  }
}

for (i in 1:nrow(carpark_rates)){
  if (carpark_rates$saturday_rate[i] == "Same as wkdays"){
    carpark_rates$saturday_rate[i] <- paste(carpark_rates$weekdays_rate_1[i], 
                                            carpark_rates$weekdays_rate_2[i], sep = ", ")
  }
  
  if (carpark_rates$sunday_publicholiday_rate[i] == "Same as wkdays"){
    carpark_rates$sunday_publicholiday_rate[i] <- paste(carpark_rates$weekdays_rate_1[i],
                                                        carpark_rates$weekdays_rate_2[i], sep = ", ")
  }
}

for (i in 1:nrow(carpark_rates)){
  if (carpark_rates$sunday_publicholiday_rate[i] == "Same as Saturday"){
    carpark_rates$sunday_publicholiday_rate[i] <- carpark_rates$saturday_rate[i]
  }
}

carpark_rates <- data.frame(carpark_rates, address = paste("Singapore", carpark_rates$carpark)) %>%
  mutate_geocode(address) %>% select(-address)

carpark_rates <- carpark_rates[!duplicated(carpark_rates$carpark),]

# Add in Bugis+ info that is not in original dataset
tmp <- data.frame(carpark="Bugis+", category="Capitaland", 
                  weekdays_rate_1="8.00am - 5.59pm: 1st hour @ $1.28. Subsequent 15 mins or part thereof @ $0.54",
                  weekdays_rate_2="Mon-Thu: 6.00pm - 7.59am (next morning): $3.21 per entry. Fri: 6.00pm - 11.59pm: 1st 2 hour @ $3.21. Subsequent 15 mins or part thereof @ $0.54",
                  saturday_rate="12.00am - 11.59pm: 1st 2 hour @ $3.21. Subsequent 15 mins or part thereof @ $0.54",
                  sunday_publicholiday_rate="12.00am - 11.59pm: 1st 2 hour @ $3.21. Subsequent 15 mins or part thereof @ $0.54",
                  address="Singapore Bugis+"
                  )
tmp <- mutate_geocode(tmp, address) %>% select(-address)
carpark_rates <- rbind(carpark_rates, tmp)

write.csv(carpark_rates, "data/data-processed/carpark_rates.csv", row.names = FALSE)
