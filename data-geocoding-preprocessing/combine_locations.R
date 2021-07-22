library(dplyr)

attractions <- read.csv("data/data-processed/attractions.csv") %>% mutate(category="Tourist Attractions")
condominiums <- read.csv("data/data-processed/condominiums.csv") %>% mutate(category="Condominiums")
hawker_centers <- read.csv("data/data-processed/hawker_centers.csv") %>% mutate(category="Hawker Centers")
hdb <- read.csv("data/data-processed/hdb.csv") %>% mutate(category="HDB Flats")
hospitals_clinics <- read.csv("data/data-processed/hospitals_clinics.csv") %>% mutate(category="Hospitals & Clinics")
hotels <- read.csv("data/data-processed/hotels.csv") %>% mutate(category="Hotels")
malls <- read.csv("data/data-processed/malls.csv") %>% mutate(category="Shopping Malls")
mrt_lrt <- read.csv("data/data-processed/mrt_lrt.csv") %>% mutate(category="MRT/LRT Stations")
bus <- read.csv("data/data-processed/bus.csv") %>% mutate(category="Bus Stations")
schools <- read.csv("data/data-processed/schools.csv") %>% mutate(category="Schools")
sport_facilities <- read.csv("data/data-processed/sport_facilities.csv") %>% mutate(category="Sports Facilities")
supermarkets <- read.csv("data/data-processed/supermarkets.csv") %>% mutate(category="Supermarkets")

locations <- rbind(attractions, condominiums, hawker_centers, hdb, 
                   hospitals_clinics, hotels, malls, mrt_lrt, bus,
                   schools, sport_facilities, supermarkets)

write.csv(locations, "data/data-processed/locations.csv", row.names = F)

View(read.csv("data/data-processed/locations.csv"))
