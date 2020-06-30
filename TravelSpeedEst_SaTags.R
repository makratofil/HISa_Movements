## Horizontal speed of travel analaysis:
## Pantropical spotted dolphin movements project

## Michaela A. Kratofil
## 30 JUN 2020

#######################################################

# load packages
library(tidyverse)
library(lubridate)
library(sf)
library(geosphere)
library(ggplot2)


# read in location data
locs <- read.csv('SaTag001-009_DouglasFiltered_KS_r20d3lc2_2020APRv1.csv', header = T)

# review data
str(locs)
summary(locs)

locs$date <- as.POSIXct(locs$date, tz = 'UTC') # format date
locs$animal <- as.factor(locs$animal) # make animal/tag a factor

## Take each tag through one at a time; easier to check for inconsistencies ##
# subset out 1 tag 
sub <- filter(locs, animal == 'SaTag001')

# calculate delta t (change in time)
sub$deltaT <- as.numeric(NA) # deltaT
sub$deltaT_r <- as.numeric(NA) # rounded deltaT

for (i in 1:nrow(sub)) {
  
  Start = sub[i, "date"]
  End = sub[i + 1, "date"]
  
  sub[i, "deltaT"] = difftime(End, Start, units = 'hours') 
  sub[i, "deltaT_r"] = round(difftime(End, Start, units = 'hours'), 0) # round digits
  
  
}


# calculate the great-circle-distance between points using the Vincenty Ellipsoid method
sub$dist <- as.numeric(NA)

for (i in 1:nrow(sub)) {
  
  lon1 = sub[i, "longitud"]
  lat1 = sub[i, "latitude"]
  
  lon2 = sub[i + 1, "longitud"]
  lat2 = sub[i + 1, "latitude"]
  
  sub[i, "dist"] <- distVincentyEllipsoid(p1 = c(lon1,lat1), p2 = c(lon2,lat2))
  
}

# make columns for km/h
sub$distKM <- sub$dist/1000
sub$kmph <- sub$distKM/sub$deltaT

# save individual speed data
sa001 <- sub

# subset tag and start at top again, until all tags are finished. Review outputs for each tag
# against KML files (i.e., measure distances b/t points, check interval calculations)

# bind all dataframes together
all <- bind_rows(sa001, sa002) %>%
  bind_rows(sa003) %>%
  bind_rows(sa004) %>%
  bind_rows(sa005) %>%
  bind_rows(sa006) %>%
  bind_rows(sa008) %>%
  bind_rows(sa009)

# round kmph to 0 
all$spd <- round(all$kmph, digits = 0)

# write csv
write.csv(all, "SaTag001-009_TravelSpeedEst_DougFilt_full.csv", row.names = F)

