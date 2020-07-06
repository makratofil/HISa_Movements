## Speed and time of day (ToD) maps for SaTags:
## Pantropical spotted dolphin movements project

## Michaela A. Kratofil
## 06 JUL 2020

###############################################################

# load packages 
library(rgdal)
library(ggplot2)
library(tidyverse)
library(ggspatial)
library(sf)

## This code is set up to plot a few different kinds of maps: speed (non-SSM locs),
## speed (SSM locs), and time of day. 

## Speed data, non-SSM locs
spd <- read.csv("SaTag001-009_TravelSpeed_ForPlots.csv", header = T)

# review data
str(spd)
summary(spd)

# format
spd$date <- as.POSIXct(spd$date, tz = 'UTC')
spd <- filter(spd, animal != 'SaTag009') # remove SaTag009

## GIS processed location data, non-SSM locs 
locs <- read.csv("SaTag001-009_GIS_20200618.csv", header = T)

# review data
str(locs)
summary(locs)

# format
locs$animal <- as.factor(locs$animal)
locs$date <- as.POSIXct(locs$date, tz = 'UTC') # format datetime 
locs$datetimeUTC <- as.POSIXct(locs$datetimeUTC, tz = 'UTC')
locs$datetimeHST <- as.POSIXct(locs$datetimeHST, tz = 'Pacific/Honolulu')
sub <- filter(locs, animal != 'SaTag009') # remove SaTag009

## foieGras 4hr SSM location data with speed estimates 
fg.spd <- read.csv("SaTag001-009_TravelSpeedEst_FG4hSSM.csv", header = T)

# review data
str(fg.spd)
summary(fg.spd)

# format
fg.spd <- filter(fg.spd, animal != "SaTag009") # remove SaTag009
fg.spd$date <- as.POSIXct(fg.spd$date, tz = 'UTC') # format datetime 

## For time of day maps:
# format all datetime components needed for ToD determination
sub$sunrise <- as.POSIXct(sub$sunrise, tz = 'Pacific/Honolulu')
sub$sunset <- as.POSIXct(sub$sunset, tz = 'Pacific/Honolulu')

# create and fill in column for time of day: ToD2 = day/night and ToD3 = day/night/twilight
sub$ToD2 <- as.character(NA)
sub$ToD3 <- as.character(NA)

for(i in 1:nrow(sub)) {
  
  sunrise = sub[i, 'sunrise']
  sunset = sub[i, 'sunset']
  sunAltitude = sub[i, 'sunAltitude']
  datetimeHST = sub[i, 'datetimeHST']
  
  # for three-level categorical variable
  if(datetimeHST <= sunrise & abs(sunAltitude) <= 18) {
    sub[i, 'ToD3'] = 'Twilight'
  } else if(datetimeHST >= sunset & abs(sunAltitude) <= 18) {
    sub[i, 'ToD3'] = 'Twilight'
  } else if(datetimeHST > sunrise & datetimeHST < sunset) {
    sub[i, 'ToD3'] = "Day"
  } else {
    sub[i, 'ToD3'] = "Night"
  }
  
  # for two-level categorical variable 
  if(datetimeHST > sunrise & datetimeHST < sunset) {
    sub[i, 'ToD2'] = "Day"
  } else {
    sub[i, 'ToD2'] = "Night"
  }
  
}
# manually check that the for loop correctly assigned the time of day variables 

# read in coastline shapefile if don't want to use ocean basemap
coast <- readOGR("Shapefiles", layer = 'Coastline')
coast <- st_as_sf(coast)
prj = " +proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0" # projection
coastr <- st_transform(coast, crs = prj)

## for non-SSM speed maps:
# select out speed columns and dates 
spd.sub <- select(spd, animal, date, kmph) 
spd.locs <- full_join(sub, spd.sub, by = c('animal', 'date')) # join spd and location data together 

## All:
# project locations
spd.sf <- st_as_sf(fg.spd, coords = c("longitud","latitude"), crs = 4326) %>% # change the df that you need to project
  st_transform(crs = prj)

# check
str(spd.sf)
summary(spd.sf)
st_geometry(spd.sf)

# plot each animal separately
sa01 <- filter(spd.sf, animal == 'SaTag001')
sa02 <- filter(spd.sf, animal == 'SaTag002')
sa03 <- filter(spd.sf, animal == 'SaTag003')
sa04 <- filter(spd.sf, animal == 'SaTag004')
sa05 <- filter(spd.sf, animal == 'SaTag005')
sa06 <- filter(spd.sf, animal == 'SaTag006')
sa08 <- filter(spd.sf, animal == 'SaTag008')

# make tracklines 
sa01.lines <- sa01 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

sa02.lines <- sa02 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

sa03.lines <- sa03 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

sa04.lines <- sa04 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

sa05.lines <- sa05 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

sa06.lines <- sa06 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

sa08.lines <- sa08 %>%
  summarise(do_union = F) %>%
  st_cast("LINESTRING")

# get X Y corrds 
spd.coords <- as.data.frame(sf::st_coordinates(spd.sf))
spd.points <- bind_cols(fg.spd, spd.coords)

# make points with non-projected XY coords, and remove points without estimates (intervals outside of 4-24hr range).
sa01.points <- filter(spd.points, animal == "SaTag001") 
sa01.points2 <- sa01.points[complete.cases(sa01.points$kmph),]

sa02.points <- filter(spd.points, animal == "SaTag002")
sa02.points2 <- sa02.points[complete.cases(sa02.points$kmph),]

sa03.points <- filter(spd.points, animal == "SaTag003")
sa03.points2 <- sa03.points[complete.cases(sa03.points$kmph),]

sa04.points <- filter(spd.points, animal == "SaTag004")
sa04.points2 <- sa04.points[complete.cases(sa04.points$kmph),]

sa05.points <- filter(spd.points, animal == "SaTag005")
sa05.points2 <- sa05.points[complete.cases(sa05.points$kmph),]

sa06.points <- filter(spd.points, animal == "SaTag006")
sa06.points2 <- sa06.points[complete.cases(sa06.points$kmph),]

sa08.points <- filter(spd.points, animal == "SaTag008")
sa08.points2 <- sa08.points[complete.cases(sa08.points$kmph),]

# Ocean basemap if want
esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')

# palette for time of day maps
tod.pal <- c("#FFFF99", "#003366", "#333366")

## labels and lims
# island labels 
island <- c(paste0("Kaua\u02BBi"), paste0("Ni\u02BBihau"), paste0("O\u02BBahu"),
            paste0("Moloka\u02BBi"), "Maui", paste0("L\u0101na\u02BBi"), paste0("Kaho\u02BBolawe"),
            paste0("Hawai\u02BBi"))

# lat/lon coords for each island label
lat <- c(22.095198, 21.708437, 21.517844, 21.140436, 20.749506, 20.831225, 20.551420, 19.591887) # for tags 5 + 8
lat <- c(22.095198, 21.708437, 21.467844, 21.020436, 20.749506, 20.681225, 20.439420, 19.591887) # for satag006
lon <- c(-159.534170, -160.0059865, -158.000082, -156.992012, -156.257611, -156.906387, -156.496687, -155.565081) # for satag004

# make into dataframe and project
lon.df <- as.data.frame(lon)
labs <- data.frame(island = island, lat = lat)
labs.df <- bind_cols(labs, lon.df)
labs.sf <- st_as_sf(labs.df, coords = c("lon","lat"), crs = 4326) %>%
  st_transform(crs = prj)

# get label coords 
labs.coords <- as.data.frame(sf::st_coordinates(labs.sf))
labs.coords$label <- island

# check island label placement 
ggplot() +
  geom_sf(data = coastr) +
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 5.5) 

## for each tag
# SaTag001
xlim.sf1 <- c(742155.3, 875806.9) # BI
ylim.sf1 <- c(2100196, 2259865) # BI

spd.map1 <- ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = 'none') +
  #geom_sf(data = coastr) +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "bl") +
  geom_sf(data = sa01.lines, lwd = 1, color = 'gray22') +
  geom_point(data = sa01.points2, aes(X,Y, fill = kmph), shape = 21, color = 'black', # change fill (kmph or ToD2)
             size = 3) +
  scale_fill_distiller(palette = 'YlOrRd', type = 'seq', direction = 1,
                       limits = c(0.10, 9.5)) +   # change this based on scale of fill
  #scale_fill_manual(values = tod.pal) + for ToD maps 
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 3.5) +
  labs(fill = "Speed (km/h)") +
  xlab("") +
  ylab("") +
  coord_sf(crs = prj) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.title = element_text(color = 'black', face = 'bold')
  ) +
  ylim(ylim.sf1) +
  xlim(xlim.sf1)
spd.map1 # check plot 

# save plot 
ggsave(plot = spd.map1, "SaTag001_SpeedMap_FG4hSSM.jpg", width = 6.81, height = 5.47, units = 'in')

# SaTag002
xlim.sf2 <- c(500000.0, 654930.8) # Oahu
ylim.sf2 <- c(2277880, 2424729) # Oahu

spd.map2 <- ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = 'none') +
  #geom_sf(data = coastr) +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "bl") +
  geom_sf(data = sa02.lines, lwd = 1, color = 'gray22') +
  geom_point(data = sa02.points2, aes(X,Y, fill = kmph), shape = 21, color = 'black',
             size = 3) +
  #scale_fill_viridis_c(limits = c(0.20, 13)) +
  scale_fill_distiller(palette = 'YlOrRd', type = 'seq', direction = 1,
                       limits = c(0.1, 9.5)) +
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 3.5) +
  labs(fill = "Speed (km/h)") +
  xlab("") +
  ylab("") +
  coord_sf(crs = prj) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.title = element_text(color = 'black', face = 'bold'),
    legend.title = element_text(face = 'bold')
  ) +
  ylim(ylim.sf2) +
  xlim(xlim.sf2)
spd.map2

# save plot 
ggsave(plot = spd.map2, "SaTag002_SpeedMap_FG4hSSM.jpg", width = 6.81, height = 5.47, units = 'in')

# SaTag003
xlim.sf3 <- c(353541.4, 646064.9) # sa003
ylim.sf3 <- c(2212093, 2488864) # sa003

spd.map3 <- ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = 'none') +
  #geom_sf(data = coastr) +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "bl") +
  geom_sf(data = sa03.lines, lwd = 1, color = 'gray22') +
  geom_point(data = sa03.points2, aes(X,Y, fill = kmph), shape = 21, color = 'black',
             size = 3) +
  #scale_fill_viridis_c(limits = c(0.20, 13)) +
  scale_fill_distiller(palette = 'YlOrRd', type = 'seq', direction = 1,
                       limits = c(0.10, 9.5)) +
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 3.5) +
  labs(fill = "Speed (km/h)") +
  xlab("") +
  ylab("") +
  coord_sf(crs = prj) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.title = element_text(color = 'black', face = 'bold'),
    legend.title = element_text(face = 'bold')
  ) +
  ylim(ylim.sf3) +
  xlim(xlim.sf3)
spd.map3

# save plot 
ggsave(plot = spd.map3, "SaTag003_SpeedMap_FG4hSSM.jpg", width = 6.81, height = 5.47, units = 'in')

# SaTag004
xlim.sf4 <- c(543956.7, 901893.7) # OhMauNuHi
ylim.sf4 <- c(2202683, 2432398) # OhMauNuHi

spd.map4 <- ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = 'none') +
  #geom_sf(data = coastr) +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "bl") +
  geom_sf(data = sa04.lines, lwd = 1, color = 'gray22') +
  geom_point(data = sa04.points2, aes(X,Y, fill = kmph), shape = 21, color = 'black',
             size = 3) +
  #scale_fill_viridis_c(limits = c(0.20, 13)) +
  scale_fill_distiller(palette = 'YlOrRd', type = 'seq', direction = 1,
                       limits = c(0.10, 9.5)) +
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 3.5) +
  labs(fill = "Speed (km/h)") +
  xlab("") +
  ylab("") +
  coord_sf(crs = prj) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.title = element_text(color = 'black', face = 'bold'),
    legend.title = element_text(face = 'bold')
  ) +
  ylim(ylim.sf4) +
  xlim(xlim.sf4)
spd.map4

# save plot 
ggsave(plot = spd.map4, "SaTag004_SpeedMap_FG4hSSM.jpg", width = 6.81, height = 5.47, units = 'in')

# SaTag005
xlim.sf5 <- c(656752.4, 821625.7) # Mau Nu
ylim.sf5 <- c(2231001, 2359621) # Mau Nu

spd.map5 <- ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = 'none') +
  #geom_sf(data = coastr) +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "bl") +
  geom_sf(data = sa05.lines, lwd = 1, color = 'gray22') +
  geom_point(data = sa05.points2, aes(X,Y, fill = kmph), shape = 21, color = 'black',
             size = 3) +
  #scale_fill_viridis_c(limits = c(0.20, 13)) +
  scale_fill_distiller(palette = 'YlOrRd', type = 'seq', direction = 1,
                       limits = c(0.10, 9.5)) +
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 3.5) +
  labs(fill = "Speed (km/h)") +
  xlab("") +
  ylab("") +
  coord_sf(crs = prj) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.title = element_text(color = 'black', face = 'bold'),
    legend.title = element_text(face = 'bold')
  ) +
  ylim(ylim.sf5) +
  xlim(xlim.sf5)
spd.map5

# save plot
ggsave(plot = spd.map5, "SaTag005_SpeedMap_FG4hSSM.jpg", width = 6.81, height = 5.47, units = 'in')

# SaTag006
xlim.sf6 <- c(354200.0, 645641.2) # sa006
ylim.sf6 <- c(2289576, 2533152) # sa006

spd.map6 <- ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = 'none') +
  #geom_sf(data = coastr) +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "bl") +
  geom_sf(data = sa06.lines, lwd = 1, color = 'gray22') +
  geom_point(data = sa06.points2, aes(X,Y, fill = kmph), shape = 21, color = 'black',
             size = 3) +
  #scale_fill_viridis_c(limits = c(0.20, 13)) +
  scale_fill_distiller(palette = 'YlOrRd', type = 'seq', direction = 1,
                       limits = c(0.10, 9.5)) +
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 3.5) +
  labs(fill = "Speed (km/h)") +
  xlab("") +
  ylab("") +
  coord_sf(crs = prj) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.title = element_text(color = 'black', face = 'bold'),
    legend.title = element_text(face = 'bold')
  ) +
  ylim(ylim.sf6) +
  xlim(xlim.sf6)
spd.map6

# save plot 
ggsave(plot = spd.map6, "SaTag006_SpeedMap_FG4hSSM.jpg", width = 6.81, height = 5.47, units = 'in')

# SaTag008
xlim.sf8 <- c(656752.4, 821625.7) # Mau Nu
ylim.sf8 <- c(2231001, 2359621) # Mau Nu

spd.map8 <- ggplot() +
  annotation_map_tile(type = esri_ocean, zoomin = 1, progress = 'none') +
  #geom_sf(data = coastr) +
  annotation_north_arrow(location = "tr") +
  annotation_scale(location = "bl") +
  geom_sf(data = sa08.lines, lwd = 1, color = 'gray22') +
  geom_point(data = sa08.points2, aes(X,Y, fill = kmph), shape = 21, color = 'black',
             size = 3) +
  #scale_fill_viridis_c(limits = c(0.20, 13)) +
  scale_fill_distiller(palette = 'YlOrRd', type = 'seq', direction = 1,
                       limits = c(0.10, 9.5)) +
  geom_text(data = labs.coords, aes(X,Y, label = label, fontface = 'bold'), size = 3.5) +
  labs(fill = "Speed (km/h)") +
  xlab("") +
  ylab("") +
  coord_sf(crs = prj) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.title = element_text(color = 'black', face = 'bold'),
    legend.title = element_text(face = 'bold')
  ) +
  ylim(ylim.sf8) +
  xlim(xlim.sf8)
spd.map8

# save plot 
ggsave(plot = spd.map8, "SaTag008_SpeedMap_FG4hSSM.jpg", width = 6.81, height = 5.47, units = 'in')
