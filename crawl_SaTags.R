## Pantropical spotted dolphin movement analysis:
## Fit CTCRW model in crawl

# Author: Michaela A. Kratofil, Cascadia Research
# Updated: 14 AUG 2020

################################################################

## load packages
library(crawl) # install devel branch of package from github
library(sf)
library(mapview) # install devel branch of package from github
library(tidyverse)
library(purrr)
library(lubridate)
library(rgdal)
library(ggplot2)
library(ggspatial)
library(janitor)
library(ptolemy) # need to install via: devtools::install_github('jmlondon/ptolemy')

## import Douglas filtered locations, Argos locations ONLY
tbl_locs <- readr::read_csv("Douglas Filtered/SaTag001-009_DouglasFiltered_KS_r20d3lc2_2020APRv1.csv",
                       col_types = cols(animal = col_character(),
                                     ptt = col_integer(),
                                     date = col_datetime(),
                                     longitud = col_double(),
                                     latitude = col_double(),
                                     LC = col_character(),
                                     error_radius = col_integer(),
                                     ellipse_orient = col_integer(),
                                     semi_major = col_integer(),
                                     semi_minor = col_integer()
                                     ))

## review data
str(tbl_locs)
summary(tbl_locs)
length(unique(tbl_locs$animal)) # 10 deployments in this dataset 
class(tbl_locs$date) # check class of date is POSIXct or POSIXt
attr(tbl_locs$date, 'tzone') # check TZ of date 

## make location class variable a factor
tbl_locs$LC <- factor(tbl_locs$LC, levels = c("DP","L3","L2","L1","L0","LA","LB","LZ"))
summary(tbl_locs$LC)

## clean data, convert everything to lower case, rename, etc.
tbl_locs <- tbl_locs %>%
  janitor::clean_names() %>% # this will drop everything to lower case to avoid typos
  dplyr::rename(lon = longitud,
                lat = latitude,
                e_radius = error_radius,
                e_orient = ellipse_orient,
                smaj = semi_major,
                smin = semi_minor) %>%
  dplyr::arrange(animal, date)

## quick summary of deployments
tbl_locs %>% dplyr::group_by(animal) %>%
  summarise(num_locs = n(),
            start_date = min(date),
            end_date = max(date))

## remove SaTag009 (short deployment)
tbl_locs <- tbl_locs %>%
  filter(animal != "SaTag009")

## assign error ellipse information for deployment locations 
tbl_locs <- tbl_locs %>%
  mutate(
    smaj = ifelse(lc == "DP", 50, smaj),
    smin = ifelse(lc == "DP", 50, smin),
    e_orient = ifelse(lc == "DP", 0, e_orient)
  )

## visualize source data with mapview (interactive map)
# first project data 
sf_locs <- sf::st_as_sf(tbl_locs, coords = c("lon","lat")) %>%
  sf::st_set_crs(4326)
st_crs(sf_locs) # check

# then organize point data into separate tracks for each deployment. the resulting object is essentially a 
# data frame with each row representing a single line for each deployment 
sf_lines <- sf_locs %>%
  arrange(animal, date) %>%
  group_by(animal) %>%
  summarise(do_union = FALSE) %>% # this piece is important! maintains point order when creating lines
  st_cast("MULTILINESTRING")

# project data to a more appropriate CRS - needed for crawl input and recommended for spatial analyses
# I use the crs EPSG:3750 (NAD83/UTM 4N) here, which works pretty well if data doesn't go too far east of
# the Big Island 
sf_locs <- sf_locs %>%
  sf::st_transform(3750)
st_crs(sf_locs) # check

sf_lines <- sf_lines %>%
  sf::st_transform(3750)
st_crs(sf_lines) # check

# get coastline data from ptolemy package
map_base <- ptolemy::extract_gshhg(sf_locs, buffer = 20000, epsg = 3750) # extract polygon data for region of loc data
plot(map_base) # check

# map theme function
theme_map <- function() {
  theme_bw() +
    theme(panel.background = element_rect(fill = 'white', colour = 'black', size = 1.25),
          axis.text = element_text(colour = 'black'),
          plot.title = element_text(colour = 'black', face = 'bold')) #+
  
}

# map tracklines
ggplot() +
  annotation_spatial(map_base, fill = 'grey', lwd = 1) +
  layer_spatial(sf_lines, size = 1, aes(color = animal)) +
  theme_map() +
  scale_color_viridis_d()

# map points 
ggplot() +
  annotation_spatial(map_base, fill = 'grey', lwd = 1) +
  layer_spatial(sf_locs %>%
                  group_by(animal) %>%
                  summarise(),
                size = 1, aes(color = animal)) +
  theme_map() +
  scale_color_viridis_d()

# interactive plots: need to convert coordinates to be on (0,360) scale for mapview to work. 
st_to_360 <- function(g) {
  
  coords <- (sf::st_geometry(g) + c(360,90)) %% c(360) - c(0,90)
  g <- sf::st_set_geometry(g, coords) %>% sf::st_set_crs(4326)
  return(g)
  
}
# map it 
sf::st_transform(sf_lines, 4326) %>%
  mapview::mapview(map.types = "Esri.OceanBasemap", zcol = 'animal',
                   burst = T, legend = F, homebutton = F)

## create nested tibble for tidy fitting with crawl
tbl_data <- sf_locs %>%
  group_by(animal) %>%
  nest()
tbl_data

## fit movement model with crawl::crwMLE() **Make sure you have the devel version of crawl installed**

# Set fixed parameters, estimated error parameters (2) and sigma (variation in velocity) and beta (velocity autocorr)
# If using error ellipse info: these will be (1,1,NA,NA) (last 2 to be estimated). Otherwise, will need to specify
# param values for each location class (e.g., c(log250), log(500), log(1500), rep(NA,3), rep(NA,2)).
fixPar <- c(1,1,NA,NA)

# Create location error covariance matrix to be fed into the model:
tbl_data <- tbl_data %>% 
  dplyr::mutate(
    diag = purrr::map(data, ~ crawl::argosDiag2Cov(
      .x$smaj, 
      .x$smin, 
      .x$e_orient)),
    data = purrr::map2(data,diag,bind_cols)) %>% 
  dplyr::select(-diag)
  

# As mentioned in the overview, there's a lot of room for user specifications (parameter constraints, optimization, etc). 
# Here I use a very general/basic model. Take the time to figure out what kind of model specifications are most
# appropriate for your research questions. 

# wrapper function for fitting model across deployments.
# I use initialSANN to obtain starting values, as that can often be challenging to do manually.
prior <- function(p) {
  dnorm(p[2], -2, 2, log = TRUE) # provide prior distribution for beta parameter
}

fit_func <- function(d, fixPar) {
  suppressWarnings(
    crwMLE(mov.model = ~ 1,
           err.model = list(
             x = ~ ln.sd.x + 0,
             y = ~ ln.sd.y + 0,
             rho = ~ error.corr
           ),
           fixPar = fixPar,
           data = d,
           Time.name = 'date',
           prior = prior,
           attempts = 50,
           control = list(maxit = 5000, trace = 0, REPORT = 1),
           initialSANN = list(maxit = 500, trace = 0, REPORT = 1))
  )
}

# fit model
tbl_data <- tbl_data %>%
  dplyr::mutate(fit = purrr::map(data, ~fit_func(d = .x, fixPar = fixPar)))

# can check parameter estimates by
tbl_data[[3]][[7]] # example, first tag 

## predict locations at 1 hour time interval
tbl_data <- tbl_data %>% 
  dplyr::filter(map_lgl(fit, ~inherits(.x,"crwFit"))) %>% 
  dplyr::mutate(pred_pts = purrr::map(fit, 
                                      ~crwPredict(.x, predTime = "1 hours", return.type = 'flat')))

## convert predicted points to sf tracks
tbl_data <- tbl_data %>% 
  dplyr::mutate(
    pts_sf = purrr::map(pred_pts, ~ crawl::crw_as_sf(.x, ftype = "POINT",
                                                     locType ="p")),
    line_sf = purrr::map(pred_pts, ~ crawl::crw_as_sf(.x, ftype = "LINESTRING",
                                                      locType = "p"))
  )
tbl_data

## unnest data and plot with mapview
tbl_data %>% 
  dplyr::select(animal,line_sf) %>% 
  tidyr::unnest() %>%
  dplyr::select(-id) %>% 
  sf::st_as_sf(crs = 3750) %>% 
  sf::st_transform(4326) %>% st_to_360() %>% 
  mapview::mapview(map.types = "Esri.OceanBasemap", zcol = "animal",
                   burst = TRUE, legend = FALSE, homebutton = FALSE)

## unnest data and get coords 
# function to inverse projected coordinates to get coords in lat/lon decimal degrees 
get_coords <- function(p, prj) {
  
  p_coords <- do.call(rbind, st_geometry(p$geometry)) %>%
    as_tibble() %>% setNames(c("longitud","latitude"))
  p_coords_df <- data.frame(x = p_coords$longitud, y = p_coords$latitude)
  
  inv_proj <- proj4::project(p_coords_df, prj, inverse = T)
  inv_proj_df <- data.frame(latitude = inv_proj$y, longitud = inv_proj$x)
  
  f <- cbind(p, inv_proj_df)
  
  return(f)
  
}

# unnest 
pred_data <- tbl_data %>%
  select(animal, pts_sf) %>%
  unnest() 

# apply the function and select columns
pred_data <- get_coords(pred_data, prj = "+proj=utm +zone=4 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
final <- pred_data %>%
  select(animal, date, latitude, longitud, se.mu.x, se.mu.y, speed, -geometry)


## write csv
write.csv(final, "SSM/SaTag001-008_crawl_1hrStep_fit1_2020AUG14.csv", row.names = F)
