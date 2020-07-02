## Pantropical spotted dolphin movement analyses:
## Fit CRW and move persistence models in foieGras

## Michaela A. Kratofil
## 18 JUN 2020

##################################################
# load packages
library(tidyverse)
library(lubridate)
library(sf)
library(foieGras)

## read in Douglas filtered location data 
df <- read.csv("SaTag001-009_DouglasFiltered_KS_r20d3lc2_2020APRv1.csv", header = T)
df$date <- as.POSIXct(df$date, tz = "UTC") # format date column

## rename variables for input into foieGras
df <- df %>%
  rename(
    id = animal,
    lc = LC,
    lon = longitud,
    lat = latitude,
    smaj = semi_major,
    smin = semi_minor,
    eor = ellipse_orient
  )

df <- select(df, id, date, lc, lon, lat, smaj, smin, eor) # select columns in required order 
df <- as_tibble(df) # make tibble
df <- filter(df, lc != "DP") # remove DP locations (foieGras hates them)

df$lc <- recode(df$lc, L3 = '3', L2 = '2', L1 = '1',
                L0 = '0', LA = 'A', LB = 'B', LZ = 'Z') # recode LC columns

df$lc <- as.factor(df$lc) # make a factor

## fit RW model: model specifications
## We use a correlated random walk model as opposed to a random walk model because the former tends to deal
## well with smaller temporal gaps between locations (median deltaT ~ .8 hours) and models correlation in
## velocity proccess.

## We choose a time step of 4 hours and set the psi parameter to 1. The psi parameter
## is meant to account for possible under estimation of Argos error from the Kalman filter, so will 
## rescale all ellipse semi-minor axes and inflates the uncertainty region around locations for estimated
## values > 1. 

## We set the numerical optimizer through the optim() function and use the default Nelder-mead
## optimization method. 

## Because estimating initial values for this type of model is challenging, we let the 
## internal sfilter of the package specify the initial values and unobserved states.
## All tags are fit simultaneously. 

# fit 
m <- fit_ssm(df, model = 'crw', spdf = F, time.step = 4, map = list(psi = factor(NA)), optim = 'optim')
m1$ssm[[7]] # check model parameters and se estimates for each tag 

# grab predicted locations 
pred <- grab(m, what = 'predicted', as_sf = F)

# save location data
write.csv(pred, "SaTag001-009_FG4hSSM_CRW_2020Jun19.csv", row.names = F)

## If desired, can fit a move persistence model using model fitted or predicted location data.
## This model generates a parameter, gamma, that serves as an index for animal behavior.
## We use the joint-move-persistence-model (jmpm) model option because we are fitting over multiple 
## tags. This option uses a pooled random variance parameter. 
# fit 
fmp <- mm %>%
  grab(., "p", as_sf = F) %>%
  select(id, date, lon, lat) %>%
  fit_mpm(., model = "jmpm")

fmp$mpm[[1]] # check parameter estimates for tags 


# grab move persistence parameters and bind back with predicted 
# FG locations
mp.df <- grab(fmp, what = 'f', as_sf = F)
g <- select(mp.df, g, g.se)
all <- bind_cols(pred, g)

# write csv for GIS output
write.csv(all, "SaTag001-009_FG4hSSM_CRW_JMPM_2020Jun26.csv", row.names = F)
