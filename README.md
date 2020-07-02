# HISa_Movements
Project analyzing movement patterns of satellite-tagged pantropical spotted dolphins in Hawaiian waters

## TravelSpeedEst_SaTags.R
Script that calculates horizontal travel speed between consecutive points for each tag. DeltaT (change in time between point 1 and point 2) is first calculated, then the distance between point 1 and point 2 is calculated using the great-circle-distance via the Vincenty ellipsoid method. Travel speed is estimated by dividing deltaD/deltaT. 

## foieGras_SaTags.R
Script that formats Douglas-filtered location data and fits a continuous-time correlated random walk model, regularized to 4 hour time intervals, using the *foieGras* package (Jonsen & Patterson, 2020). Model specifications included in the code comments. Script includes code to fit move-persistence model using CRW-predicted locations. 
