# HISa_Movements
Project analyzing movement patterns of satellite-tagged pantropical spotted dolphins in Hawaiian waters

# TravelSpeedEst_SaTags.R
Script that calculates horizontal travel speed between consecutive points for each tag. DeltaT (change in time between point 1 and point 2) is first calculated, then the distance between point 1 and point 2 is calculated using the great-circle-distance via the Vincenty ellipsoid method. Travel speed is estimated by dividing deltaD/deltaT. 
