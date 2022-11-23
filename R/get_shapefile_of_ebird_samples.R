# A script to get a shapefile for GEE ingestion

library(dplyr)
library(sf)
library(tidyverse)

setwd("Data")
dat <- c("ebird_data_raw_Apr.RDS", "ebird_data_raw_Aug.RDS", "ebird_data_raw_Dec.RDS", "ebird_data_raw_Feb.RDS",
         "ebird_data_raw_Jan.RDS", "ebird_data_raw_Jul.RDS", "ebird_data_raw_Jun.RDS", "ebird_data_raw_Mar.RDS",
         "ebird_data_raw_May.RDS", "ebird_data_raw_Nov.RDS", "ebird_data_raw_Oct.RDS","ebird_data_raw_Sep.RDS") %>%
  map(readRDS) %>% 
  bind_rows()
setwd("..")

# unique points/localities

shape <- dat %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, LATITUDE, LONGITUDE) %>%
  distinct() %>%
  st_as_sf(., coords=c("LONGITUDE", "LATITUDE"), crs=4326)

class(shape)


st_write(shape, "Data/shapefile_of_ebird_samples/ebird_samples.shp")



