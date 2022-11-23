library(spData)
library(tidyverse)
library(sf)

us <- us_states %>%
  st_union() %>%
  st_as_sf()

st_write(us, "Data/us_shapefile.shp")
