# prelim EDA
# packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readr)
library(sf)
library(ggridges)
library(forcats)
library(patchwork)
library(rnaturalearth)

source("R/global_functions.R")

files <- list.files("random_polygon_results/500_km")

# summarize data function
aggregate_data_500 <- function(file_name){
  
  dat <- readRDS(paste0("random_polygon_results/500_km/", file_name)) %>%
    mutate(sample=gsub("random_sample_", "", file_name)) %>%
    mutate(sample=gsub(".RDS", "", sample))
  
  return(dat)
  
}

data_500_km_all <- bind_rows(lapply(files, aggregate_data_500))

length(unique(data_500_km_all$sample))

species_per_sample <- data_500_km_all %>%
  group_by(sample) %>%
  summarize(number_species=length(unique(COMMON_NAME)))

# Add lat/lng to the dataframe
sample_lat_lng <- readRDS("Data/potential_sample_points.RDS") %>%
  mutate(lat=st_coordinates(.)[,2]) %>%
  mutate(lng=st_coordinates(.)[,1]) %>%
  st_set_geometry(NULL) %>%
  rename(sample=ID) %>%
  mutate(sample=as.character(as.integer(sample)))

data_500_km_all <- data_500_km_all %>%
  left_join(., sample_lat_lng)

# first
# check if the resampling process is strongly correlated with the total process
# will probably be a good methods supplementary figure
ggplot(data_500_km_all, aes(x=UT_median, y=UT_median_mean))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urban tolerance using all observations")+
  ylab("Urban tolerance resampled using 50 observations")

ggsave("Figures/ut_resampling_figure.png", width=6.1, height=5.8, units="in")

# how many species per buffer met the 'cutoff'
# because this can differ based on the total number of observations
# and the region in the US where the buffer was calculated
# this is mostly about sampling design than results or anything
buffer_species <- data_500_km_all %>%
  group_by(sample) %>%
  summarize(number_species=length(unique(COMMON_NAME)))

ggplot(buffer_species, aes(x=number_species))+
  geom_histogram(color="black", fill="gray70")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of species included")+
  ylab("Number of buffers")

ggsave("Figures/species_per_buffer.png", width=6.1, height=5.8, units="in")

# calculate the standard deviation
# of all urban tolerance scores
# as well as the mean of all urban tolerance scores
# for each species, and importantly calculate the number of
# buffers a species is found in as well
species_sd <- data_500_km_all %>%
  group_by(COMMON_NAME) %>%
  summarize(N=n(),
            sd_urban=sd(UT_median),
            sd_urban_unadjusted=sd(median_viirs),
            mean_urban_unadjusted=mean(median_viirs),
            mean_urban=mean(UT_median),
            sd_total=sd(total_median_viirs),
            mean_total=mean(total_median_viirs),
            range_urban=max(UT_median)-min(UT_median)) %>%
  mutate(CV_sp=sd_urban_unadjusted/mean_urban_unadjusted) %>%
  mutate(CV_buffer=sd_total/mean_total)


############################################
######### Now look at some spatial stuff
######### Make some example plots of example species
##################################################

# read in points
# read in data necessary to do the sampling for example species
potential_points <- readRDS("Data/potential_sample_points.RDS") %>%
  dplyr::filter(ID<=2000)

subrealms <- st_read("Data/OE_subrealms/OE_subrealms.shp")

st_crs(subrealms)
st_crs(potential_points)

# assign each sampling point a subrealm
points_subrealm <- potential_points %>%
  st_transform(crs=st_crs(subrealms)) %>%
  st_within(subrealms) %>%
  as.data.frame() %>%
  rename(point_ID=row.id) %>%
  left_join(., subrealms %>%
              mutate(col.id=1:nrow(.))) %>%
  dplyr::select(-geometry) %>%
  dplyr::select(-col.id)


# NOW summarize the variability of urban tolerance among different subrealms
species_sd2 <- data_500_km_all %>%
  left_join(., points_subrealm) %>%
  group_by(COMMON_NAME, subrealm) %>%
  summarize(N=n(),
            sd_urban=sd(UT_median),
            sd_urban_unadjusted=sd(median_viirs),
            mean_urban_unadjusted=mean(median_viirs),
            mean_urban=mean(UT_median),
            sd_total=sd(total_median_viirs),
            mean_total=mean(total_median_viirs),
            range_urban=max(UT_median)-min(UT_median)) %>%
  mutate(CV_sp=sd_urban_unadjusted/mean_urban_unadjusted) %>%
  mutate(CV_buffer=sd_total/mean_total)

species_sd2 %>%
  dplyr::filter(N>=50) %>%
  group_by(COMMON_NAME) %>%
  mutate(number_subrealms=length(unique(subrealm))) %>%
  dplyr::filter(number_subrealms>=4) %>%
  ggplot(., aes(x=COMMON_NAME, y=sd_urban))+
  geom_point()+
  coord_flip()+
  theme_bw()


# NOW fit mixed models again
model_dat <- data_500_km_all %>%
  left_join(., points_subrealm)


original_mod <- lme4::lmer(UT_median ~ (1|COMMON_NAME) + (1|sample), data=model_dat)
summary(original_mod)

ecoregion_mod <- lme4::lmer(UT_median ~ (1|COMMON_NAME/subrealm) + (1|sample), data=model_dat)
summary(ecoregion_mod)


