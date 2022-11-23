# Reviewer comment:
# Typically, presence-only data are compared to background data either through maximum entropy modeling, 
# inhomogeneous Poisson point process modeling, by generating pseudo-absences and using logistic regression, 
# as well as some other approaches (Elith et al. 2006). This current approach, which the authors have used before, 
# feels similar in spirit, but it is not quite the same. 
# In fact, it sort of feels somewhere in the middle between older (and discouraged) approaches that only 
# evaluate presence-only locations and newer techniques that use background data (Elith et al. 2006). 
# For example, comparing the locations birds were observed to pseudo-absences may provide a more valid metric for 
# urban tolerance with some sort of random effect to estimate within-species variability.  
# Likewise, this would get you away from needing to randomly select 2000 locations to evaluate, 
# you could instead analyze all of the data. While I am not suggesting the authors use the approach 
# I just suggested, it may be a good idea to try and 'get ahead' of readers who may be wondering about this when you introduce your analysis. 


# packages
library(tidyverse)

# read in data
example_data <- readRDS("Data/example_data_for_assessment.RDS")


# calculate the 'urban tolerance metric' using the simplistic way
species_summary <- example_data %>%
  dplyr::filter(complete.cases(viirs)) %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME, point_ID) %>%
  summarize(obs=n(),
            mean_viirs=mean(viirs),
            median_viirs=median(viirs),
            sd_viirs=sd(viirs),
            max_viirs=max(viirs)) %>%
  dplyr::filter(obs>=100)

all_samples_summary <- example_data %>%
  dplyr::select(point_ID, SAMPLING_EVENT_IDENTIFIER, viirs) %>%
  distinct() %>%
  dplyr::filter(complete.cases(viirs)) %>%
  group_by(point_ID) %>%
  summarize(total_obs=n(),
            total_mean_viirs=mean(viirs),
            total_median_viirs=median(viirs),
            total_sd_viirs=sd(viirs),
            total_max_viirs=max(viirs))

temp_summary <- species_summary %>%
  left_join(., all_samples_summary) %>%
  mutate(UT_median=median_viirs-total_median_viirs) %>%
  mutate(UT_mean=mean_viirs-total_mean_viirs)

# some ideas would not be to 'test' a more sophisticated way to get
# a response of urban tolerance for each species
# and see if the ranking is the same as it is for the simple way
# could be occupancy model?
# or maybe a generalized weighted regression?