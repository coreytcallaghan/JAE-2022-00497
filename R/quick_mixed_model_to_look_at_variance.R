# prelim EDA
# packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readr)

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


hist(data_500_km_all$UT_median)
ut_adjusted_mod <- lme4::lmer(UT_median ~ (1|COMMON_NAME) + (1|sample), data=data_500_km_all)
summary(ut_adjusted_mod)

hist(data_500_km_all$median_viirs)
hist(log10(data_500_km_all$median_viirs))
ut_unadjusted_mod <- lme4::lmer(log10(median_viirs) ~ (1|COMMON_NAME) + (1|sample), data=data_500_km_all)
summary(ut_unadjusted_mod)
