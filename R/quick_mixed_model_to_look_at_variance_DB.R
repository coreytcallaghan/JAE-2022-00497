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

#add in repeatability stuff
library(rptR)
mod <- rptGaussian(UT_median ~ (1|COMMON_NAME) + (1|sample), 
                                data=data_500_km_all,
                                grname=c("COMMON_NAME","sample"),
                                parallel=TRUE, nboot=1000)
summary(mod)
plot(mod, grname='COMMON_NAME')
plot(mod, grname='sample')

# look at the estimation overview in the summary output for each grouping factor
# R is the repeatabilty i.e, intraclass correlation 
# = fraction of variation in response explainable by that grouping factor

#the point estimates can be calculated directly from the original outputs
#its just a ratio of the variance of the random effects
#i.e. for COMMON_NAME its 0.13302/(0.13302 + 0.04931 + 0.06192) from the lmer output
# but this package gives us 95% CI - so that seems to be what we want to report
