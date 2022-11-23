# This script is to assess the influence of survey effort
# based in response to this reviewer comment:
# Surveys that vary from 5 minutes to 240 have the potential uncover a wildly different species pool 
# given uneven sampling effort. This analysis, 
# however, assumes that survey effort is equivalent over this rather large time period. 

# packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(rnaturalearth)
library(urbnmapr)

# read in checklists with viirs
checklists <- read_csv("Data/checklists_viirs_scores/ebird_samples_viirs_scores.csv") %>%
  dplyr::select(2, 3) %>%
  rename(viirs=first) %>%
  rename(SAMPLING_EVENT_IDENTIFIER=SAMPLIN)

# read in eBird dataset
ebird_data <- readRDS("Data/ebird_data_raw_May.RDS") %>% 
  bind_rows(readRDS("Data/ebird_data_raw_Jun.RDS")) %>%
  bind_rows(readRDS("Data/ebird_data_raw_Jul.RDS")) %>%
  bind_rows(readRDS("Data/ebird_data_raw_Aug.RDS")) %>%
  left_join(., read_csv("Data/checklists_mod_scores/ebird_samples_mod_scores.csv") %>%
              dplyr::select(first, SAMPLIN) %>%
              rename(ghm=first) %>%
              rename(SAMPLING_EVENT_IDENTIFIER=SAMPLIN), by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., read_csv("Data/Clements-Checklist-v2019-August-2019.csv") %>%
              dplyr::filter(category=="species") %>%
              dplyr::select(category, `English name`, `scientific name`, order, family) %>%
              rename(COMMON_NAME=`English name`,
                     SCIENTIFIC_NAME=`scientific name`)) %>%
  dplyr::filter(!family %in% c("Strigidae (Owls)", "Tytonidae (Barn-Owls)",
                               "Stercorariidae (Skuas and Jaegers)", "Alcidae (Auks, Murres, and Puffins)",
                               "Sulidae (Boobies and Gannets)", "Procellariidae (Shearwaters and Petrels)",
                               "Hydrobatidae (Northern Storm-Petrels)", "Oceanitidae (Southern Storm-Petrels)")) %>%
  dplyr::filter(complete.cases(BCR_CODE))

# create a checklists sf object for spatial stuff below
checklists_sf <- checklists %>%
  left_join(ebird_data %>%
              dplyr::select(SAMPLING_EVENT_IDENTIFIER, LONGITUDE, LATITUDE) %>%
              distinct()) %>%
  dplyr::filter(complete.cases(.)) %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)

# quick plot of viirs spatial data to check it makes sense
ggplot()+
  geom_sf(data=checklists_sf, aes(color=log10(viirs)))+
  scale_color_viridis_c()

# read in US shapefile
states_sf <- get_urbn_map("states", sf = TRUE) %>%
  dplyr::filter(state_name != "Alaska") %>%
  dplyr::filter(state_name != "Hawaii")

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "grey", color = "#ffffff")

st_crs(states_sf)

# Now my goal is write a function
# that randomly creates a polygon
# and then selects all eBird checklists within that polygon
# correlates this with the data
# summarizes the median scores and adjusted scores of all species
# within that polygon
# and also summarizes the number of observations for each species
# then writes out this summary somehow

# first create a random sample of 10,000 points in the us
# potential_points <- st_sample(states_sf, 10000) %>%
#   st_as_sf() %>%
#   mutate(ID=1:nrow(.))
# 
# saveRDS(potential_points, "Data/potential_sample_points.RDS")

potential_points <- readRDS("Data/potential_sample_points.RDS")

# Get some of the most well-sampled points
# summarize data function
aggregate_data_500 <- function(file_name){
  
  dat <- readRDS(paste0("random_polygon_results/500_km/", file_name)) %>%
    mutate(sample=gsub("random_sample_", "", file_name)) %>%
    mutate(sample=gsub(".RDS", "", sample))
  
  return(dat)
  
}

data_500_km_all <- bind_rows(lapply(files, aggregate_data_500))

temp <- data_500_km_all %>% 
  group_by(point_ID) %>% 
  summarize(N=sum(obs)) %>%
  arrange(desc(N)) %>%
  slice(1:200) %>%
  sample_n(10)

assess_survey_duration_function <- function(id_number, grain_size=500000){
  
  message(paste0("Analyzing point id number: ", id_number))
  
  # filter to point
  point <- potential_points %>%
    dplyr::filter(ID==id_number)
  
  point2 <- point %>%
    st_transform(crs=4326)
  
  # create a random buffer that is specified by the grain size
  buff <- point %>%
    st_buffer(grain_size)
  
  # plot test
  states_sf %>% 
    ggplot(aes()) +
    geom_sf(fill = "grey", color = "#ffffff")+
    geom_sf(data=buff)+
    geom_sf(data=point)
  
  buff2 <- buff %>%
    st_transform(crs=st_crs(checklists_sf))
  
  states_sf %>% 
    ggplot(aes()) +
    geom_sf(fill = "grey", color = "#ffffff")+
    geom_sf(data=buff2)+
    geom_sf(data=point)
  
  # now get all eBird checklists that fall within that buffer
  lists <- checklists_sf %>%
    st_intersects(buff2) %>%
    as.data.frame()
  
  buff_ebird_dat <- checklists_sf %>%
    as.data.frame() %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, viirs) %>%
    mutate(row.id=1:nrow(.)) %>%
    right_join(., lists, by="row.id") %>%
    dplyr::select(-row.id, -col.id) %>%
    left_join(ebird_data, by="SAMPLING_EVENT_IDENTIFIER")
  
  lists_dat <- buff_ebird_dat %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, DURATION_MINUTES, viirs) %>%
    distinct() %>%
    mutate(point_ID=id_number)
  
  return(lists_dat)
  
}

lists_dat_examples <- bind_rows(lapply(temp$point_ID, assess_survey_duration_function))


ggplot(lists_dat_examples, aes(x=as.character(DURATION_MINUTES), y=viirs))+
  geom_boxplot()+
  scale_y_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Length of eBird checklist (minutes)")+
  ylab("VIIRS of eBird checklist")+
  facet_wrap(~point_ID, ncol=5)
  
  
assess_survey_duration_function_v2 <- function(id_number, grain_size=500000){
  
  message(paste0("Analyzing point id number: ", id_number))
  
  # filter to point
  point <- potential_points %>%
    dplyr::filter(ID==id_number)
  
  point2 <- point %>%
    st_transform(crs=4326)
  
  # create a random buffer that is specified by the grain size
  buff <- point %>%
    st_buffer(grain_size)
  
  # plot test
  states_sf %>% 
    ggplot(aes()) +
    geom_sf(fill = "grey", color = "#ffffff")+
    geom_sf(data=buff)+
    geom_sf(data=point)
  
  buff2 <- buff %>%
    st_transform(crs=st_crs(checklists_sf))
  
  states_sf %>% 
    ggplot(aes()) +
    geom_sf(fill = "grey", color = "#ffffff")+
    geom_sf(data=buff2)+
    geom_sf(data=point)
  
  # now get all eBird checklists that fall within that buffer
  lists <- checklists_sf %>%
    st_intersects(buff2) %>%
    as.data.frame()
  
  buff_ebird_dat <- checklists_sf %>%
    as.data.frame() %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, viirs) %>%
    mutate(row.id=1:nrow(.)) %>%
    right_join(., lists, by="row.id") %>%
    dplyr::select(-row.id, -col.id) %>%
    left_join(ebird_data, by="SAMPLING_EVENT_IDENTIFIER")
  
  lists_dat <- buff_ebird_dat %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, DURATION_MINUTES, viirs) %>%
    distinct() %>%
    mutate(point_ID=id_number)
  
  species_summary <- buff_ebird_dat %>%
    dplyr::filter(complete.cases(viirs)) %>%
    mutate(DURATION_MINUTES=as.character(DURATION_MINUTES)) %>%
    group_by(COMMON_NAME, SCIENTIFIC_NAME, DURATION_MINUTES) %>%
    summarize(obs=n(),
              mean_viirs=mean(viirs),
              median_viirs=median(viirs),
              sd_viirs=sd(viirs),
              max_viirs=max(viirs)) %>%
    dplyr::filter(obs>=100)
  
  all_samples_summary <- buff_ebird_dat %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, viirs) %>%
    distinct() %>%
    dplyr::filter(complete.cases(viirs)) %>%
    summarize(total_obs=n(),
              total_mean_viirs=mean(viirs),
              total_median_viirs=median(viirs),
              total_sd_viirs=sd(viirs),
              total_max_viirs=max(viirs))
  
  all_samples_summary <- buff_ebird_dat %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, viirs) %>%
    distinct() %>%
    dplyr::filter(complete.cases(viirs)) %>%
    summarize(total_obs=n(),
              total_mean_viirs=mean(viirs),
              total_median_viirs=median(viirs),
              total_sd_viirs=sd(viirs),
              total_max_viirs=max(viirs))
  
  temp_summary <- bind_cols(species_summary, all_samples_summary) %>%
    mutate(point_ID=id_number) %>%
    mutate(LATITUDE=st_coordinates(point2)[2]) %>%
    mutate(LONGITUDE=st_coordinates(point2[1])) %>%
    mutate(UT_median=median_viirs-total_median_viirs) %>%
    mutate(UT_mean=mean_viirs-total_mean_viirs)
  
  mat <- temp_summary %>%
    ungroup() %>%
    dplyr::select(COMMON_NAME, DURATION_MINUTES, UT_median) %>%
    distinct() %>%
    pivot_wider(names_from=DURATION_MINUTES, values_from=UT_median) %>%
    dplyr::select(-COMMON_NAME) %>%
    cor(., use="pairwise.complete.obs")
  
  count_mat <- mat %>%
    psych::pairwiseCount()
  
  count_mat[count_mat < 6] <- 0
  
  count_mat[count_mat==0] <- NA
  
  mat[is.na(count_mat)] <- NA
  
  plot <- mat %>%
    ggcorrplot::ggcorrplot(lab=TRUE, lab_size=2.5)+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    theme(axis.text=element_text(size=6))+
    theme(axis.title=element_text(size=7))+
    xlab("Duration of checklist (minutes)")+
    ylab("Duration of checklists (minutes)")+
    ggtitle(paste0("Point ID: ", id_number))
  
  plot
  
  return(plot)
  
} 
  

a <- assess_survey_duration_function_v2(527)
b <- assess_survey_duration_function_v2(557)
c <- assess_survey_duration_function_v2(634)
d <- assess_survey_duration_function_v2(898)
e <- assess_survey_duration_function_v2(1234)
f <- assess_survey_duration_function_v2(1414)
g <- assess_survey_duration_function_v2(1453)
h <- assess_survey_duration_function_v2(1661)
i <- assess_survey_duration_function_v2(1894)
j <- assess_survey_duration_function_v2(2000)


library(patchwork)

a + b + c + d + e + f + g + h + i + j + plot_layout(ncol=5) + plot_layout(guides = "collect")





