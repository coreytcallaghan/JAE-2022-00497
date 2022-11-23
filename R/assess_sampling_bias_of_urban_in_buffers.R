# a script to look at the sampling proportion in different buffers
# packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(rnaturalearth)
library(urbnmapr)


sf_use_s2(FALSE)

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



# Get some of the most well-sampled points
# summarize data function
aggregate_data_500 <- function(file_name){
  
  dat <- readRDS(paste0("random_polygon_results/500_km/", file_name)) %>%
    mutate(sample=gsub("random_sample_", "", file_name)) %>%
    mutate(sample=gsub(".RDS", "", sample))
  
  return(dat)
  
}

files <- list.files("random_polygon_results/500_km")
data_500_km_all <- bind_rows(lapply(files, aggregate_data_500))

temp <- data_500_km_all %>% 
  group_by(point_ID) %>% 
  summarize(N=sum(obs)) %>%
  sample_n(250)


get_buffer_samples <- function(id_number, grain_size){
  
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
    left_join(ebird_data, by="SAMPLING_EVENT_IDENTIFIER") %>%
    mutate(buffer_id=id_number) %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, viirs, buffer_id) %>%
    distinct()
  
  return(buff_ebird_dat)
  
}

buffer_samples <- bind_rows(lapply(temp$point_ID, function(x){get_buffer_samples(x, 500000)}))

ggplot(buffer_samples, aes(x=viirs, group=buffer_id))+
  geom_density(alpha=0.8)+
  theme_bw()+
  scale_x_log10()+
  xlab("VIIRS night-time lights average radiance (log10)")+
  ylab("Density of observations")+
  ggtitle("each line represents a randomly sampled buffer")


