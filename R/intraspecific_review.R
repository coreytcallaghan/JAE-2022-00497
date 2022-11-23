#Reviewer comment:
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

# how many species were usually seen on a sampling event
example_data %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(nuSpecies = length(unique(SCIENTIFIC_NAME))) %>%
  pull(nuSpecies) %>%
  summary()

# #subset to make it easy on Diana's poor comp - totally arbitrary decisions!!!
# mySamples <- sample(sort(unique(example_data$SAMPLING_EVENT_IDENTIFIER)),1000)
# example_data <- example_data %>%
#                   filter(SAMPLING_EVENT_IDENTIFIER %in% mySamples) 
#scale viirs
example_data$viirs <- scale(example_data$viirs)

#convert data into presence absence matrix per sampling event
samplingLists <- example_data %>%
                  select(SAMPLING_EVENT_IDENTIFIER, viirs,ghm,
                         SCIENTIFIC_NAME, LOCALITY_ID,point_ID) %>%
                  add_column(Presence = 1) %>%
                  pivot_wider(names_from=SCIENTIFIC_NAME, 
                              values_from=Presence,
                              values_fill = 0) %>%
                  janitor::clean_names() %>%
                  arrange(sampling_event_identifier) 

# Anyhow we could fit a simple binomial glm 
library(lme4)
library(lmerTest)

#wrap code in a function to get the intraspecific variation for each species

#testing species
mySpecies <- "larus_argentatus"

#plot
ggplot(samplingLists, aes(x = viirs, y = larus_argentatus))+
  geom_point(alpha=0.5)+
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))+
  facet_wrap(~point_id) +
  ylab("Occupancy probability")



fitRandomModels <- function(mySpecies){
  
#list of points where species is present
speciesPoints <- samplingLists %>%
                  select(point_id, all_of(mySpecies)) %>%
                  filter(.[mySpecies]==1) %>%
                  pull(point_id) %>%
                  unique()

#make sure species is at least present in the point - we need it to be present in at least 5 to fit a random effect
if(length(speciesPoints)>5) {
  
  #fit model for that species
  modelFormula <- paste(mySpecies, "viirs + (1|locality_id) + (viirs|point_id)", sep = '~')
  
  glmer1 <-  glmer(as.formula(modelFormula), family = binomial, 
                 data = subset(samplingLists, point_id %in% speciesPoints))
  
  #extract the sd of the viirs effects across localities
  re_dat = as.data.frame(VarCorr(glmer1))
  effectSD = re_dat$sdcor[re_dat$var1=="viirs"]
  
  #extract the species slopes in each point if
  df <- data.frame(Species = mySpecies,
                   point_id = row.names(coef(glmer1)$point_id),
                   urbanEffect = coef(glmer1)$point_id$viirs,
                   randomSD = effectSD)
  
} else {
  
  #if insufficient data
  df= data.frame(Species = mySpecies,
                  point_id = NA,
                  urbanEffect = NA,
                  randomSD = NA)
}

return(df)

}

# fit to all species
commonSpecies <- example_data %>%
                  janitor::clean_names() %>%
                  group_by(scientific_name) %>%
                  summarise(nuPoints = length(unique(point_id)),
                            nuLocals = length(unique(locality_id))) %>%
                  ungroup() %>%
                  filter(nuLocals >= 00,
                         nuPoints >= 5) %>%
                  mutate(scientific_name = tolower(gsub(" ","_", scientific_name)))%>%
                  pull(scientific_name) 

speciesOutputs <- commonSpecies[1:5] %>%
                    map_dfr(fitRandomModels)



#same as above except with Fixed effects
fitFixedModels <- function(mySpecies){
  
  #list of points where species is present
  speciesPoints <- samplingLists %>%
    select(point_id, all_of(mySpecies)) %>%
    filter(.[mySpecies]==1) %>%
    pull(point_id) %>%
    unique()
  
  #make sure species is at least present in the point - we need it to be present in at least 5 to fit a random effect
  if(length(speciesPoints)>5) {
    
    #fit model for that species
    samplingLists$point_id <- factor(samplingLists$point_id)
    modelFormula <- paste(mySpecies, "viirs + point_id:viirs + (1|locality_id)", sep = '~')
    
    glmer1 <-  glmer(as.formula(modelFormula), family = binomial, 
                     data = subset(samplingLists, point_id %in% speciesPoints))
    
    #extract the species slopes in each point if
    df <- data.frame(Species = mySpecies,
                     point_id = names(fixef(glmer1))[-c(1:2)],
                     urbanEffect = as.numeric(fixef(glmer1))[-c(1:2)]) %>%
          mutate(point_id = gsub("viirs:point_id","",point_id))
    
  } else {
    
    #if insufficient data
    df= data.frame(Species = mySpecies,
                   point_id = NA,
                   urbanEffect = NA)
  }
  
  return(df)
  
}

# try again to same set of common species
speciesOutputs <- commonSpecies[1:5] %>%
  map_dfr(fitFixedModels)


######### Corey's simpler attempt
# write a function for buffer
model_species_in_buffer <- function(buffer_id){
  
  buffer_dat <- example_data %>%
    dplyr::filter(point_ID==buffer_id)
  
  # get species list
  species_number <- buffer_dat %>%
    group_by(COMMON_NAME) %>%
    summarize(N=n()) %>%
    dplyr::filter(N>=100)
  
  # now for every species
  # get data and fit a model
  species_mod_function <- function(species){
    
    message(paste0("Modelling buffer ", buffer_id, " ", species))
    
    species_dat <- buffer_dat %>%
      dplyr::filter(COMMON_NAME==species)
    
    list_dat <- buffer_dat %>%
      dplyr::filter(! SAMPLING_EVENT_IDENTIFIER %in% unique(species_dat$SAMPLING_EVENT_IDENTIFIER)) %>%
      dplyr::select(1,2,6:19) %>%
      distinct()
    
    length(unique(list_dat$SAMPLING_EVENT_IDENTIFIER))==nrow(list_dat)
    
    model_dat <- species_dat %>%
      bind_rows(list_dat) %>%
      replace_na(list(COMMON_NAME=species,
                      OBSERVATION_COUNT=0,
                      point_ID=buffer_id)) %>%
      mutate(yday=lubridate::yday(OBSERVATION_DATE))
    
    nrow(model_dat)==nrow(list_dat)+nrow(species_dat)
    
    # fit a model
    mod <- mgcv::gam(OBSERVATION_COUNT ~ log10(viirs) + s(yday, k=3) + s(LONGITUDE, LATITUDE) + s(DURATION_MINUTES, k=3), 
                     data=model_dat,
                     family="nb")
    
    summary(mod)
    
    summary_df <- data.frame(COMMON_NAME=species,
                             urbanEffect=as.numeric(mod$coefficients[2]),
                             point_ID=buffer_id)
    
    return(summary_df)
    
  }
  
  species_results <- bind_rows(lapply(species_number$COMMON_NAME, species_mod_function))
  
  return(species_results)
  
}


buffer_results <- bind_rows(lapply(unique(example_data$point_ID), model_species_in_buffer))
buffer_results <- readRDS("Data/buffer_results_gam_example.RDS")

example_data


# get the data for the other method
# summarize data function
files <- list.files("random_polygon_results/500_km")

aggregate_data_500 <- function(file_name){
  
  dat <- readRDS(paste0("random_polygon_results/500_km/", file_name)) %>%
    mutate(sample=gsub("random_sample_", "", file_name)) %>%
    mutate(sample=gsub(".RDS", "", sample))
  
  return(dat)
  
}

data_500_km_all <- bind_rows(lapply(files, aggregate_data_500))

data_500_example_buffers <- data_500_km_all %>%
  dplyr::filter(point_ID %in% buffer_results$point_ID)

# make a comparison
buffer_results %>%
  left_join(., data_500_example_buffers) %>%
  ggplot(., aes(x=urbanEffect, y=UT_median, group=point_ID, color=as.factor(point_ID)))+
  #geom_point()+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Modelled urban effect using presence/absence")+
  ylab("Urban tolerance using our method")+
  theme(legend.title=element_blank())







# or geographic regession?? not used, ignore this section

# https://www.bristol.ac.uk/media-library/sites/cmpo/migrated/documents/gwr.pdf
#https://rpubs.com/quarcs-lab/tutorial-gwr1

library(spgwr)

#add on coords (or lets use onservation counts instead of binary, see below)
samplingLists <- example_data %>%
  select(SAMPLING_EVENT_IDENTIFIER, viirs,ghm,
         SCIENTIFIC_NAME, LOCALITY_ID,point_ID,
         OBSERVATION_COUNT,LATITUDE,LONGITUDE) %>%
  pivot_wider(names_from=SCIENTIFIC_NAME, 
              values_from=OBSERVATION_COUNT,
              values_fill = 0) %>%
  janitor::clean_names() %>%
  arrange(sampling_event_identifier) 

# has to be sp...
library(sp)
coordinates(samplingLists) <- c("longitude","latitude")
proj4string(samplingLists) <- CRS("+proj=longlat")

#not sure whether glm poss, so start with counts and log it (using lm)

bw = gwr.sel(log(larus_argentatus+1) ~ viirs, data = samplingLists, adapt=T)
gwr.model = gwr(log(larus_argentatus+1) ~ viirs, data = samplingLists, adapt=bw)
gwr.model
results <-as.data.frame(gwr.model$SDF)
names(results)

ggplot(results) +
  geom_point(aes(x=longitude,
                 y=latitude,
                 color=viirs))

