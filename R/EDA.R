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

species_sd %>%
  dplyr::select(COMMON_NAME, N, sd_urban, mean_urban) %>%
  readr::write_csv(., "submissions/Journal of Animal Ecology/Table_S1.csv")

# make a 'label' so we can label the six example species on this plot
ex_sp_list <- c("Mourning Dove", "Vaux's Swift",
                "Northern Parula", "Peregrine Falcon",
                "Northern Bobwhite", "Mallard")

species_sd <- species_sd %>%
  mutate(label=ifelse(COMMON_NAME %in% ex_sp_list, COMMON_NAME, NA))

ggplot(species_sd, aes(x=N))+
  geom_histogram(color="black", fill="gray70")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of buffers occupied")+
  ylab("Number of species")

ggsave("Figures/sample_size_per_species.png", width=6.1, height=5.8, units="in")

median(species_sd$N)
sd(species_sd$N)
mean(species_sd$N)

min(species_sd$sd_urban)
max(species_sd$sd_urban)
median(species_sd$sd_urban)
sd(species_sd$sd_urban)
mean(species_sd$sd_urban)


# number of species with mean 'negative' response to urbanization
species_sd %>%
  dplyr::filter(mean_urban<=0) %>%
  nrow(.)/nrow(species_sd)

species_sd %>%
  dplyr::filter(mean_urban>=0) %>%
  nrow(.)/nrow(species_sd)

# make a boxplot showing the
# mean urban tolerance scores (so the mean of all urban tolerance scores)
# versus the sd of urban tolerance scores (the SD of all urban tolerance scores)
# but because the mean can be negative
# scale both values from 0-1
inter_vs_intra <- species_sd %>%
  mutate(Intraspecific=scales::rescale(sd_urban)) %>%
  mutate(Interspecific=scales::rescale(mean_urban)) %>%
  dplyr::select(COMMON_NAME, Intraspecific, Interspecific) %>%
  pivot_longer(!COMMON_NAME, names_to="type", values_to="value") %>%
  ggplot(.) +
  geom_boxplot(aes(y=type, x=value, fill=type))+
  theme_bw()+
  theme(axis.text=element_text(color='black'))+
  scale_fill_brewer(palette="Dark2")+
  xlab("Standardized urban tolerance metric")+
  ylab("")+
  guides(fill=FALSE)+ 
  ggtitle("(B)")+
  theme(panel.grid=element_blank())

inter_vs_intra

ggsave("Figures/inter_vs_intra_boxplot.png", width=6.1, height=5.8, units="in")

# fit a model for this figure
species_sd %>%
  mutate(Intraspecific=scales::rescale(sd_urban)) %>%
  mutate(Interspecific=scales::rescale(mean_urban)) %>%
  dplyr::select(COMMON_NAME, Intraspecific, Interspecific) %>%
  pivot_longer(!COMMON_NAME) %>%
  lm(value ~ name, data=.) %>%
  summary()

# test for whether at the species level
# 'inter' is greater than 'intra'
species_sd %>%
  mutate(Intraspecific=scales::rescale(sd_urban)) %>%
  mutate(Interspecific=scales::rescale(mean_urban)) %>%
  dplyr::select(COMMON_NAME, Intraspecific, Interspecific) %>%
  sample_n(50) %>%
  pivot_longer(!COMMON_NAME, names_to="type", values_to="value") %>%
  arrange(type, value) %>%
  ggplot(.)+
  geom_point(aes(x=fct_inorder(COMMON_NAME), y=value, color=type))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  xlab("")+
  ylab("Standardized value")+
  scale_color_brewer(palette="Dark2", name="")+
  theme(legend.position="bottom")

ggsave("Figures/example_species_level_inter_vs_intra_values.png", width=6.2, height=7.4, units="in")

# but summarize this visualization textually for the paper
inter_vs_intra_difference <- species_sd %>%
  mutate(Intraspecific=scales::rescale(sd_urban)) %>%
  mutate(Interspecific=scales::rescale(mean_urban)) %>%
  dplyr::select(COMMON_NAME, Intraspecific, Interspecific) %>%
  mutate(difference=Intraspecific-Interspecific)

# percent of species that intra is less than inter?
inter_vs_intra_difference %>%
  dplyr::filter(difference<0) %>%
  nrow(.)/nrow(inter_vs_intra_difference)

# do species that are, on average across all buffers, selecting towards urban habitat
# versus those that are selecting against it have more variability in their urban scores among
# regions?
positive_vs_negative <- species_sd %>%
  mutate(association=ifelse(mean_urban>0, "Mean positive", "Mean negative")) %>%
  ggplot(., aes(x=association, y=sd_urban, fill=association))+
  geom_boxplot()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_fill_brewer(palette="Dark2")+
  ylab("Standard deviation of urban tolerance")+
  xlab("")+
  guides(fill=FALSE)+ 
  ggtitle("(C)")+
  theme(panel.grid=element_blank())

positive_vs_negative

ggsave("Figures/positive_vs_negative_species.png", width=6.1, height=5.8, units="in")

# make a scatterplot
inter_vs_intra_scatter <- species_sd %>%
  ggplot(., aes(x=mean_urban, y=sd_urban))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Standard deviation of urban tolerance")+
  xlab("Mean urban tolerance")+
  geom_smooth(method="lm")+ 
  geom_vline(xintercept=0, color="red", linetype="dashed")+
  ggrepel::geom_label_repel(aes(label=label))+
  ggtitle("(A)")+
  theme(panel.grid=element_blank())

inter_vs_intra_scatter

hist(species_sd$mean_urban)
hist(species_sd$sd_urban)
mod <- lm(mean_urban ~ sd_urban, data=species_sd)
summary(mod)


# also calculate the median versus the interquartile range
data_500_km_all %>%
  group_by(COMMON_NAME) %>%
  summarize(N=n(),
            median=median(UT_median),
            iqr=IQR(UT_median)) %>%
  ggplot(., aes(x=median, y=iqr))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Interquartile range")+
  xlab("Median urban tolerance")+
  geom_smooth(method="lm")+ 
  geom_vline(xintercept=0, color="red", linetype="dashed")

# make a plot where things are ranked by species
# not loving it, but will save in here for now
inter_vs_intra_species <- species_sd %>%
  arrange(mean_urban) %>%
  ggplot(., aes(x=fct_inorder(COMMON_NAME), y=mean_urban))+
  geom_point()+
  geom_errorbar((aes(ymax=mean_urban+sd_urban, ymin=mean_urban-sd_urban)))

inter_vs_intra_species

# put some figures together for a final figure
# potentially for the paper
inter_vs_intra_scatter  / (inter_vs_intra  | positive_vs_negative) + plot_layout(nrow=2)

ggsave("Figures/all_species_summary.png", width=7.3, height=6.8, units="in")

# plot sd as a function of N (number of buffers)
ggplot(species_sd, aes(x=N, y=sd_urban))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Number of buffers")+
  ylab("Species-specific SD of urban scores")+
  geom_smooth(method="lm")

ggsave("Figures/number_buffers_vs_sd_species_ut.png", width=7.6, height=6.1, units="in")

mod <- lm(log10(sd_urban) ~ log10(N), data=species_sd)
summary(mod)



# now look at the sd of the buffers
# a species is found in
# does that correlate with the sd of the species
ggplot(species_sd, aes(y=sd_urban, x=sd_total))+
  geom_point(aes(size=N), alpha=0.4)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  ylab("Standard deviation of urban tolerance")+
  xlab("Buffer level SD")+
  geom_smooth(method="lm", show.legend = FALSE)+
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")+
  guides(size=guide_legend(title="Number of buffers"))+
  ggrepel::geom_label_repel(aes(label=label))+
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank())

ggsave("Figures/sd_buffers_vs_sd_species_ut.png", width=7.6, height=6.1, units="in")

# fit a simple linear model that puts these together
# tests for whether across species, there is a significant difference among
mod <- lm(log10(sd_urban) ~ log10(sd_total) + log10(N), data=species_sd)
summary(mod)

resids <- species_sd %>%
  mutate(residual=resid(mod))

pal <- wesanderson::wes_palette("Zissou1", 20, type = "continuous")

# repeat this figure but color the points based on mean
# urban tolerance of a species
ggplot(species_sd, aes(y=sd_urban, x=sd_total))+
  geom_point(aes(size=N, color=mean_urban))+
  scale_color_gradientn(colours=pal, name="Urban tolerance: ")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  ylab("Urban tolerance SD")+
  xlab("Buffer level SD")+
  geom_smooth(method="lm", show.legend = FALSE)+
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")+
  guides(size=guide_legend(title="Number of buffers"))+
  ggrepel::geom_label_repel(aes(label=label))+
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank()) 

ggsave("Figures/sd_buffers_vs_sd_species_ut_with_points_colored.png", width=7.6, height=6.1, units="in")

# repeat this figure, but only for the species that are more 'common'
ggplot(species_sd %>%
         dplyr::filter(N>1000), aes(y=sd_urban, x=sd_total))+
  geom_point(aes(size=N))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  ylab("Urban tolerance SD")+
  xlab("Buffer level SD")+
  geom_smooth(method="lm", show.legend = FALSE)+
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")+
  guides(size=guide_legend(title="Number of buffers"))+
  ggrepel::geom_label_repel(aes(label=label))+
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank())

# test whether urban unadjusted looks like
ggplot(species_sd, aes(y=sd_urban_unadjusted, x=sd_total))+
  geom_point(aes(size=N))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  ylab("Urban tolerance SD (unadjusted)")+
  xlab("Buffer level SD")+
  geom_smooth(method="lm", show.legend = FALSE)+
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")+
  guides(size=guide_legend(title="Number of buffers"))+
  ggrepel::geom_label_repel(aes(label=label))+
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank())

ggplot(species_sd, aes(y=sd_urban_unadjusted, x=sd_urban))+
  geom_point(aes(size=N))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  ylab("Urban tolerance SD (unadjusted)")+
  xlab("Urban tolerance SD (adjusted)")+
  geom_smooth(method="lm", show.legend = FALSE)+
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")+
  guides(size=guide_legend(title="Number of buffers"))+
  ggrepel::geom_label_repel(aes(label=label))+
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank())

# plot CV (for unadjusted urban tolerance)
ggplot(species_sd, aes(x=CV_sp, y=CV_buffer))+
  geom_point(aes(size=N))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  ylab("Urban tolerance CV")+
  xlab("Buffer level CV")+
  geom_smooth(method="lm", show.legend = FALSE)+
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")+
  guides(size=guide_legend(title="Number of buffers"))+
  ggrepel::geom_label_repel(aes(label=label))+
  theme(legend.position="bottom")+
  theme(panel.grid=element_blank())


# test how the number of buffers standard deviation correlates
# with sample size
buffer_urban <- data_500_km_all %>%
  ungroup() %>%
  dplyr::select(total_median_viirs, sample) %>%
  distinct()

random_sample_buffers <- function(sample_size){
  
  samp <- buffer_urban %>%
    sample_n(sample_size) %>%
    summarize(sd=sd(total_median_viirs)) %>%
    mutate(number_buffers=sample_size)
  
  return(samp)
}

# apply this function on a sequence from 10, 2000
sampled_sd <- bind_rows(lapply(seq(10, 2000, by=10), random_sample_buffers))

ggplot(sampled_sd, aes(x=number_buffers, y=sd))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Number of randomly sampled buffers")+
  ylab("Standard deviation")


# Now lets calculate the 'distance' from each point (i.e., species)
# to the 1:1 line
# as a measure of 'deviation' from what it should be
# get the distance from the line for each point
# function to calculate distance
dist2d <- function(species) {
  dat <- species_sd %>%
    dplyr::filter(COMMON_NAME==species)
  a <- c(dat$sd_total, dat$sd_urban)
  b = c(0, 0)
  c = c(1, 1)
  v1 <- b - c
  v2 <- a - b
  m1 <- cbind(v1,v2)
  d <- data.frame(distance=abs(det(m1))/sqrt(sum(v1*v1)),
                  COMMON_NAME=species)
  return(d)
} 

distance_from_line <- bind_rows(lapply(unique(species_sd$COMMON_NAME), dist2d))

distance_from_line %>%
  dplyr::filter(distance<=1) %>%
  nrow()/nrow(distance_from_line)

# let's remake this figure again but try to pretty it up
datPoly_above <- buildPoly(range(species_sd$sd_total), range(species_sd$sd_urban),
                           slope=1, intercept=0, above=TRUE)

datPoly_above <- data.frame(x=c(-Inf, Inf, Inf), 
                            y=c(0.1, 10, 9.8))

datPoly_below <- buildPoly(range(species_sd$sd_total), range(species_sd$sd_total),
                           slope=1, intercept=0, above=FALSE)
# now look at the sd of the buffers
# a species is found in
# does that correlate with the sd of the species
# ideally we can vizualize this in log-log relationship, but it doesn't seem to work well!
ggplot(species_sd, aes(y=sd_urban, x=sd_total))+
  scale_x_log10()+
  scale_y_log10()+
  #geom_polygon(data=datPoly_above, aes(x=x, y=y), alpha=0.2, fill="blue")+
  #geom_polygon(data=datPoly_below, aes(x=x, y=y), alpha=0.2, fill="red")+
  geom_point(aes(size=N))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ylab("Urban tolerance SD")+
  xlab("Buffer level SD")+
  ggrepel::geom_label_repel(aes(label=label))+
  geom_smooth(method="lm", show.legend = FALSE)+
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed")+
  guides(size=guide_legend(title="Number of buffers"))+
  theme(legend.position="bottom")

#ggsave("Figures/sd_buffers_vs_sd_species_ut.png", width=7.6, height=6.1, units="in")





############################################
######### Now look at some spatial stuff
######### Make some example plots of example species
##################################################

# read in points
# read in data necessary to do the sampling for example species
potential_points <- readRDS("Data/potential_sample_points.RDS") %>%
  dplyr::filter(ID<=2000)

# make a map of the buffers sampled and their median urban level
all_buffer_dat <- potential_points %>%
  mutate(sample=as.character(as.integer(ID))) %>%
  left_join(., data_500_km_all %>%
              ungroup() %>%
              dplyr::select(sample, total_median_viirs) %>%
              distinct()) 

ggplot()+
  geom_sf(data=all_buffer_dat, aes(color=total_median_viirs))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_color_viridis_c()

# plot the map of Urban tolerance
# for a species
# UT already adjusted for available urban habitat
plot_species_map <- function(species_name){
  
  dat <- potential_points  %>%
    mutate(sample=as.character(as.integer(ID))) %>%
    left_join(., data_500_km_all %>%
                ungroup() %>%
                dplyr::filter(COMMON_NAME==species_name) %>%
                dplyr::select(sample, UT_median) %>%
                distinct())
  
  ggplot()+
    geom_sf(data=dat, aes(color=UT_median))+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    scale_color_viridis_c()
  
}


# How does the UT score change as a function
# of the total urbanness of the randomly sampled buffer?
# look at this for some example species
# do this for four species for now
ex_sp_list <- c("Canada Warbler", "Peregrine Falcon",
                "Northern Cardinal", "Roseate Spoonbill")

ex_sp_list <- c("Mourning Dove", "Vaux's Swift",
                "Northern Parula", "Peregrine Falcon",
                "Northern Bobwhite", "Mallard")

map_dat <- potential_points %>%
  rename(sample=ID) %>%
  mutate(sample=as.character(as.integer(sample))) %>%
  left_join(., data_500_km_all %>%
              ungroup() %>%
              dplyr::filter(COMMON_NAME %in% ex_sp_list)) %>%
  ungroup() %>%
  mutate(across(COMMON_NAME, factor, levels=c("Peregrine Falcon", "Vaux's Swift",
                                                   "Mallard", "Mourning Dove",
                                                   "Northern Bobwhite", "Northern Parula")))

# read in map of US
us <- ne_countries(scale="medium", returnclass="sf", country = "United States of America")

pal <- wesanderson::wes_palette("Zissou1", 20, type = "continuous")

ggplot()+
  geom_sf(data=us, fill="gray25", color="black")+
  geom_sf(data=map_dat, aes(color=UT_median), size=0.5)+
  xlim(130, 10)+
  ylim(20, 50)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  theme(panel.grid=element_blank())+
  facet_wrap(~COMMON_NAME, ncol=2)+
  scale_color_gradientn(colours=pal, name="Urban tolerance: ")+
  theme(legend.position="bottom")

ggsave("Figures/map_example_sp_figure.png", width=8.6, height=8.5, units="in")

# function to make a tiny plot
# for each species
# but with a fitted gam model onto it
species_scatter_function_plot <- function(species_name){
  
  tmp <- data_500_km_all %>%
    dplyr::filter(COMMON_NAME==species_name)
  
  
  mod <- lm(UT_median ~ total_median_viirs, data=tmp)
  mod2 <- mgcv::gam(UT_median ~ total_median_viirs + s(lng, lat, k=4), data=tmp)
  
  summary <- broom::tidy(mod2, parametric=TRUE) %>%
    mutate(N=nrow(tmp)) %>%
    mutate(prop_negative=sum(tmp$UT_median<0)/nrow(tmp)) %>%
    mutate(COMMON_NAME=species_name) %>%
    mutate(upr_95=confint.gam(mod2)$`97.5%`) %>%
    mutate(lwr_95=confint.gam(mod2)$`2.5%`)
  
  plot <- ggplot(tmp, aes(x=total_median_viirs, y=UT_median))+
    geom_point(size=0.5, color="transparent")+
    theme_bw()+
    theme(axis.text=element_text(color="black"))+
    geom_abline(intercept=summary$estimate[1], slope=summary$estimate[2], 
                linetype="solid", color="blue", size=1)+
    geom_abline(intercept=summary$upr_95[1], slope=summary$upr_95[2], 
                linetype="dashed", color="blue", size=0.7)+
    geom_abline(intercept=summary$lwr_95[1], slope=summary$lwr_95[2], 
                linetype="dashed", color="blue", size=0.7)+
    xlab("Median VIIRS of sampled checklists")+
    ylab("Urban tolerance")+
    theme(axis.text=element_text(size=4))+
    theme(axis.title=element_text(size=6))+
    theme(panel.grid=element_blank())
  
  plot
  
  ggsave(paste0("Figures/", species_name, "_example_scatter.png"), height=1.3, width=2, units="in")
  
  return(plot)
  
}

pefa <- species_scatter_function_plot("Peregrine Falcon")
modo <- species_scatter_function_plot("Mourning Dove")
nopa <- species_scatter_function_plot("Northern Parula")
vasw <- species_scatter_function_plot("Vaux's Swift")
nobo <- species_scatter_function_plot("Northern Bobwhite")
mall <- species_scatter_function_plot("Mallard")

mall + pefa + modo + nobo + nopa + vasw + plot_layout(ncol=2)

data_500_km_all %>%
  ungroup() %>%
  dplyr::filter(COMMON_NAME %in% ex_sp_list) %>%
  ggplot(., aes(y=UT_median, x=total_median_viirs))+
  geom_point()+
  facet_wrap(~COMMON_NAME, scales="free")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

data_500_km_all %>%
  ungroup() %>%
  dplyr::filter(COMMON_NAME %in% ex_sp_list) %>%
  ggplot(., aes(y=UT_median, x=total_median_viirs))+
  geom_point()+
  facet_wrap(~COMMON_NAME)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")



# run a simple lm and get the parameter estimate
# between UT median and the total median VIIRS for the buffers that a species
# occurs in
species_model_function <- function(species_name){
  
  tmp <- data_500_km_all %>%
    dplyr::filter(COMMON_NAME==species_name)
  
  
  mod <- lm(UT_median ~ total_median_viirs, data=tmp)
  mod2 <- mgcv::gam(UT_median ~ total_median_viirs + s(lng, lat, k=4), data=tmp)
  
  summary <- broom::tidy(mod2, parametric=TRUE) %>%
    mutate(N=nrow(tmp)) %>%
    mutate(prop_negative=sum(tmp$UT_median<0)/nrow(tmp)) %>%
    mutate(COMMON_NAME=species_name)
   
  return(summary)
  
}

species_results <- bind_rows(lapply(species_sd %>%
                                      dplyr::filter(N>10) %>%
                                      .$COMMON_NAME, species_model_function))

species_results %>%
  dplyr::filter(term=="total_median_viirs") %>%
  dplyr::filter(estimate<=0) %>%
  nrow()/length(unique(species_results$COMMON_NAME))

species_results %>%
  dplyr::filter(term=="total_median_viirs") %>%
  ggplot(., aes(y=estimate, x=prop_negative))+
  geom_point()+
  geom_smooth(method="lm")

species_results %>%
  left_join(., distance_from_line, by="COMMON_NAME") %>%
  dplyr::filter(term=="total_median_viirs") %>%
  ggplot(., aes(y=estimate, x=distance))+
  geom_point()+
  geom_smooth(method="lm")

#######################################################
#######################################################
################ one additional test for inter-specific vs intraspecific
################ variability
# the relationship between mean urban tolerance (interspecific measure) and
# randomly sampled urban tolerance at the species level many times
# make a function to do this
correlation_between_mean_and_random_ut <- function(id_number){
  
  tmp <- data_500_km_all %>%
    group_by(COMMON_NAME) %>%
    sample_n(1) %>%
    dplyr::select(COMMON_NAME, UT_median) %>%
    left_join(., species_sd %>%
                dplyr::select(COMMON_NAME, mean_urban))
  
  ggplot(tmp, aes(x=mean_urban, y=UT_median))+
    geom_point()+
    theme_bw()+
    geom_smooth(method="lm")
  
  summary_df <- broom::glance(lm(UT_median ~ mean_urban, data=tmp)) %>%
    mutate(draw=id_number)
  
  return(summary_df)
  
}

inter_vs_intra_resampling <- bind_rows(lapply(c(1:1000), correlation_between_mean_and_random_ut))

ggplot(inter_vs_intra_resampling, aes(x=r.squared))+
  geom_histogram(fill="gray80", color="black")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("R squared")+
  ylab("Number of random samples")

mean(inter_vs_intra_resampling$r.squared)

ggsave("Figures/randomly_sampling_correlations.png", width=8.6, height=8.5, units="in")

######################################################
######################################################
############### Run phylo model of main model
################# as a test
#########################################
##########################################
#### Now start phylo stuff ###############
library(phylolm)
library(phylosignal)
library(phylobase)
library(phytools)


# first join the tiplabel with the
# dataset
dat1 <- species_sd %>%
  left_join(., read_csv("Data/clements_clean.csv") %>%
              rename(COMMON_NAME=ebird_COMMON_NAME)) %>%
  dplyr::filter(complete.cases(TipLabel)) %>%
  group_by(COMMON_NAME) %>%
  slice(1) %>%
  mutate(TipLabel2=TipLabel) %>%
  column_to_rownames(var="TipLabel")

length(unique(dat1$COMMON_NAME))==length(unique(dat1$TipLabel2))

# function to read one tree in
read_one_tree<-function(path, x=1){
  
  one_bird_tree <- ape::read.tree(file = "phylo/phy.tre")[[x]]
  
  return(one_bird_tree)
}

bird_tree <- read_one_tree()


# function to read all trees in
read_all_trees<-function(path){
  
  ape::read.tree(file = "phylo/phy.tre")
  
}

all_trees <- read_all_trees()

# a function to subset the tree to the tips of the 245 species
# described above
subset_tree <- function(bird_tree, dataset) {
  
  non_usa_sp <- bird_tree$tip.label[!bird_tree$tip.label %in% dataset$TipLabel]
  
  usa_bird_tree <- drop.tip(bird_tree, non_usa_sp)
  
  return(usa_bird_tree)
}

usa_tree <- subset_tree(bird_tree, dat1)

# need to get a consensus tree to run the phylogenetic analyses on
# first subset all trees to the 245 species
non_usa_sp <- bird_tree$tip.label[!bird_tree$tip.label %in% dat1$TipLabel]

subset_trees <- lapply(all_trees, drop.tip, tip=non_usa_sp)

con_tree <- consensus.edges(subset_trees, consensus.tree=consensus(subset_trees, p=0.5, check.labels=TRUE))

mod <- lm(log10(sd_urban) ~ log10(sd_total) + log10(N), data=species_sd)
summary(mod)

phymod <- phylolm(log10(sd_urban) ~ log10(sd_total) + log10(N), 
                  data=dat1, phy=con_tree, na.action="na.fail")
summary(phymod)

#########################################################
#########################################################
# get an example distribution of scores
# for 4 species
data_500_km_all %>%
  dplyr::filter(COMMON_NAME %in% c("Mourning Dove", "Song Sparrow",
                "Northern Bobwhite", "Mallard")) %>%
  ggplot(., aes(x=UT_median))+
  geom_histogram(fill="gray80", color="black")+
  theme_minimal()+
  theme(axis.text.x=element_blank())+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(color='black'))+
  xlab("Urban tolerance scores")+
  theme(panel.grid=element_blank())+
  facet_wrap(~COMMON_NAME, scales="free")+
  theme(strip.background = element_blank(),
    strip.text.x = element_blank())+
  coord_flip()

ggsave("Figures/example_distribution_figures.png", height=3, width=4.2, units="in")

#########################
#########################
######## DOES TRAITS MATTER?
########## look at two different model structures - 1 using the mean interspecific value
########### and the other allowing for the variability in the
traits <- readRDS("Data/predictor_variables.RDS")

# mean interspecific model
mean_inter_mod_dat <- species_sd %>%
  left_join(., traits) %>%
  dplyr::select(COMMON_NAME, mean_urban, N, adult_body_mass_g, brain_residual, 
                habitat_generalism_scaled, migration_status, clutch_size) %>%
  dplyr::filter(complete.cases(.))


hist(mean_inter_mod_dat$mean_urban)

inter_mod <- lm(mean_urban ~ habitat_generalism_scaled + log10(adult_body_mass_g) +
                  brain_residual + clutch_size + migration_status, data=mean_inter_mod_dat)

inter_mod.s <- arm::standardize(inter_mod)

broom::tidy(inter_mod.s, conf.int=TRUE)

intra_mod_dat <- data_500_km_all %>%
  left_join(., traits, by="COMMON_NAME") %>%
  ungroup() %>%
  dplyr::select(COMMON_NAME, UT_mean, sample, adult_body_mass_g, brain_residual, 
                habitat_generalism_scaled, migration_status, clutch_size) %>%
  dplyr::filter(complete.cases(.))

length(unique(intra_mod_dat$COMMON_NAME))

intra_mod <- lme4::lmer(UT_mean ~ habitat_generalism_scaled + log10(adult_body_mass_g) +
                    brain_residual + clutch_size + migration_status + (1|sample), data=intra_mod_dat)

intra_mod.s <- arm::standardize(intra_mod)

broom.mixed::tidy(intra_mod.s,, conf.int=TRUE)

summary(inter_mod.s)
summary(intra_mod.s)

# results of both models/approaches together
comparison <- broom::tidy(inter_mod.s, conf.int=TRUE) %>%
  mutate(intercept=as.numeric(broom::tidy(inter_mod.s, conf.int=TRUE)[1,2])) %>%
  rename(slope=estimate) %>%
  mutate(Analysis="Mean urban tolerance") %>%
  slice(2:6) %>%
  bind_rows(broom.mixed::tidy(intra_mod.s,, conf.int=TRUE) %>%
              dplyr::select(3:8) %>%
              slice(2:6) %>%
              rename(slope=estimate) %>%
              mutate(intercept=as.numeric(broom.mixed::tidy(intra_mod.s,, conf.int=TRUE)[1,4])) %>%
              mutate(Analysis="Varying urban tolerance"))

a <- ggplot(comparison, aes(x=Analysis, y=slope, color=term, group=term))+
  geom_point()+
  geom_line()+
  #coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_hline(yintercept=0, color="black", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("(A)")

ggplot(comparison, aes(x=term, y=slope, color=Analysis, group=Analysis))+
  geom_point()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_hline(yintercept=0, color="black", linetype="dashed")+
  scale_color_brewer(palette="Dark2")+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))

b <- comparison %>%
  dplyr::select(term, slope, Analysis) %>%
  tidyr::pivot_wider(names_from=Analysis, values_from=slope) %>%
  ggplot(., aes(x=`Mean urban tolerance`, y=`Varying urban tolerance`))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")+
  geom_hline(yintercept=0, linetype="dashed", color="red")+
  geom_vline(xintercept=0, linetype="dashed", color="red")+
  ggtitle("(B)")

a + b + plot_layout(ncol=1)

ggsave("Figures/lm_vs_lmm_approach.png", width=6.2, height=8.3, units="in")

########################
########################
######### LOOK at residuals vs some
### traits
trait_data <- resids %>%
  left_join(., readRDS("Data/predictor_variables.RDS")) %>%
  mutate(intra_species_variability=residual)

sum(is.na(trait_data$migration_status))
sum(is.na(trait_data$migratory_status))
sum(is.na(trait_data$diet_breadth))
sum(is.na(trait_data$brain_residual))
sum(is.na(trait_data$adult_body_mass_g))
sum(is.na(trait_data$mean_flock_size))
sum(is.na(trait_data$clutch_size))
sum(is.na(trait_data$habitat_generalism_scaled))


ggplot(trait_data, aes(x=migration_status, y=intra_species_variability))+
  geom_violin()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

ggplot(trait_data, aes(x=brain_residual, y=intra_species_variability))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

ggplot(trait_data, aes(x=habitat_generalism_scaled, y=intra_species_variability))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

ggplot(trait_data, aes(x=adult_body_mass_g, y=intra_species_variability))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")+
  scale_x_log10()


trait_model_dat <- trait_data %>%
  dplyr::select(COMMON_NAME, intra_species_variability,
                adult_body_mass_g, brain_residual, habitat_generalism_scaled,
                migration_status, clutch_size) %>%
  dplyr::filter(complete.cases(.))


hist(trait_model_dat$intra_species_variability)
hist(trait_model_dat$habitat_generalism_scaled)
hist(trait_model_dat$adult_body_mass_g)
hist(log10(trait_model_dat$adult_body_mass_g))
hist(trait_model_dat$brain_residual)
hist(trait_model_dat$clutch_size)

big_trait_mod <- lm(intra_species_variability ~ habitat_generalism_scaled + brain_residual +
                       log10(adult_body_mass_g) + migration_status + clutch_size, data=trait_model_dat)

summary(big_trait_mod)

standardized.mod <- arm::standardize(big_trait_mod)
summary(standardized.mod)

trait_plot_dat <- broom::tidy(standardized.mod, conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%
  arrange(estimate)

ggplot(trait_plot_dat, aes(x=fct_inorder(term), y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  geom_hline(yintercept=0, linetype="dashed", color="red")+
  ylab("Parameter estimate")+
  xlab("")

# specific variable single regression model
# brain residual
brain <- trait_data %>%
  dplyr::select(COMMON_NAME, intra_species_variability, brain_residual) %>%
  dplyr::filter(complete.cases(.))

brain_mod <- lm(intra_species_variability ~ brain_residual, data=brain)
summary(brain_mod)
brain_mod.standard <- arm::standardize(brain_mod)
summary(brain_mod.standard)

# habitat generalism
habitat <- trait_data %>%
  dplyr::select(COMMON_NAME, intra_species_variability, habitat_generalism_scaled) %>%
  dplyr::filter(complete.cases(.))

habitat_mod <- lm(intra_species_variability ~ habitat_generalism_scaled, data=habitat)
summary(habitat_mod)
habitat_mod.standard <- arm::standardize(habitat_mod)
summary(habitat_mod.standard)

# body size
body_dat <- trait_data %>%
  dplyr::select(COMMON_NAME, intra_species_variability, adult_body_mass_g) %>%
  dplyr::filter(complete.cases(.)) %>%
  mutate(log10_body=log10(adult_body_mass_g))

body_mod <- lm(intra_species_variability ~ log10(adult_body_mass_g), data=body_dat)
summary(body_mod)
body_mod.standard <- arm::standardize(body_mod)
summary(body_mod.standard)

# clutch size
clutch <- trait_data %>%
  dplyr::select(COMMON_NAME, intra_species_variability, clutch_size) %>%
  dplyr::filter(complete.cases(.))

clutch_mod <- lm(intra_species_variability ~ clutch_size, data=clutch)
summary(clutch_mod)
clutch_mod.standard <- arm::standardize(clutch_mod)
summary(clutch_mod.standard)

# migration
migration <- trait_data %>%
  dplyr::select(COMMON_NAME, intra_species_variability, migration_status) %>%
  dplyr::filter(complete.cases(.))

migration_mod <- lm(intra_species_variability ~ migration_status, data=migration)
summary(migration_mod)
migration_mod.standard <- arm::standardize(migration_mod)
summary(migration_mod.standard)

single_regression_dat <- broom::tidy(migration_mod.standard, conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%
  bind_rows(broom::tidy(clutch_mod.standard, conf.int=TRUE) %>%
              dplyr::filter(term != "(Intercept)")) %>%
  bind_rows(broom::tidy(body_mod.standard, conf.int=TRUE) %>%
              dplyr::filter(term != "(Intercept)")) %>%
  bind_rows(broom::tidy(habitat_mod.standard, conf.int=TRUE) %>%
              dplyr::filter(term != "(Intercept)")) %>%
  bind_rows(broom::tidy(brain_mod.standard, conf.int=TRUE) %>%
              dplyr::filter(term != "(Intercept)")) %>%
  mutate(model_type="Single regression")

trait_plot_dat.2 <- broom::tidy(standardized.mod, conf.int=TRUE) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(model_type="Multiple regression") %>%
  arrange(estimate) %>%
  bind_rows(single_regression_dat)


ggplot(trait_plot_dat.2, aes(x=fct_inorder(term), y=estimate, color=model_type))+
  geom_point()+
  scale_color_brewer(palette="Dark2")+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=0)+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  coord_flip()+
  geom_hline(yintercept=0, linetype="dashed", color="red")+
  ylab("Parameter estimate")+
  xlab("")












trait_data2 <- inter_vs_intra_difference %>%
  left_join(., readRDS("Data/predictor_variables.RDS")) %>%
  mutate(intra_species_variability=difference)


ggplot(trait_data2, aes(x=migration_status, y=intra_species_variability))+
  geom_violin()+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

ggplot(trait_data2, aes(x=brain_residual, y=intra_species_variability))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

ggplot(trait_data2, aes(x=habitat_generalism_scaled, y=intra_species_variability))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")

ggplot(trait_data2, aes(x=adult_body_mass_g, y=intra_species_variability))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  geom_smooth(method="lm")+
  scale_x_log10()






#############################################################
#############################################################

























################################################################
################################################################
############### OLD STUFF that may be useful


# Make a couple of big plots
ggplot(data_500_km_all, aes(x=total_median_viirs, y=UT_median_mean))+
  geom_point()+
  theme_bw()


data_500_km_all %>%
  group_by(sample) %>%
  summarize(number_species=n(),
            total_median_viirs=mean(total_median_viirs),
            positive_species=sum(UT_median>0),
            negative_species=sum(UT_median<0)) %>%
  mutate(proportion_negative=negative_species/number_species) %>%
  ggplot(., aes(x=total_median_viirs, y=proportion_negative))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))




# Pick some species
# and make some ggridges figures where the ridges are high
# medium
# low ranked as urbanization

# read in data necessary to do the sampling for example species
potential_points <- readRDS("Data/potential_sample_points.RDS")

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



species_ex_function <- function(species_name){
  
  summary <- data_500_km_all %>%
    dplyr::filter(COMMON_NAME==species_name)
  
  # now read in data for the species
  # from those samples where it occurs
  get_data_function <- function(sample_number, grain_size=500000){
    
    # filter to point
    point <- potential_points %>%
      dplyr::filter(ID==sample_number)
    
    point2 <- point %>%
      st_transform(crs=4326)
    
    # create a random buffer that is specified by the grain size
    buff <- point %>%
      st_buffer(grain_size)
    
    buff2 <- buff %>%
      st_transform(crs=st_crs(checklists_sf))
    
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
      dplyr::filter(COMMON_NAME==species_name) %>%
      mutate(sample=sample_number)
    
  }
  
  dat <- bind_rows(lapply(unique(summary$sample), get_data_function))
  
  final_sp_dat <- dat %>%
    left_join(., summary %>%
                ungroup() %>%
                dplyr::select(sample, total_median_viirs)) %>%
    mutate(urban_category=case_when(total_median_viirs >= quantile(summary$total_median_viirs, 0.66) ~ "High",
                                    total_median_viirs < quantile(summary$total_median_viirs, 0.66) & total_median_viirs >= quantile(summary$total_median_viirs, 0.33) ~ "Medium",
                                    total_median_viirs < quantile(summary$total_median_viirs, 0.33) ~ "Low"))
  
  return(final_sp_dat)
  
}


# do this for four species for now
ex_sp_list <- c("Canada Warbler", "White-winged Dove",
                "Northern Cardinal", "Roseate Spoonbill")


example_sp_dat <- bind_rows(lapply(ex_sp_list, species_ex_function))

cawa <- example_sp_dat %>%
  dplyr::filter(COMMON_NAME=="Canada Warbler") %>%
  ggplot(., aes(y=factor(urban_category, levels=c("Low", "Medium", "High")), x=viirs))+
  geom_density_ridges()+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urbanization (VIIRS night-time lights)")+
  ylab("Categorical urbanization of buffer")+
  facet_wrap(~COMMON_NAME)

cawa

rosp <- example_sp_dat %>%
  dplyr::filter(COMMON_NAME=="Roseate Spoonbill") %>%
  ggplot(., aes(y=factor(urban_category, levels=c("Low", "Medium", "High")), x=viirs))+
  geom_density_ridges()+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("Urbanization (VIIRS night-time lights)")+
  ylab("")+
  facet_wrap(~COMMON_NAME)

rosp

wwdo <- example_sp_dat %>%
  dplyr::filter(COMMON_NAME=="White-winged Dove") %>%
  ggplot(., aes(y=factor(urban_category, levels=c("Low", "Medium", "High")), x=viirs))+
  geom_density_ridges()+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("")+
  facet_wrap(~COMMON_NAME)

wwdo

noca <- example_sp_dat %>%
  dplyr::filter(COMMON_NAME=="Northern Cardinal") %>%
  ggplot(., aes(y=factor(urban_category, levels=c("Low", "Medium", "High")), x=viirs))+
  geom_density_ridges()+
  scale_x_log10()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Categorical urbanization of buffer")+
  facet_wrap(~COMMON_NAME)

noca



noca + wwdo + cawa + rosp + plot_layout(ncol=2)


#####################################################################




















###############################################################
###############################################################
# OLD STUFF
# summarize data function
aggregate_data_100 <- function(file_name){
  
  dat <- readRDS(paste0("random_polygon_results/100_km/", file_name)) %>%
    mutate(sample=gsub("random_sample_", "", file_name)) %>%
    mutate(sample=gsub(".RDS", "", sample))
  
  return(dat)
  
}

data_100_km_all <- bind_rows(lapply(files, aggregate_data_100))

length(unique(data_100_km_all$sample))

species_per_sample <- data_100_km_all %>%
  group_by(sample) %>%
  summarize(number_species=length(unique(COMMON_NAME)))

species_sd <- data_100_km_all %>%
  mutate(adjusted_viirs=median_viirs-total_median_viirs) %>%
  group_by(COMMON_NAME) %>%
  summarize(N=n(),
            sd_urban=sd(adjusted_viirs),
            mean_urban=mean(adjusted_viirs),
            sd_total=sd(total_median_viirs))

ggplot(species_sd, aes(x=sd_urban, y=sd_total))+
  geom_point()+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Species-specific SD of urban scores")+
  ylab("Buffer SD of urban sampling")+
  geom_smooth(method="lm")


# any samples with at least 10 species
samples_to_test <- species_per_sample %>%
  dplyr::filter(number_species>=10)

# what about pairwise R2 of every sample
# with more than 10 species (10 is arbitrary for now)
pairwise_r2_function <- function(sample_number){
  
  xxx <- data_100_km_all %>%
    dplyr::filter(sample==sample_number) %>%
    mutate(adjusted_viirs=median_viirs-total_median_viirs)
  
  # get R2
  get_r2 <- function(sample_comparison){
    
    zzz <- data_100_km_all %>%
      dplyr::filter(sample==sample_comparison) %>%
      mutate(adjusted_viirs=median_viirs-total_median_viirs) %>%
      dplyr::select(COMMON_NAME, adjusted_viirs) %>%
      left_join(., xxx %>%
                  dplyr::select(COMMON_NAME, adjusted_viirs), by="COMMON_NAME") %>%
      ungroup() %>%
      dplyr::filter(complete.cases(.))
    
    summary_df <- data.frame(r2=summary(lm(adjusted_viirs.x ~ adjusted_viirs.y, data=zzz))$r.squared) %>%
      mutate(number_species=nrow(zzz)) %>%
      mutate(comparison=paste0(sample_number, "_vs_", sample_comparison))
    
  }
  
  r2s <- bind_rows(lapply(samples_to_test %>%
                            dplyr::filter(sample != sample_number) %>%
                            .$sample, get_r2))
  
}

# lots of comparisons!
lapply_with_error <- function(X,FUN,...){    
  lapply(X, function(x, ...) tryCatch(FUN(x, ...),
                                      error=function(e) NULL))
}

r2_comparisons <- bind_rows(lapply_with_error(samples_to_test$sample, pairwise_r2_function))

temp <- r2_comparisons %>%
  dplyr::filter(number_species>=5)

hist(temp$r2)



