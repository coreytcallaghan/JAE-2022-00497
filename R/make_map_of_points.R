# make map of observations

library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(readr)

# read in eBird data and
# join with modification scores
# for each eBird checklist
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

points_sf <- ebird_data %>%
  dplyr::select(LATITUDE, LONGITUDE, SAMPLING_EVENT_IDENTIFIER, ghm) %>%
  distinct() %>%
  st_as_sf(coords=c("LONGITUDE", "LATITUDE"), crs=4326)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

us <- ne_countries(scale="medium", returnclass="sf", country = "United States of America")

ggplot()+
  geom_sf(data=us)+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  geom_sf(data=points_sf, aes(color=ghm), size=0.4)+
  scale_color_viridis_c(option="inferno")+
  xlim(130, 60)+
  ylim(25, 50)+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(color="black"))

ggsave("Figures/map_of_points_in_USA.png", height=4.3, width=5.4, units="in")

# make dark version of figure for presentations
ggplot()+
  geom_sf(data=us)+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  geom_sf(data=points_sf, aes(color=ghm), size=0.4)+
  scale_color_viridis_c(option="inferno")+
  xlim(130, 60)+
  ylim(25, 50)+
  theme(legend.position="bottom")+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave("Figures/talk_figures/map_of_points_in_USA.png", height=7.5, width=13.3, units="in")

bcrs <- st_read("Data/Spatial data/bcr_terrestrial_shape/BCR_Terrestrial_master.shp")

bcr_summary <- ebird_data %>%
  group_by(BCR_CODE) %>%
  summarize(N=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  dplyr::filter(BCR_CODE != 0) %>%
  rename(BCR=BCR_CODE)

bcr_plot_dat <- bcrs %>%
  left_join(., bcr_summary, by="BCR") %>%
  dplyr::filter(COUNTRY=="USA") %>%
  dplyr::filter(!PROVINCE_S %in% c("ALASKA", "HAWAIIAN ISLANDS"))

ggplot()+
  geom_sf(data=bcr_plot_dat, aes(fill=log10(N)))+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  scale_fill_viridis_c(option="inferno")+
  theme(axis.text=element_text(color="black"))

ggsave("Figures/map_of_bcrs_and_sample_size.png", height=4.3, width=5.4, units="in")

ggplot()+
  geom_sf(data=bcr_plot_dat, aes(fill=log10(N)))+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  scale_fill_viridis_c(option="inferno")+
  theme(axis.text=element_text(color="black"))+
  theme(axis.text=element_text(color="white"))+
  theme(panel.background=element_rect(fill="black"))+
  theme(plot.background=element_rect(fill="black"))+
  theme(axis.ticks=element_blank())+
  theme(panel.border=element_blank())+
  theme(legend.background=element_rect(fill="black"))+
  theme(legend.text=element_text(color="white"))+
  theme(legend.title=element_text(color="white"))+
  theme(legend.key = element_blank())+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave("Figures/talk_figures/map_of_bcrs_and_sample_size.png", height=7.5, width=13.3, units="in")


