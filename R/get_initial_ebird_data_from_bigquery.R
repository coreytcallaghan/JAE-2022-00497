# get data from eBird
## packages
library(readr)
library(bigrquery)
library(dbplyr)
library(dplyr)
library(tidyr)
library(lubridate)

# create connection with online database
con <- DBI::dbConnect(bigrquery::bigquery(),
                      dataset= "ebird",
                      project="ebird-database",
                      billing="ebird-database")

# create ebird table
ebird <- tbl(con, 'ebird_qa_may_2020')

## extract data
# with strict filters
## extract data
example_dat <- ebird %>%
  dplyr::filter(OBSERVATION_DATE >= "2015-01-01") %>%
  dplyr::filter(OBSERVATION_DATE <= "2017-12-31") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT,
                LOCALITY_ID, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY, BCR_CODE,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, GROUP_IDENTIFIER,
                COUNTRY, STATE_CODE) %>%
  dplyr::filter(PROTOCOL_TYPE=="Stationary") %>%
  dplyr::filter(NUMBER_OBSERVERS==1) %>%
  dplyr::filter(CATEGORY=="species") %>%
  dplyr::filter(DURATION_MINUTES<=30) %>%
  dplyr::filter(DURATION_MINUTES>=20) %>%
  collect(n=Inf)

# get rid of any checklists that have an X in them
lists_with_x <- example_dat %>%
  dplyr::filter(OBSERVATION_COUNT=="X") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER) %>%
  distinct()

dat2 <- example_dat %>%
  dplyr::filter(!SAMPLING_EVENT_IDENTIFIER %in% lists_with_x$SAMPLING_EVENT_IDENTIFIER)

# # filter data to only one unique checklist per locality ID
# lists_to_keep <- dat2 %>%
#   dplyr::select(SAMPLING_EVENT_IDENTIFIER, LOCALITY_ID) %>%
#   distinct() %>%
#   group_by(LOCALITY_ID) %>%
#   sample_n(1)

# filter data to just the continental United States
dat3 <- dat2 %>%
  dplyr::filter(COUNTRY=="United States") %>%
  dplyr::filter(!STATE_CODE %in% c("US-AK", "US-HI"))

# only keep one checklist (randomly) when there is a group identifier
checklists_to_keep <- dat3 %>%
  dplyr::select(GROUP_IDENTIFIER, SAMPLING_EVENT_IDENTIFIER) %>%
  group_by(GROUP_IDENTIFIER) %>%
  sample_n(1)

dat4 <- dat3 %>%
  dplyr::filter(SAMPLING_EVENT_IDENTIFIER %in% checklists_to_keep$SAMPLING_EVENT_IDENTIFIER) %>%
  bind_rows(dat3 %>%
              dplyr::filter(is.na(GROUP_IDENTIFIER))) %>%
  dplyr::select(-GROUP_IDENTIFIER, -EFFORT_DISTANCE_KM, -EFFORT_AREA_HA, -NUMBER_OBSERVERS, -CATEGORY) %>%
  mutate(OBSERVATION_COUNT=as.numeric(as.character(OBSERVATION_COUNT)))

# dat3 <- dat2 %>%
#   dplyr::filter(SAMPLING_EVENT_IDENTIFIER %in% lists_to_keep$SAMPLING_EVENT_IDENTIFIER) %>%
#   replace_na(list(GROUP_IDENTIFIER="keep")) %>%
#   dplyr::filter(GROUP_IDENTIFIER=="keep") %>%
#   dplyr::select(-GROUP_IDENTIFIER, -EFFORT_DISTANCE_KM, -EFFORT_AREA_HA, -NUMBER_OBSERVERS, -CATEGORY) 

saveRDS(dat4, "Data/ebird_data_raw.RDS")

# filter to breeding season only
dat5 <- dat4 %>%
  mutate(MONTH=month(OBSERVATION_DATE, label=TRUE, abbr=TRUE))


# split data by month
# and write out each as an RDS
split_by_month_function <- function(month){
  
  temp <- dat5 %>% 
    dplyr::filter(MONTH==month)
  
  saveRDS(temp, paste0("Data/ebird_data_raw_", month, ".RDS"))
  
  
}

lapply(unique(dat5$MONTH), split_by_month_function)


