#########################################################################
### Explore the CDFW PR data for an index of abundance
### Copper assessment 2023
### Melissa Monk
#########################################################################
rm(list = ls(all = TRUE))
graphics.off()
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)
#species and area identifiers - eventually put in function
targetAbbr <- 'COPP'

#set working directory
setwd(here())
setwd(glue::glue(here(),"/data/rec_indices/crfs_pr_dockside"))

#load data from RecFIN - takes a minute; 2619429 rows
#dataframe cdfw_crfs
load("ca_comprehensive_rec_sample.RData")

#-------------------------------------------------------------------------------
#Filter RecFIN data just the PR data and the area
#Using both PR1 and PR2
#look at where coppers caught
cdfw_crfs %>% 
  filter(PACFIN_SPECIES_CODE == 'COPP') %>%
  group_by(RECFIN_MODE_NAME) %>%
  summarise(numberfish = sum(NUMBER_KEPT_OBSERVED))

#only a handful from beach/bank and man-made/jetty
#look at where coppers caught
cdfw_crfs %>% 
  filter(PACFIN_SPECIES_CODE == 'COPP') %>%
  group_by(RECFIN_WATER_AREA_NAME) %>%
  summarise(numberfish = sum(NUMBER_KEPT_OBSERVED))


#really should be using 2014 and on
anglers88 <- cdfw_crfs %>% filter(NUMBER_OF_ANGLERS==88) %>%
  dplyr::select(ANGLER_ID, RECFIN_YEAR, RECFIN_PORT_CODE,
                INT_COUNTY_NAME, RECFIN_YEAR, RECFIN_MONTH, 
                RECFIN_DAY, NUMBER_OF_ANGLERS) %>%
  unique()
summary(as.factor(anglers88$RECFIN_YEAR))
#Anglers==88 drops off in 2014

#Filter the data right off the bat to include only 2014 to present
#also drop 2020,2021
#keep only PR mode
cdfwpr <- cdfw_crfs %>%
  filter(RECFIN_YEAR >2013,
         RECFIN_MODE_CODE == 7) %>%
  filter(!RECFIN_YEAR %in% c(2020,2021))

#how many trips per year and district
YearDistrict <- cdfwpr %>%
  group_by(ANGLER_ID, RECFIN_YEAR, RECFIN_MONTH, 
                           RECFIN_DAY, RECFIN_PORT_CODE) %>% 
  tally() %>%
  rename(total.trips = n) %>%
  pivot_wider(names_from = RECFIN_PORT_CODE, values_from = total.trips)

# look at number of fish and cpue for qa qc
cdfwTarget.qaqc <- cdfwpr %>%
  dplyr::select(SAMPLE_ID, ANGLER_ID, LOCATION_ID, CATCH_ID,
                RECFIN_YEAR, RECFIN_MONTH, RECFIN_DAY, RECFIN_PORT_CODE,
                SPECIES_NAME, NUMBER_KEPT_OBSERVED, NUMBER_OF_ANGLERS) %>%
  mutate(cpue = NUMBER_KEPT_OBSERVED/NUMBER_OF_ANGLERS) %>%
  filter(NUMBER_KEPT_OBSERVED > 0,
         grepl(targetAbbr, SPECIES_NAME))

quantile(cdfwTarget.qaqc$NUMBER_KEPT_OBSERVED, seq(0,1,.025))
quantile(cdfwTarget.qaqc$cpue, seq(0,1,.025))

cdfwTarget.qaqc <- cdfwTarget.qaqc %>%
  filter(cpue > 9 | NUMBER_KEPT_OBSERVED > 20)
#write.csv(cdfwTarget.qaqc, 'copper_pr_qaqc.csv')


prDat <- cdfwpr %>%
  mutate(WAVE = case_when(RECFIN_MONTH %in% c(1,2) ~ 1,
                          RECFIN_MONTH %in% c(3,4) ~ 2,
                          RECFIN_MONTH %in% c(5,6) ~ 3,
                          RECFIN_MONTH %in% c(7,8) ~ 4,
                          RECFIN_MONTH %in% c(9,10) ~ 5,
                          RECFIN_MONTH %in% c(11,12) ~ 6)) %>%
  mutate_at(vars(WAVE), as.factor)



save(prDat, file = "crfs_pr_dat_to_process.RData" )

#-------------------------------------------------------------------------------



