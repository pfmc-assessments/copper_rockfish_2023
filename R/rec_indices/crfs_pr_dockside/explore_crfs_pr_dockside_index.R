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

#look at water area fished
table(cdfw_crfs$RECFIN_WATER_AREA_CODE, cdfw_crfs$RECFIN_WATER_AREA_NAME)
#keep 1,2,3

#check to see which water area names the target species was observed
#make sure not any in inland or the bays
cdfwTarget <- cdfw_crfs %>%
  filter(NUMBER_KEPT_OBSERVED > 0,
         NUMBER_OF_ANGLERS >0,
         grepl(targetAbbr,SPECIES_NAME))
#very few inland and mexico

with(cdfwTarget,table(RECFIN_WATER_AREA_NAME, RECFIN_YEAR))


#RECFIN_SUBREGION_CODE
#1 = southern CA
#2 = northern CA
#9 = not known

#need to remove any row that was unobserved
prDat <- cdfw_crfs %>%
  filter(RECFIN_MODE_CODE == 7, # PR only
         RECFIN_WATER_AREA_CODE %in% c(1,2,3), # ocean only
         RECFIN_PORT_CODE != 25, #portcode 25 is NA
         INT_COUNTY_NAME != '',
         !NUMBER_OF_ANGLERS %in% c(0, 88, 888, NA)) #remove 0 anglers and 88 is NA
      #   NUMBER_KEPT_OBSERVED > 0)

# look at number of fish and cpue for qa qc
cdfwTarget.qaqc <- prDat %>%
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
write.csv(cdfwTarget.qaqc, 'copper_pr_qaqc.csv')


#Need to get the individual trips
#ANGLER_ID, INT_COUNTY_NAME, RECFIN_YEAR, RECFIN_MONTH, RECFIN_DAY, 
#NUMBER_HOURS_FISHED, NUMBER_OF_ANGLERS, HOURS_FISHED_CPUE, ANGLER_CPUE
#

prTrips <- prDat %>%
  dplyr::select(ANGLER_ID, INT_COUNTY_NAME, RECFIN_YEAR, RECFIN_MONTH, 
                RECFIN_DAY, NUMBER_HOURS_FISHED, NUMBER_OF_ANGLERS, 
                HOURS_FISHED_CPUE, SURVEY, RECFIN_PORT_CODE,
                INTERVIEW_SITE_NAME) %>%
  unique()

#See how many trips had 0 observed and 0 unobserved catch
prTripKept <- prDat %>%
  group_by(ANGLER_ID, RECFIN_YEAR) %>%
  summarise(obkeptsum = sum(NUMBER_KEPT_OBSERVED),
            unobskeptsum = sum(NUMBER_KEPT_UNOBSERVED))

tripsAllUnob <- prTripKept %>% filter(obkeptsum==0 & unobskeptsum>0) 
tripsNokept <- prTripKept %>% filter(obkeptsum==0 & unobskeptsum==0) 
length(tripsAllUnob$ANGLER_ID) #47,699 trips where all of the kept fish were unobserved
length(tripsNokept$ANGLER_ID) #116,385 trips where nothing was kept, may be reported discard

#how many of the the trips with no observed catch were in each year
tripsNokept %>% group_by(RECFIN_YEAR) %>% tally()
unob.year <-tripsAllUnob %>% group_by(RECFIN_YEAR) %>% tally() %>%
  rename(unobcatch.trips = n)
#unobserved catch highest in 2020 and some in 2021 as expected
#trips where nothing was kept highest in the early 2000s

#total trips
prTrips.year <- prTrips %>% group_by(RECFIN_YEAR) %>% tally() %>%
  rename(total.trips = n)
#look at the percent of trips where all catch was unobserved by year
pryear <- inner_join(unob.year, prTrips.year) %>%
  mutate(percentUnobs = unobcatch.trips/total.trips)
pryear


#look at the target species - how many kept obserserved, unobserved,
#discarded, and descenced, ets
cdfwTargetcnt <- prDat %>%
         filter(grepl(targetAbbr,SPECIES_NAME)) 

total.by.cat <- cdfwTargetcnt %>%
  group_by(RECFIN_YEAR, RECFIN_SUBREGION_CODE) %>%
  summarise(tot.obkept = sum(NUMBER_KEPT_OBSERVED),
            tot.unobkept = sum(NUMBER_KEPT_UNOBSERVED),
            tot.rel = sum(NUMBER_RELEASED_ALIVE+NUMBER_RELEASED_DEAD))

#make sure there are no missing data where needed
summary(as.factor(prTrips$INT_COUNTY_NAME)) 
summary(as.factor(prTrips$RECFIN_PORT_CODE))
summary(prDat$RECFIN_YEAR)
summary(prDat$RECFIN_MONTH)
summary(prDat$NUMBER_OF_ANGLERS) #0 for some, NAs and 88 should be removed

#save the prDat and prTrips
#need to remove any row that was unobserved
prDat <- prDat %>%
  filter(NUMBER_KEPT_OBSERVED > 0)


prDat <- prDat %>%
  mutate(WAVE = case_when(RECFIN_MONTH %in% c(1,2) ~ 1,
                          RECFIN_MONTH %in% c(3,4) ~ 2,
                          RECFIN_MONTH %in% c(5,6) ~ 3,
                          RECFIN_MONTH %in% c(7,8) ~ 4,
                          RECFIN_MONTH %in% c(9,10) ~ 5,
                          RECFIN_MONTH %in% c(11,12) ~ 6)) %>%
  mutate_at(vars(WAVE), as.factor)



save(prDat, file = "crfs_pr_dat_to_process.RData" )

#-------------------------------------------------------------------------------



