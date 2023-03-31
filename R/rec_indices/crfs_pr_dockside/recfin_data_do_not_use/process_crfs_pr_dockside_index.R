#########################################################################
### Process the CDFW PR data for an index of abundance
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
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "south"


#setwd to the north or the south
#set working directory
setwd(here())
setwd(glue::glue(here(),"/data/rec_indices/crfs_pr_dockside/"))
out.dir <- glue::glue(getwd(),'/',modelArea,'/')

#load data for processing
load("crfs_pr_dat_to_process.RData")
trip.identifiers <- c("ANGLER_ID", "RECFIN_YEAR", "RECFIN_MONTH", 
                      "RECFIN_DAY", "RECFIN_PORT_CODE")
# Data filter dataframe
filter.num <- 1
dataFilters <- data.frame(matrix(vector(), 10, 4,
                                 dimnames = list(c(), c(
                                   "Filter", "Description", "Samples",
                                   "Positive_Samples"
                                 ))), stringsAsFactors = F)
#-------------------------------------------------------------------------------
#Filter to north or south
#Need to get the individual trips
#remove blank species names
cdfwpr <- prDat %>% 
  mutate(area = ifelse(RECFIN_PORT_CODE > 2, "north", "south")) %>%
  filter(area == modelArea)
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("All data")
dataFilters$Description[filter.num] <- c("Pre-filtered data for years 2014-2019, 2022")
dataFilters$Samples[filter.num] <- cdfwpr %>% dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% 
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#Remove trips with no observed catch
cdfwpr <- cdfwpr %>%
  filter(NUMBER_KEPT_OBSERVED>0)
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("No observed catch")
dataFilters$Description[filter.num] <- c("Retain trips with at least 1 observed fish")
dataFilters$Samples[filter.num] <- cdfwpr %>% dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% 
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#Look at number of anglers
summary(cdfwpr$NUMBER_OF_ANGLERS)
#all ok - no filter needed

#Look at ports
summary(cdfwpr$RECFIN_PORT_CODE)
#all ok - no filter needed

#look at water area and target species by water area
cdfwpr %>%
  dplyr::select(all_of(trip.identifiers), RECFIN_WATER_AREA_NAME) %>%
  unique() %>%
  group_by(RECFIN_WATER_AREA_NAME) %>%
  tally()

cdfwpr %>%
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  dplyr::select(all_of(trip.identifiers), RECFIN_WATER_AREA_NAME) %>%
  unique() %>%
  group_by(RECFIN_WATER_AREA_NAME) %>%
  tally()
#Only 75 trips with observed copper in inland, out of 18,978 inland trips in the north
#Only 7 from inland in the south
#remove inland, mexico and not known - not representative of the trips with copper
cdfwpr <- cdfwpr %>%
  filter(RECFIN_WATER_AREA_CODE %in% c(1,2,3))
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Areas fished")
dataFilters$Description[filter.num] <- c("Retain trips occuring in ocean areas")
dataFilters$Samples[filter.num] <- cdfwpr %>% dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% 
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#look at county or district to make sure we have one or the other
summary(as.factor(cdfwpr$INT_COUNTY_NAME))
summary(as.factor(cdfwpr$RECFIN_PORT_CODE))
#all ok - no filter needed

#look at time of year
summary(as.factor(cdfwpr$RECFIN_MONTH))

#remove Jan-March no rockfishing these months
if(modelArea =="north"){
cdfwpr <- cdfwpr %>%
  filter(RECFIN_MONTH > 3)
} else {
  cdfwpr <- cdfwpr %>%
    filter(RECFIN_MONTH > 2)
}
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Months fished")
dataFilters$Description[filter.num] <- c("Remove Jan-March; fishery closed")
dataFilters$Samples[filter.num] <- cdfwpr %>% dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% 
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  dplyr::select(all_of(trip.identifiers)) %>% unique %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#look at survey and copper in pr1 vs pr2

#look at water area and target species by water area
cdfwpr %>%
  dplyr::select(all_of(trip.identifiers), SURVEY) %>%
  unique() %>%
  group_by(SURVEY) %>%
  tally()

cdfwpr %>%
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  dplyr::select(all_of(trip.identifiers), SURVEY) %>%
  unique() %>%
  group_by(SURVEY) %>%
  tally()

coppers_year_district <- cdfwpr %>%
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  group_by(RECFIN_PORT_CODE, RECFIN_YEAR) %>%
  summarise(tot.coppers = sum(NUMBER_KEPT_OBSERVED)) %>%
  pivot_wider(names_from = RECFIN_PORT_CODE, values_from = tot.coppers)
#about the same fraction to keep pr2

#LOOK AT THE TARGET SPECIES 
#trips by target species and put all rockfish entries in the rockfish genus 
cdfwpr <- cdfwpr %>%
  mutate(SECONDARY_TARGET_SPECIES_NAME = 
           ifelse(SECONDARY_TARGET_SPECIES_NAME == '', "UNKNOWN", SECONDARY_TARGET_SPECIES_NAME)) %>%
  mutate(PRIMARY_TARGET_SPECIES_NAME = 
           ifelse(PRIMARY_TARGET_SPECIES_NAME == '', "UNKNOWN", PRIMARY_TARGET_SPECIES_NAME)) 

#trips with lingcod in bottomfish
cdfwpr$PRIMARY_TARGET_SPECIES_NAME[grepl("rockfish",cdfwpr$PRIMARY_TARGET_SPECIES_NAME)] <- "rockfish genus"
cdfwpr$SECONDARY_TARGET_SPECIES_NAME[grepl("rockfish",cdfwpr$SECONDARY_TARGET_SPECIES_NAME)] <- "rockfish genus"
cdfwpr$PRIMARY_TARGET_SPECIES_NAME[grepl("lingcod",cdfwpr$PRIMARY_TARGET_SPECIES_NAME)] <- "bottomfish (groundfish)"
cdfwpr$SECONDARY_TARGET_SPECIES_NAME[grepl("lingcod",cdfwpr$SECONDARY_TARGET_SPECIES_NAME)] <- "bottomfish (groundfish)"

#Get the Trip level table and then merge in the sum of the target presence
#look at water area and target species by water area
trips <- cdfwpr %>%
  dplyr::select(ANGLER_ID, INT_COUNTY_NAME, RECFIN_YEAR, RECFIN_MONTH, 
                RECFIN_DAY, NUMBER_OF_ANGLERS, WAVE, SURVEY, 
                RECFIN_PORT_CODE, INTERVIEW_SITE_NAME, 
                PRIMARY_TARGET_SPECIES_NAME, 
                SECONDARY_TARGET_SPECIES_NAME) %>%
  unique() 

postrips <- cdfwpr %>%
  filter(grepl(pacfinSpecies,SPECIES_NAME), NUMBER_KEPT_OBSERVED>0) %>%
  group_by(ANGLER_ID, RECFIN_YEAR, RECFIN_MONTH, 
           RECFIN_DAY, RECFIN_PORT_CODE) %>%
 summarise(targetCatch = sum(NUMBER_KEPT_OBSERVED))
tripData <- left_join(trips, postrips) %>%
  mutate(targetCatch = replace_na(targetCatch, 0))

 

#primary target species only
tripTargets <- tripData %>%
  group_by(PRIMARY_TARGET_SPECIES_NAME) %>%
  summarise(tripsWithTarget = sum(targetCatch),
            tripsWOTarget = sum(targetCatch == 0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
    percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) # %>%
# pivot_wider(names_from = SECONDARY_TARGET_SPECIES_NAME, values_from = percentpos)

tripTargetFilter <- tripTargets %>%
  filter(tripsWithTarget > 0,
         totalTrips >= 1000,
         percentpos >= 0.1)

#look at secondary trip target for unidentified fish
unIDTarget <- tripData %>%
  mutate(SECONDARY_TARGET_SPECIES_NAME = 
           ifelse(SECONDARY_TARGET_SPECIES_NAME == '', "UNKNOWN", SECONDARY_TARGET_SPECIES_NAME)) %>%
  filter(PRIMARY_TARGET_SPECIES_NAME == 'unidentified fish') %>%
  group_by(SECONDARY_TARGET_SPECIES_NAME) %>%
    summarise(tripsWithTarget = sum(targetCatch),
              tripsWOTarget = sum(targetCatch == 0)) %>%
    mutate(totalTrips = tripsWithTarget+tripsWOTarget,
           percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget))


#-------------------------------------------------------------------------------
#Retain trips with just the primary species being bottom fish or rockfish
#want to try and avoid mixed trip effort
#Could change this - esp. for the south...more mixed trips, but really want just
#trips with rockfihs effort
tripTargetFilter <- tripTargetFilter %>%
  filter(PRIMARY_TARGET_SPECIES_NAME %in% c('bottomfish (groundfish)', 'rockfish genus'))

tripData <- tripData %>%
  filter(PRIMARY_TARGET_SPECIES_NAME %in% tripTargetFilter$PRIMARY_TARGET_SPECIES_NAME)

summary(as.factor(tripData$PRIMARY_TARGET_SPECIES_NAME))
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Target species")
dataFilters$Description[filter.num] <- c("Retain trips with primary rockfish or bottomfish target")
dataFilters$Samples[filter.num] <- tripData %>% unique %>% tally()
dataFilters$Positive_Samples[filter.num] <- tripData %>% filter(targetCatch>0) %>% unique %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

with(tripData, table(RECFIN_YEAR, INT_COUNTY_NAME))
with(tripData, table(RECFIN_YEAR, RECFIN_PORT_CODE))
with(tripData, table(RECFIN_YEAR, RECFIN_MONTH))
with(tripData, table(RECFIN_YEAR, WAVE))
round(with(subset(tripData, targetCatch > 0), table(RECFIN_MONTH, RECFIN_PORT_CODE)) / 
        with(tripData, table(RECFIN_MONTH, RECFIN_PORT_CODE)), 2)
round(with(subset(tripData, targetCatch > 0), table(RECFIN_YEAR, RECFIN_PORT_CODE)) / 
        with(tripData, table(RECFIN_YEAR, RECFIN_PORT_CODE)), 2)

save(tripData, dataFilters, file = glue(
  out.dir, "crfs_pr_dockside_data_for_GLM.RData"
))
