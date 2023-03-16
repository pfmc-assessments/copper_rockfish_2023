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

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"


#setwd to the north or the south
#set working directory
setwd(here())
setwd(glue::glue(here(),"/data/rec_indices/crfs_pr_dockside/"))
out.dir <- glue::glue(getwd(),'/',modelArea,'/')

#load data for processing
load("crfs_pr_dat_to_process.RData")


#-------------------------------------------------------------------------------
#Filter to north or south
#Need to get the individual trips
#remove blank species names
prDat <- prDat %>% 
  filter(!PACFIN_SPECIES_CODE %in% c('NOT KNOWN','')) %>%
  mutate(area = ifelse(RECFIN_PORT_CODE > 2, "north", "south")) %>%
  filter(area == modelArea)
           
#get unique trips
prTrips <- prDat %>%
  dplyr::select(ANGLER_ID, INT_COUNTY_NAME, RECFIN_YEAR, RECFIN_MONTH, 
                RECFIN_DAY, NUMBER_OF_ANGLERS, RECFIN_PORT_CODE, WAVE,
                PRIMARY_TARGET_SPECIES_NAME, SECONDARY_TARGET_SPECIES_NAME) %>%
  unique()

# Add copper observations to the trip table
targetKept <- prDat %>%
  filter(PACFIN_SPECIES_CODE == pacfinSpecies,
         NUMBER_KEPT_OBSERVED > 0) %>%
  dplyr::select(ANGLER_ID, NUMBER_KEPT_OBSERVED)

prTrips <- left_join(prTrips, targetKept) %>%
  mutate(targetKeptObs = ifelse(is.na(NUMBER_KEPT_OBSERVED), 0, NUMBER_KEPT_OBSERVED)) %>%
  mutate(targetPresence = ifelse(targetKeptObs == 0 , 0, 1))
#-------------------------------------------------------------------------------
# Data filter dataframe
filter.num <- 1
dataFilters <- data.frame(matrix(vector(), 10, 4,
  dimnames = list(c(), c(
    "Filter", "Description", "Samples",
    "Positive_Samples"
  ))
),
stringsAsFactors = F
)

# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("All data")
dataFilters$Description[filter.num] <- c("Pre-filtered for drifts with marked for exclusion")
dataFilters$Samples[filter.num] <- length(unique(prTrips$ANGLER_ID))
dataFilters$Positive_Samples[filter.num] <- dim(subset(prTrips, targetKeptObs > 0))[1]
filter.num <- filter.num + 1

#Remove 2020, 2021, 2022
prTrips <- prTrips %>%
  filter(RECFIN_YEAR < 2020)

prDat <- prDat %>%
  filter(RECFIN_YEAR < 2020)

# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Year 2020-2022")
dataFilters$Description[filter.num] <- c("Remove 2020 due to decreased sampling.")
dataFilters$Samples[filter.num] <- length(unique(prTrips$ANGLER_ID))
dataFilters$Positive_Samples[filter.num] <- dim(subset(prTrips, targetKeptObs > 0))[1]
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#Look at waves
#remove wave 1 - no rockfishing these months 
with(prDat, table(RECFIN_YEAR, WAVE))

prTrips <- prTrips %>%
  filter(WAVE != 1) %>%
  droplevels

#update prDat 
prDat <- prDat %>%
  filter(ANGLER_ID %in% prTrips$ANGLER_ID)

# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Waves")
dataFilters$Description[filter.num] <- paste0("Remove Jan/Feb due to small sample sizes and fishery closures.")
dataFilters$Samples[filter.num] <- length(unique(prTrips$ANGLER_ID))
dataFilters$Positive_Samples[filter.num] <- dim(subset(prTrips, targetKeptObs > 0))[1]
filter.num <- filter.num + 1

#-------------------------------------------------------------------------------
#LOOK AT THE TARGET SPECIES 
#trips by target species and put all rockfish entries in the rockfish genus 

tripTargets <- prTrips %>%
  mutate(SECONDARY_TARGET_SPECIES_NAME = 
           ifelse(SECONDARY_TARGET_SPECIES_NAME == '', "UNKNOWN", SECONDARY_TARGET_SPECIES_NAME)) %>%
  mutate(PRIMARY_TARGET_SPECIES_NAME = 
           ifelse(PRIMARY_TARGET_SPECIES_NAME == '', "UNKNOWN", PRIMARY_TARGET_SPECIES_NAME)) 

#trips with lingcod in bottomfish
tripTargets$PRIMARY_TARGET_SPECIES_NAME[grepl("rockfish",tripTargets$PRIMARY_TARGET_SPECIES_NAME)] <- "rockfish genus"
tripTargets$SECONDARY_TARGET_SPECIES_NAME[grepl("rockfish",tripTargets$SECONDARY_TARGET_SPECIES_NAME)] <- "rockfish genus"
tripTargets$PRIMARY_TARGET_SPECIES_NAME[grepl("lingcod",tripTargets$PRIMARY_TARGET_SPECIES_NAME)] <- "bottomfish (groundfish)"
tripTargets$SECONDARY_TARGET_SPECIES_NAME[grepl("lingcod",tripTargets$SECONDARY_TARGET_SPECIES_NAME)] <- "bottomfish (groundfish)"

 

#primary target species only
tripTargets <- tripTargets %>%
  group_by(PRIMARY_TARGET_SPECIES_NAME) %>%
  summarise(tripsWithTarget = sum(targetPresence),
            tripsWOTarget = sum(targetPresence == 0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
    percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) # %>%
# pivot_wider(names_from = SECONDARY_TARGET_SPECIES_NAME, values_from = percentpos)

tripTargetFilter <- tripTargets %>%
  filter(tripsWithTarget > 0,
         totalTrips >= 1000,
         percentpos >= 0.1)

#look at secondary trip target for unidentified fish
unIDTarget <- prTrips %>%
  mutate(SECONDARY_TARGET_SPECIES_NAME = 
           ifelse(SECONDARY_TARGET_SPECIES_NAME == '', "UNKNOWN", SECONDARY_TARGET_SPECIES_NAME)) %>%
  filter(PRIMARY_TARGET_SPECIES_NAME == 'unidentified fish') %>%
  group_by(SECONDARY_TARGET_SPECIES_NAME) %>%
    summarise(tripsWithTarget = sum(targetPresence),
              tripsWOTarget = sum(targetPresence == 0)) %>%
    mutate(totalTrips = tripsWithTarget+tripsWOTarget,
           percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget))

#exploratory
#for now remove unknown and unidentified fish trips
dd <- prDat %>%
  filter(PRIMARY_TARGET_SPECIES_NAME %in% c('UNKNOWN', 'unidentified fish')) %>%
  group_by(ANGLER_ID, SPECIES_NAME) %>%
  summarise(fish = sum(NUMBER_KEPT_OBSERVED)) %>%
  pivot_wider(names_from = SPECIES_NAME, values_from = fish)
#-------------------------------------------------------------------------------
#Remove trips with just the primary species left in the tripTargetFilter
tripTargetFilter <- tripTargetFilter %>%
  filter(PRIMARY_TARGET_SPECIES_NAME %in% c('bottomfish (groundfish)', 'rockfish genus'))


prTrips <- prTrips %>%
  filter(PRIMARY_TARGET_SPECIES_NAME %in% tripTargetFilter$PRIMARY_TARGET_SPECIES_NAME)

summary(as.factor(prTrips$PRIMARY_TARGET_SPECIES_NAME))

# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Groundfish")
dataFilters$Description[filter.num] <- c("Removed trips not targetting groundfish")
dataFilters$Samples[filter.num] <- length(unique(prTrips$ANGLER_ID))
dataFilters$Positive_Samples[filter.num] <- dim(subset(prTrips, targetKeptObs > 0))[1]
filter.num <- filter.num + 1


with(prTrips, table(RECFIN_YEAR, INT_COUNTY_NAME))
with(prTrips, table(RECFIN_YEAR, RECFIN_PORT_CODE))
with(prTrips, table(RECFIN_YEAR, RECFIN_MONTH))
with(prTrips, table(RECFIN_YEAR, WAVE))
round(with(subset(prTrips, targetKeptObs > 0), table(WAVE, RECFIN_PORT_CODE)) / with(prTrips, table(WAVE, RECFIN_PORT_CODE)), 2)
round(with(subset(prTrips, targetKeptObs > 0), table(RECFIN_YEAR, RECFIN_PORT_CODE)) / with(prTrips, table(RECFIN_YEAR, RECFIN_PORT_CODE)), 2)



save(prTrips, dataFilters, file = glue(
  out.dir, "crfs_pr_dockside_data_for_GLM_", speciesName,
  "_", modelArea, ".RData"
))
