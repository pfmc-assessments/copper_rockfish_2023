################################################################################
### Process the CDFW PR data for an index of abundance
### Depending on the species and the area you may need to modify the filters
### Copper assessment 2023
### Melissa Monk
################################################################################
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
modelArea = "north"

#setwd to the north or the south
#set working directory
setwd("S:/copper_rockfish_2023/data/rec_indices/crfs_pr_dockside")
#setwd(glue::glue(here(),"/data/rec_indices/crfs_pr_dockside/"))
out.dir <- glue::glue(getwd(),'/',modelArea,'/')

#load data for processing
load("all_pr_data.RData")

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
cdfwpr <- all_pr_data %>% 
  mutate(area = ifelse(district > 2, "north", "south")) %>%
  filter(area == modelArea) %>%
  mutate(cpue = kept/anglers)

#cpue over time to see if there's a reduction in copper cpue in 2022
#will run with and without 2022
q = c(.25, .5, .75)
cpue_year <- cdfwpr %>%
filter(kept>0) %>%
group_by(year) %>%
 summarize(
  min = min(cpue),
  quant25 = quantile(cpue, probs = q[1]), 
            quant50 = quantile(cpue, probs = q[2]),
            quant75 = quantile(cpue, probs = q[3]),
            max = max(cpue))
cpue_year
#histogram of the number of fish per bag
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("All data")
dataFilters$Description[filter.num] <- c("All data")
dataFilters$Samples[filter.num] <- cdfwpr %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(kept>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#Remove 2020-2021 due to covid
cdfwpr <- cdfwpr %>%
filter(!year %in% c(2020,2021))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Year")
dataFilters$Description[filter.num] <- c("Remove 2020,2021 due to COVID sampling restrictions")
dataFilters$Samples[filter.num] <- cdfwpr %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(kept>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#Look at where the target is found within the possible remaining data
target <- cdfwpr %>% filter(kept>0)
summary(as.factor(cdfwpr$area_x))
summary(as.factor(target$area_x))
#remove mexico, na, and bays and harbors
area.to.remove <- c("Mexico","Bay/estuary/harbor")

cdfwpr <- cdfwpr %>%
  filter(!area_x %in% area.to.remove,
         !is.na(area_x))
#Look at where the target is found within the possible remaining data
target <- cdfwpr %>% filter(kept>0)
summary(as.factor(cdfwpr$area_x))
summary(as.factor(target$area_x))
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Areas fished")
dataFilters$Description[filter.num] <- c("Retain trips occuring in ocean areas")
dataFilters$Samples[filter.num] <- cdfwpr %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(kept>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#remove uncommon gears and then look at the target
gears <- cdfwpr %>%
  group_by(geara) %>%
  tally() %>%
  filter(n>100)

#remove uncommon gears
cdfwpr <- cdfwpr %>%
  filter(geara %in% gears$geara)

#new target table
target <- cdfwpr %>% filter(kept>0)
summary(as.factor(target$geara))
summary(as.factor(cdfwpr$geara))
#1 = Hook & line
#2 = Dip net
#3 = Cast net
#4 = Gill net
#5 = Seine
#6 = Trawl
#7 = Trap
#8 = Spear/spear gun
#9 = Hand
#10 = Other
#Blank = no gear recorded or not the first species and location in a PR1 sample


#keep only hook and line - explore keeping troll as well
gears.to.keep <- c('H','Hook and Line', 'T', 'Troll')
cdfwpr <- cdfwpr %>%
  filter(geara %in% gears.to.keep)

target <- cdfwpr %>% filter(kept>0)
summary(as.factor(target$geara))
summary(as.factor(cdfwpr$geara))
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Gear")
dataFilters$Description[filter.num] <- c("Retain trips with primary gear of hook-and-line")
dataFilters$Samples[filter.num] <- cdfwpr %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(kept>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#number of anglers
summary(cdfwpr$anglers)
#seems reasonable

#months
summary(as.factor(cdfwpr$month))
target <- cdfwpr %>% filter(kept>0)
summary(as.factor(target$month))
#remove Jan-March in the north - no rockfishing these months
#remove Jan-Feb in the south - no rockfishing these months
if(modelArea =="north"){
  cdfwpr <- cdfwpr %>%
    filter(month > 3)
} else {
  cdfwpr <- cdfwpr %>%
    filter(month > 2)
}
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Months fished")
dataFilters$Description[filter.num] <- c("Remove Jan-Feb; recreational rockfish fishery closed")
dataFilters$Samples[filter.num] <- cdfwpr %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(kept>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#LOOK AT THE TARGET SPECIES 
#trips by target species and put all rockfish entries in the rockfish genus 
#trips with lingcod in bottomfish
cdfwpr$prim1Common <- trimws(cdfwpr$prim1Common, which = c("right"))
cdfwpr$prim1Common[grepl("rockfish",cdfwpr$prim1Common)] <- "rockfish genus"
cdfwpr$prim1Common[grepl("lingcod",cdfwpr$prim1Common)] <- "bottomfish (groundfish)"
cdfwpr$prim1Common[grepl("bocaccio",cdfwpr$prim1Common)] <- "rockfish genus"
cdfwpr$prim1Common[grepl("salmon",cdfwpr$prim1Common)] <- "salmon"
#same for prim2
cdfwpr$prim2Common <- trimws(cdfwpr$prim2Common, which = c("right"))
cdfwpr$prim2Common[grepl("rockfish",cdfwpr$prim2Common)] <- "rockfish genus"
cdfwpr$prim2Common[grepl("lingcod",cdfwpr$prim2Common)] <- "bottomfish (groundfish)"
cdfwpr$prim2Common[grepl("bocaccio",cdfwpr$prim2Common)] <- "rockfish genus"
cdfwpr$prim2Common[grepl("salmon",cdfwpr$prim2Common)] <- "salmon"

#primary target species only
tripTargets <- cdfwpr %>%
  group_by(prim1Common) %>%
  summarise(tripsWithTarget = sum(kept>0),
            tripsWOTarget = sum(kept==0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
         percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 

tripTargetFilter  <- tripTargets %>%
  filter(tripsWithTarget > 0) %>%
  filter(totalTrips >= 500) %>%
  filter(percentpos > 0.02)

#filter to keept only identified primary targets chose in tripTargetFilter
cdfwpr <- cdfwpr %>%
  filter(prim1Common %in% tripTargetFilter$prim1Common)

#look at secondary trip target
prim1_2summary <- cdfwpr %>%
  group_by(prim1Common, prim2Common) %>%
  summarise(tripsWithTarget = sum(kept)) %>% #
pivot_wider(names_from = prim2Common, values_from = tripsWithTarget)

#
trip2Targets <- cdfwpr %>%
  group_by(prim2Common) %>%
  summarise(tripsWithTarget = sum(kept),
            tripsWOTarget = sum(kept == 0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
         percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 

#-------------------------------------------------------------------------------
#Retain trips with just the primary species being bottom fish or rockfish
#want to try and avoid mixed trip effort
#Could change this - esp. for the south...more mixed trips, but really want just
#trips with rockfihs effort

cdfwpr <- cdfwpr %>%
mutate(keep.trip = ifelse(
  prim1Common %in% c('bottomfish (groundfish)', 'rockfish genus') |
  prim2Common %in% c('bottomfish (groundfish)', 'rockfish genus'), 1,0)) %>%
  filter(keep.trip==1)
 
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Target species")
dataFilters$Description[filter.num] <- c("Retain trips with primary target of 
rockfish or bottomfish target; or as secondary target species for trips identified in 
the previous tabletrips")
dataFilters$Samples[filter.num] <- cdfwpr %>% tally()
dataFilters$Positive_Samples[filter.num] <- cdfwpr %>% filter(kept>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#save the datafile and filters for the run file
save(cdfwpr, tripTargets, dataFilters, file = 
paste0(file.path(getwd(),modelArea),"/data_for_glm.RData"))
