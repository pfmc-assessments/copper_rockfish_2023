#########################################################################
### CCFRP data import and clean up
### Copper rockfish assessment 2023
### Melissa Monk
#########################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RODBC)
library(here)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
ccfrpSpeciesCode <- "CPR"
#setwd to the north or the south
setwd(glue::glue(here(),"/data/survey_indices/ccfrp/"))


#-------------------------------------------------------------------------------
#Read in data and basic cleanup
#odbcDriver does not like the spaces or hyphens in the database name
#does not want to read in a dynamic file name
channel <- odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                             DBQ=S:/copper_rockfish_2023/data/survey_indices/ccfrp/CCFRPDatabase.accdb")

#trips
trips <- sqlQuery(channel, "SELECT * FROM [1-Trip Information]")
#drifts
drifts <- sqlQuery(channel, "SELECT * FROM [3-Drift Information]")
#catches
catches <- sqlQuery(channel, "SELECT * FROM [4-Caught Fishes]")
#species 
specieslu <- sqlQuery(channel, "SELECT * FROM [Fish Species]")
#monitoring
areas <- sqlQuery(channel, "SELECT * FROM [Monitoring Areas]")
#returned tags
tagReturns <- sqlQuery(channel, "SELECT * FROM [Returned Tags Data]")
#cell locations
cellLocation <- sqlQuery(channel, "SELECT * FROM [Grid Cell Locations]")

odbcClose(channel)

#columns have spaces - remove all spaces here
trips <- trips %>% rename_all(make.names)
catches <- catches %>% rename_all(make.names)
drifts <- drifts %>% rename_all(make.names)
specieslu <- specieslu %>% rename_all(make.names)
areas <- areas %>% rename_all(make.names)
tagReturns <- tagReturns %>% rename_all(make.names)
cellLocation <- cellLocation %>% rename_all(make.names)

#Fixed two instances of a space after month by hand in access database
#clean up trip table
trips <- trips %>%
  rename(tripID = Trip.ID,
         area = Area,
         site = Site..MPA..REF.,
         year = Year.Automatic,
         month = Month,
         day = Day,
         vessel = Vessel,
         captain = Captain,
         deckhand = Deckhand,
         boatAnglers = X.Volunteer.Anglers,
         comments = Comments)

#clean up drift table
drifts <- drifts %>%
  rename(driftID = Drift.ID, 
         tripID = Trip.ID,
         tripCellID = ID.Cell.per.Trip, 
         gridCellID = Grid.Cell.ID,
         site = Site..MPA..REF., 
         driftTime = Drift.Time..hrs., 
         anglers = Total...Anglers.Fishing,
         startDepthft = Start.Depth..ft., 
         endDepthft = End.Depth..ft., 
         excludeDrift = Excluded.Drift.Comment,
         startLat = ST_LatDD, 
         startLong = ST_LonDD, 
         anglerHours = Total.Angler.Hrs,
         relief = Relief..1.3.)

#clean up catches table
catches <- catches %>%
  rename(fishID = Fish.ID,
         driftID = Drift.ID,
         speciesCode = Species.Code,
         tagID = Tag.ID,
         lengthcm = Length..cm.,
         gear = Gear.Type,
         station = Station..,
         anglerID = Angler.ID,
         sex = Sex,
         retained = Retained,
         recapture = Recapture,
         comments = Comments)

#clean up species look up table
specieslu <- specieslu %>%
  rename(speciesCode = Species.Code,
         commonName = Common.Name)

#clean up cell location
cellLocation <- cellLocation %>%
  rename(gridCellID = Grid.Cell.ID,
         area = Area,
         site = Site..MPA..REF.)

#clean up tag returns
tagReturns <- tagReturns %>%
  rename(tagReturnID = Tag.Return.ID,
         tagID = Tag.ID,
         speciesCode = Species.Code,
         monitoringGroup = Monitoring.Group)

#clean up monitoring areas
areas <- areas %>%
  rename(area = Area.code,
         name = Name,
         mpaArea = Area.of.MPA..km.2.,
         region = Region,
         monitoringGroup = Monitoring.Group)


#Join trip to areas
trips_areas <- left_join(trips, areas)
#join trips_areas to drifts
drifts_trip_area <- left_join(drifts, trips_areas, by = "tripID")

#-------------------------------------------------------------------------------
# Collapse catches to drift level
Target_catches <- subset(catches, speciesCode == ccfrpSpeciesCode)
Target_catches <- Target_catches %>%
  group_by(driftID) %>%
  tally()
colnames(Target_catches)[2] <- "Target"

#join drifts and catch info and make NA 0 where target species not observed
dat <- left_join(drifts_trip_area, Target_catches)
dat <- dat %>%
  mutate(
    Target = replace_na(Target, 0),
    area = substring(driftID, 1, 2)
  ) %>%
  mutate(effort = anglers * driftTime) %>%
  mutate(cpue = Target / effort)

lengths <- catches %>%
  filter(!is.na(lengthcm),
         speciesCode == ccfrpSpeciesCode)
lengths <- inner_join(lengths, drifts_trip_area, by = "driftID")
#-------------------------------------------------------------------------------
###look at depth data
summary(dat$startDepthft)
#NAs for 823 sites
summary(dat$endDepthft)
#NAs for 1995 sites

#where are the depth NA's
aa <- subset(dat, is.na(startDepthft))
summary(as.factor(aa$monitoringGroup))
#mostly humboldt that doesn't have depth
#cell could be included and accounts for depth likely

#exploratory plots
ggplot(dat %>% filter(cpue>0), aes(x = startDepthft , y = cpue )) +
  geom_point(alpha = .5)


ggplot(lengths, aes(lengthcm, color = name, fill = name)) +
  geom_density(alpha = .5) +
  facet_wrap(~region)


#how many drifts with coppers by MPA
total_effort <- dat %>% group_by(name) %>%
  summarise(total_effort = sum(effort, na.rm=TRUE))
#total coppers 
total_target <- dat %>% group_by(name) %>%
  summarise(total_target = sum(Target))
totals <- inner_join(total_effort, total_target) %>%
  mutate(total_cpue = total_target/total_effort)

#how often sites sampled
sites_sampled <- dat %>%
  group_by(name, year) %>%
  tally() %>%
  pivot_wider(names_from = name, values_from = n)
#remove Laguna Beach, Pt. C, Farallons, Trinidad 




save(areas, catches, cellLocation, dat, drifts, specieslu,
     tagReturns, dat, file = file.path(getwd(),"ccfrp.RData"))
