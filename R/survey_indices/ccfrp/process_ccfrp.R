################################################################################
### CCFRP data filtering and prep
### Copper rockfish assessment 2023
### Melissa Monk
################################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RODBC)
library(here)
#species and area identifiers
pacfinSpecies <- 'COPP'
speciesName <- "copper"
ccfrpSpeciesCode <- "CPR"
modelArea = "south"

#setwd to the north or the south
#dir <- file.path(here(),"data","survey_indices","ccfrp")
#setwd(dir)
#out.dir <- file.path(getwd(),modelArea)
#load data

load("S:/copper_rockfish_2023/data/survey_indices/ccfrp/CCFRP.RData")
#set working directory
dir <- file.path("S:/copper_rockfish_2023/data/survey_indices/ccfrp",modelArea)
setwd(dir)


####data for Arc maps
grid_cells <- dat %>%
  group_by(gridCellID) %>%
  summarise(sum_copper = sum(Target))
grid_cells <- left_join(grid_cells, cellLocation, by = "gridCellID")
write.csv(grid_cells, "S:/copper_rockfish_2023/data/survey_indices/ccfrp/copper_for_arc.csv")



if(modelArea=="south"){
  dat <- dat %>%
  filter(region == "South")
} else {
  dat <- dat %>%
  filter(!region == "South")
}

#-------------------------------------------------------------------------------
# Data filter dataframe
filter.num <- 1
data_filters <- data.frame(matrix(vector(), 10, 4,
  dimnames = list(c(), c(
    "Filter", "Description", "Samples",
    "Positive_Samples"))), stringsAsFactors = F)
#-------------------------------------------------------------------------------
# Add to filter dataframe
data_filters$Filter[filter.num] <- c("All data")
data_filters$Description[filter.num] <- c("")
data_filters$Samples[filter.num] <- dim(dat)[1]
data_filters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#Drop locations sampled 1-2 years
summary(as.factor(dat$area))
area_to_keep <- dat %>%
  group_by(area) %>%
  summarise(n = n_distinct(year)) %>%
  filter(n>2)
dat <- dat %>%
  filter(area %in% area_to_keep$area)

#Remove drifts noted to exclude
dat <- dat %>% 
  filter(is.na(excludeDrift))
#Look at cell not well sampled
#Drop cells not well sampled = usually tested the first year and then removed
cells_to_keep <- dat %>%
  group_by(gridCellID) %>%
  summarise(n = n_distinct(year)) %>%
  filter(n>2)
dat <- dat %>%
  filter(gridCellID %in% cells_to_keep$gridCellID) %>%
  droplevels()

#drop cells marked as have the following last two character
#taken care of now with filter above
#dat <- dat %>% filter(!(grepl("MM", .$gridCellID) | grepl("RR", .$gridCellID) |
#  grepl("MN", .$gridCellID) | grepl("MO", .$gridCellID)))
#-------------------------------------------------------------------------------
# Add to filter dataframe
data_filters$Filter[filter.num] <- c("Sampling frequency")
data_filters$Description[filter.num] <- c("Remove locations and cells not well 
                                          sampled and drifts marked for exclusion")
data_filters$Samples[filter.num] <- dim(dat)[1]
data_filters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
# look at each location to see if coppers observed
drifts_by_area <- dat %>%
  group_by(area) %>%
  tally() %>%
  rename(total_drifts = n)

# tally how many coppers in an area
target_by_area <- dat %>%
  filter(Target>0) %>%
  group_by(area) %>%
  summarise(tot = sum(Target))
target_area <- inner_join(drifts_by_area, target_by_area)

#Remove swami's in the south
if(modelArea=="south"){
  dat <- dat %>%
    filter(area != "SW")
  #-------------------------------------------------------------------------------
  # Add to filter dataframe
  data_filters$Filter[filter.num] <- c("Location")
  data_filters$Description[filter.num] <- c("Remove Swami's; only 5 coppers caught")
  data_filters$Samples[filter.num] <- dim(dat)[1]
  data_filters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
  filter.num <- filter.num + 1
  #-------------------------------------------------------------------------------  
}
#-------------------------------------------------------------------------------
#grid cells where the target was never observed
#see how many
target_by_gridcell <- dat %>%
   group_by(gridCellID,name) %>%
   summarise(target = sum(Target),
   count = n())
gridcell.to.keep <- target_by_gridcell %>%
filter(target>0)
length(unique(target_by_gridcell$gridCellID))
length(unique(gridcell.to.keep$gridCellID))
#lose 20 grid cells
dat <- dat %>%
filter(gridCellID %in% gridcell.to.keep$gridCellID)
#-------------------------------------------------------------------------------
# Add to filter dataframe
data_filters$Filter[filter.num] <- c("Location")
data_filters$Description[filter.num] <- c("Remove grid cells that never observed
                                           the target species")
data_filters$Samples[filter.num] <- dim(dat)[1]
data_filters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# Fished time filter
#Remove drifts fished less than two minutes
dat <- dat %>%
  filter(driftTime > (2/60))

# Give drifts within a cell on the same day a drift number
# See how many drifts and total fished time
Num_drifts_fished <- dat %>%
  group_by(tripCellID) %>%
  summarise(
    num.drifts = n(),
    tot_time = sum(driftTime)) %>%
  filter(tot_time >= .25)


ggplot(Num_drifts_fished, aes(x = tot_time, colour = "#E69F00", fill = "#E69F00")) +
  geom_histogram(show.legend = FALSE) +
     xlab("Hours fished") + ylab("Count") +
       scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "time_fished.png"), width = 7, height = 7)

# Remove cells fished less than a total of 15 minutes on a day
dat <- dat %>%
  filter(tripCellID %in% Num_drifts_fished$tripCellID)
#-------------------------------------------------------------------------------
# Add to filter dataframe
data_filters$Filter[filter.num] <- c("Time fished")
data_filters$Description[filter.num] <- c("Remove drifts less than two minutes 
                                          and cells fished less than 15 minutes
                                          during a sampling event")
data_filters$Samples[filter.num] <- dim(dat)[1]
data_filters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------




#-------------------------------------------------------------------------------
## Raw cpue
avg.cpue.area <- dat %>%
  group_by(year, area, site) %>%
  summarise(avg.cpue = mean(cpue))

# Plot the average cpue by year and reef
ggplot(avg.cpue.area, aes(year, avg.cpue, colour = as.factor(area))) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  facet_wrap(~site) +
  theme_bw() +
  labs(colour = "Area", x = "Year", y = "Average CPUE") +
     scale_color_viridis_d()
ggsave(paste0(dir, "/Average CPUE by year and site.png"),
  width = 7, height = 7, units = "in")

with(dat, table(year, site))
with(dat, table(year, area))

percent_pos <-round(with(subset(dat, Target > 0), table(area, site)) / with(dat, table(area, site)), 2)
write.csv(percent_pos, file.path(dir, "percent_pos.csv"))

round(with(subset(dat, Target > 0), table(site)) / with(dat, table(site)), 2)


#-------------------------------------------------------------------------------
#write out length data
target_lengths <- lengths %>%
dplyr::select(fishID, driftID, lengthcm, sex, speciesCode, monitoringGroup, name, 
              site.x, tripID, year, name) %>%
rename(site = site.x) %>%
filter(driftID %in% dat$driftID)

save(target_lengths, file = file.path(dir,"CCFRP_lengths.RData"))
write.csv(target_lengths,  
file = file.path(dir,"CCFRP_lengths.csv"), row.names = FALSE)



#-------------------------------------------------------------------------------
#map of copper locations
#do later


save(dat, data_filters,file = file.path(dir,"Filtered_data_CCFRP.RData"))


