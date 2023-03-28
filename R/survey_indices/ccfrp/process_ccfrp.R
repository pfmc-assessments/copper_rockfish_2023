#########################################################################
### CCFRP data filtering and prep
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

#species and area identifiers
pacfinSpecies <- 'COPP'
speciesName <- "copper"
ccfrpSpeciesCode <- "CPR"
modelArea = "south"

#setwd to the north or the south
dir <- file.path(here(),"data","survey_indices","ccfrp")
setwd(dir)
out.dir <- file.path(getwd(),modelArea)

#load data
load("ccfrp.RData")

#Subset to the area of interest
if(modelArea == "north"){
  dat <- dat %>% 
    filter(region %in% c("Central", "North"))
} else {
  dat <- dat %>%
    filter(region =="South")
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
# Fish time filter
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

hist(Num_drifts_fished$tot_time)

# Remove cells fished less tan a total of 15 minutes on a day
dat <- dat %>%
  filter(tripCellID %in% Num_drifts_fished$tripCellID)
#-------------------------------------------------------------------------------
# Add to filter dataframe
data_filters$Filter[filter.num] <- c("Time fished")
data_filters$Description[filter.num] <- c("Remove drifts less than two minutes 
                                          and cells fished less than 15 minutes")
data_filters$Samples[filter.num] <- dim(dat)[1]
data_filters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#HSU doesn't record depth
#assign average block depth????





#-------------------------------------------------------------------------------
## Raw cpue
avg.cpue.area <- dat %>%
  group_by(year, area, site.x) %>%
  summarise(avg.cpue = mean(cpue))

# Plot the average cpue by year and reef
ggplot(avg.cpue.area, aes(year, avg.cpue, colour = as.factor(area))) +
  geom_line(lwd = 1.05) +
  facet_wrap(~site.x) +
  theme_bw() +
  labs(
    colour = "Area",
    x = "Year",
    y = "Raw average CPUE"
  )
ggsave(paste0(out.dir, "/Average CPUE by year and site.png"),
  width = 6, height = 4,
  units = "in"
)
target_by_gridcell <- dat %>%
   group_by(gridCellID) %>%
   summarise(target = sum(Target))

with(dat, table(year, site.x))
with(dat, table(year, area))

round(with(subset(dat, Target > 0), table(area, site.x)) / with(dat, table(area, site.x)), 2)
round(with(subset(dat, Target > 0), table(site.x)) / with(dat, table(site.x)), 2)

save(dat, data_filters,file = file.path(out.dir,"Filtered_data_CCFRP.RData"))

