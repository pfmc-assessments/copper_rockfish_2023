################################################################################
### Process the CDFW CPFV onbard data for an index of abundance
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
modelArea = "south"

#setwd to the north or the south
#set working directory
dir <- file.path(here(), "data", "rec_indices", "crfs_cpfv_onboard", modelArea)
setwd(dir)

#load data for processing
load(file.path(here(), "data", "rec_indices", "crfs_cpfv_onboard", "onboard.RData"))



# Data filter dataframe
filter.num <- 1
dataFilters <- data.frame(matrix(vector(), 10, 4,
                                 dimnames = list(c(), c(
                                   "Filter", "Description", "Samples",
                                   "Positive_Samples"
                                 ))), stringsAsFactors = F)
#-------------------------------------------------------------------------------
onboard <- onboard_data %>% 
  mutate(area = ifelse(district %in% c(1, 2), "south", "north")) %>%
  filter(area == modelArea)


#Get the number of available samples by year
samples_year_district <- onboard %>%
  group_by(year, district) %>%
  tally() %>%
  tidyr::pivot_wider(names_from=district, values_from = n)
View(samples_year_district)
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("All data")
dataFilters$Description[filter.num] <- c("All data")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
#IF Depth isn't available and GIS depth is - add that in
#ONLY using starting depth which is depth1ft, and the gis depths are based on 
#start locations
#remove any remaining drifts with no depth information
onboard <- onboard %>% 
  mutate(depth = ifelse(is.na(depth1ft) & gis90depthft > 0, gis90depthft, depth1ft)) %>%
  filter(!is.na(depth))

#how many positive drifts is depth missing from
pos_data <- onboard %>% filter(number.fish > 0)
summary(pos_data$depth)
summary(as.factor(pos_data$LocationTableError))


#check fish time and observed anglers
summary(onboard$fishtime)
summary(onboard$obsang)

#remove any drifts with a location table error, missing fish time and missing
#number of anglers

onboard <- onboard %>%
  filter(is.na(LocationTableError)) %>%
  filter(!is.na(fishtime)) %>%
  filter(!is.na(obsang)) %>%
  filter(!is.na(reef)) %>%
  filter(fishtime > 0)

#missing reef info
summary(onboard$reef)

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Errors and Missing Data")
dataFilters$Description[filter.num] <- c("Remove drifts with missing data and identified errors")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#Keep ocean waters only
onboard <- onboard %>%
  filter(waterarea %in% c("N", "O"))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Area fished")
dataFilters$Description[filter.num] <- c("Remove drifts in bays and Mexico (if applicable)")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------



#remove Jan-March in the north - no rockfishing these months
#remove Jan-Feb in the south - no rockfishing these months
if(modelArea == "north"){
  onboard <- onboard %>%
    filter(month %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12))
} else {
  onboard <- onboard %>%
    filter(month %in% c(2, 4, 5, 6, 7, 8, 9, 10, 11, 12))
}

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Months fished")
if(modelArea=="north"){
dataFilters$Description[filter.num] <- c("Remove Jan-March; recreational rockfish fishery closed")
} else{
  dataFilters$Description[filter.num] <- c("Remove Jan-Feb; recreational rockfish fishery closed")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish> 0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#positive data
pos_data <- onboard %>% filter(number.fish > 0) %>% filter(depth<500)
#look at depth distribution
ggplot(pos_data, aes(x = depth, fill = district)) +
  geom_histogram() + xlab("Depth (ft)") + ylab("Number of drifts with copper") +
scale_fill_viridis_d()
ggsave(file = file.path(dir, "drifts_by_depth_district.png"), width = 7, height = 7)

depth_quantile <- round(quantile(pos_data$depth, c(0.01, .99)), 0)
depth_quantile
#remove upper and lower 1%
onboard <- onboard %>%
  filter(depth %in% (depth_quantile[1]:depth_quantile[2]))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Depth")
if(modelArea=="north"){
  dataFilters$Description[filter.num] <- c("Remove upper and lower 1% of depth with observed coppers; 
                                           Remaining drifts between 50 and 318 feet")
} else{
  dataFilters$Description[filter.num] <- c("Remove upper and lower 1% of depth with observed coppers; 
                                           Remaining drifts between 48 and 320 feet")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

#look at number of observed anglers
summary(onboard$obsang)
obsang_quantile <- quantile(pos_data$obsang, seq(0, 1, .025))
obsang_quantile

#remove upper and lower 2.5%
#CRFS manual suggests a max number of anglers to observe is 12
onboard <- onboard %>% 
  filter(obsang %in% obsang_quantile[2] : obsang_quantile[40])

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Observed anglers")
if(modelArea=="north"){
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% of observed anglers; 
                                           Remaining drifts with 4-12 observed anglers")
} else{
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% of observed anglers; 
                                           Remaining data: Observed anglers 4-14")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

summary(onboard$fishtime)
fishtime_quantile <- quantile(pos_data$fishtime, seq(0, 1, .025))
fishtime_quantile

#remove upper and lower 2.5%
#Drifts less than 5 minutes probably were not successful
onboard <- onboard %>% 
  filter(fishtime %in% fishtime_quantile[2] : fishtime_quantile[40])


#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Time fished")
if(modelArea=="north"){
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% time fished and 
                                         time fished; Remaining drifts with 3-75 minutes time fished")
} else{
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% time fished and 
                                         time fished; Remaining drifts with 5-102 minutes time fished")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

if(modelArea=="north"){
#subset automatically to 500m
onboard <- onboard %>%
  filter(reef.dist < 500)

#Look at distance from reef for the positives
pos_data <- onboard %>% filter(number.fish > 0)

summary(pos_data$reef.dist)

ggplot(pos_data, aes(x = reef.dist, y = cpue, colour = district)) +
  geom_jitter(alpha = 0.5)

#Cumulative distribution of distance from reef
ggplot(pos_data, aes(reef.dist)) +
  stat_ecdf(geom = "step")


reef.dist_quantile <- quantile(pos_data$reef.dist , seq(0, 1, .01))
reef.dist_quantile
#keep 95% of the data

onboard <- onboard %>%
  filter(reef.dist < reef.dist_quantile[96]) %>%
  droplevels()

onboard <- onboard %>%
  mutate(region = district)
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Distance from rocky substrate")
dataFilters$Description[filter.num] <- c("After removing observations further 
than 0.5km from rocky substrate, keep 95% of the data; drifts within 41.7 m of rocky substrate")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
}

if(modelArea== "south"){
  
#use cdfw block number to filter the southern data
  
#Look at distance from reef for the positives
pos_data <- onboard %>% filter(number.fish > 0)
summary(pos_data$reef.dist)
reef.dist_quantile <- quantile(pos_data$reef.dist, seq(0, 1, .01))

onboard <- onboard %>%
  filter(reef.dist < reef.dist_quantile[96]) %>%
  droplevels()


#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Distance from rocky substrate")
dataFilters$Description[filter.num] <- c("Southern CA rocky substrate incomplete; 
                                         keep 95% of the data; drifts within 4927 m of rocky substrate")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

pos_data <- onboard %>% filter(number.fish > 0)
#look at reef distance
summary(pos_data$reef.dist)
quantile(pos_data$reef.dist, seq(0, 1, .01))
ggplot(pos_data, aes(x = reef.dist, y = cpue, colour = district)) +
  geom_jitter(alpha = 0.5)


#primary target species only
cdfwblockTargets <- onboard %>%
  group_by(cdfw.block) %>%
  summarise(driftsWithTarget = sum(number.fish > 0),
            driftsWOTarget = sum(number.fish == 0)) %>%
  mutate(totaldrifts = driftsWithTarget + driftsWOTarget,
         percentpos = driftsWithTarget / (driftsWithTarget + driftsWOTarget)) %>%
  filter(percentpos > 0.02) %>%
  filter(!cdfw.block == 0)
View(cdfwblockTargets)
#write.csv(driftTargets, 
#          file.path(dir, "driftTargets.csv"),
#          row.names=FALSE)

onboard <- onboard %>%
  filter(cdfw.block %in% cdfwblockTargets$cdfw.block) %>%
  droplevels()
  
#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("CDFW block")
dataFilters$Description[filter.num] <- c("Retain drifts in CDFW blocks where at 
                                         least 2% of all drifts in that block 
                                         caught a copper rockfish")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>% filter(number.fish>0) %>% tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------
}

#look at years
summary(as.factor(onboard$year))


#look at district
summary(as.factor(onboard$district))
#will weight by district

#final tables and visualizations
#Get the number of available samples by year
samples_year_district <- onboard %>%
group_by(year, district) %>%
tally() %>%
tidyr::pivot_wider(names_from=district, values_from = n)
View(samples_year_district)
write.csv(samples_year_district, 
file.path(dir,"samples_by_year_district.csv"),
row.names=FALSE)

#primary target species only
driftTargets <- onboard %>%
  group_by(district) %>%
  summarise(driftsWithTarget = sum(number.fish > 0),
            driftsWOTarget = sum(number.fish == 0)) %>%
  mutate(totaldrifts = driftsWithTarget + driftsWOTarget,
         percentpos = driftsWithTarget / (driftsWithTarget + driftsWOTarget)) 
write.csv(driftTargets, 
          file.path(dir, "driftTargets.csv"),
          row.names=FALSE)


#look at water area by district
#you can see in 2017 that they started fishing outside state waters
waterarea_year <- onboard %>%
  filter(number.fish >0) %>%
  group_by(waterarea, district, year) %>%
  tally() %>%
  pivot_wider(names_from = waterarea, values_from = n) %>%
  mutate(percent_N = N / (N + O))
write.csv(waterarea_year, file.path(dir, "target_water_area.csv"))  


#sample sizes by month
samples_month_year <- onboard %>%
group_by(year, month) %>%
tally() %>%
tidyr::pivot_wider(names_from = month, values_from = n)
samples_month_year



cpue_by_district <- onboard %>%
group_by(year,district) %>%
summarise(average_cpue = mean(cpue))

#look average cpue by district
ggplot(cpue_by_district, aes(x = year, y = average_cpue, colour = district)) +
geom_point(size = 3)  + theme_bw() +
geom_line(aes(x = year, y = average_cpue, colour = district)) +
xlab("Year") + ylab("Average CPUE") + ylim(c(0, (max(cpue_by_district$average_cpue)*1.1))) + 
scale_color_viridis_d()
ggsave(file = file.path(getwd(),modelArea, "average_cpue_by_district.png"), width = 7, height = 7)

#look at 2015 in district 5
aa <- subset(onboard, year==2015 & district == 5)
#one observation


#Assign blocks to areas
if(modelArea == "south"){
 onboard <- onboard %>%
   mutate(region = case_when(
     cdfw.block %in% c(876:879, 860:861, 842:843, 821:822, 801:802, 756:757, 737:740, 718:720 , 681:682, 702:703) ~ "District 1 mainland",
     cdfw.block %in% c(651:658) ~ "District 2 mainland",
     cdfw.block %in% c(888, 870, 867, 849:850, 829, 806:808, 813:815, 768:769, 764:765, 761:762, 744:745, 724) ~ "Southern Channel Islands",
     cdfw.block %in% c(684:691, 707:713) ~ "Northern Channel Islands", 
     TRUE ~ cdfw.block
   ))
 
 #keep only those assigned
blocks_remove <- c(666, 667, 680, 683, 760)

onboard <- onboard %>%
  filter(!region %in% blocks_remove) %>%
  mutate_at(vars(region), as.factor)
summary(onboard$region)
# mainland south 876:879 , 860:861, 842:843, 821:822, 801:802, 756: 757, 737:740, 718:720 , 681:682,
# mainland north 651:658, 
#  southern channel islands 888, 870, 867, 849:850,829, 806:808, 813:815, 768:769, 764:765, 761:762, 744:745: 724 
#  norther channel islands 684:691, 707:713
  
  
cpue_by_region <- onboard %>%
  group_by(year, region) %>%
  summarise(average_cpue = mean(cpue))

#look average cpue by district
ggplot(cpue_by_region, aes(x = year, y = average_cpue, colour = region)) +
  geom_point(size = 3)  + theme_bw() +
  geom_line(aes(x = year, y = average_cpue, colour = region)) +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, (max(cpue_by_region$average_cpue)*1.1))) + 
  scale_color_viridis_d()
ggsave(file = file.path(getwd(),modelArea, "average_cpue_by_region.png"), width = 7, height = 7)
  
}


#save the datafile and filters for the run file
save(onboard, dataFilters, 
file = file.path(dir,"data_for_glm.RData"))
