# PISCO data clean up and filtering for copper for 2023 assessment
# Based off of Melissa Monk's script PISCO_data_clean_up.R for gopher and black and yellow for 2019 assessment

# Note - sites called PURISIMA_N and PURISIMA_S appear in both UCSC and UCSB site lists - is this right?

#################### Set up: ####################
##### Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

##### Load packages
library(tidyverse)  # includes dplyr, tidyr, ggplot2, stringr
library(here)  # useful for providing directory paths - it sets the base at the folder level (usually - where a RProject or Git repo or other identifiers are)

dir <- file.path(here(),"data","survey_indices","pisco")
setwd(dir)
##### Load data
# Raw transects file
#PISCO = read.csv(here("Data/P","MLPA_fish_biomass_transect_raw.csv"))

# Site info file with lat/long and mpa data
load(here::here("PISCO.sites.RData"))  # PISCO.sites that was saved in workspace image from PISCO_data_clean_up script

##### Set constants
species_code = "SCAU"  # classcode for copper
start_year_UCSC = 2001  # start data for UCSC in 2001 due to inconsistent sampling before that (from Melissa Monk)
start_year_UCSB = 2004  # start data for UCSB in 2004 due to inconsistent sampling before that (from Melissa Monk)

##### Set definitions for regularly sampled sites
# 50% of sampling years
UCSC_half = floor((2021-(start_year_UCSC-1))/2)
UCSB_half = floor((2021-(start_year_UCSB-1))/2)

# 25% of sampling years
UCSC_quarter = floor((2021-(start_year_UCSC-1))/4)
UCSB_quarter = floor((2021-(start_year_UCSB-1))/4)


#################### Process and filter data: ####################
# Filter to keep only bottom transects, remove any data without a year, and start UCSC in 2001 and UCSC in 2004
PISCO.filtered = PISCO %>%
  filter(!is.na(year), level=="BOT") %>%
  filter((year>=start_year_UCSB & campus=='UCSB') | (year>=start_year_UCSC & campus=='UCSC')) 

# Group observations to individual transects, assign each transect an id
PISCO.transect = PISCO.filtered %>%  
  select(campus, method, year, month, day, site, zone,
         level, transect) %>%
  unique() %>%
  mutate(id = row_number())   # adds a row number as a trip identifier

# Merge to add the transect id into the filtered transect dataframe
PISCO.keep = right_join(PISCO.filtered, PISCO.transect)
summary(PISCO.transect$id)  # check to make sure min/max id are the same and it looks like the join worked okay
summary(PISCO.keep$id)

# Get fish by transect, replace NAs with 0s for transects no coppers scene, and select just the coppers
PISCO.cnts  = PISCO.keep %>%
  select(id, classcode, count) %>%  # keep just transect id, species and count
  group_by(id, classcode) %>%       # group by the id and species
  summarise(sum_cnt = sum(count)) %>%  # sum up the number of fish per transect of each species
  spread(classcode, sum_cnt) %>%   # make the data wide format rather than long (each species is column)
  replace(is.na(.), 0) %>%  # replace NA counts with 0 for species not seen
  select(id, all_of(species_code))  # select just the coppers

# Merge back in with PISCO.transect to add back rest of transect info 
PISCO.CPR = inner_join(PISCO.transect, PISCO.cnts)  # PISCO.CPR at this point has been filtered to just BOT, post-2003/2000 for grouped by transect id, just has copper counts with zeros for transects where coppers not seen

# Sites that have seen copper
CPR.pos.site = PISCO.CPR %>%  # just see which sites had CPR and on how many transects they were seen
  filter(SCAU>0) %>%   # get positives and look at which transects ever saw them
  group_by(campus, site) %>%  # group and tally by site - include campus so the two PURISMA_N and _S sites are separated
  tally()

##### Three levels for filtering sites: 
##### 1) all sites that have seen copper at least once
##### 2) sites that were sampled in at least 25% of years and saw copper at least 25% of the time
##### 3) sites that were sampled in at least 50% of years and saw copper at least 25% of the time

### 1) sites that have seen copper at least once - 18033 transects 
PISCO.CPR.all <- PISCO.CPR %>%
filter(site %in% CPR.pos.site$site)  # both UCSC and UCSB PURISIMA_N and PURISIMA_S sites still present - need to cut UCSC PURISIMA_N b/c no copper seen there

# Need to remove UCSC site PURISIMA_N b/c no copper seen there
UCSC_PN_ids = PISCO.CPR %>% filter(site == "PURISIMA_N", campus == "UCSC")  # find the transect ids of the transects to remove
PISCO.CPR.all = PISCO.CPR.all %>%  # cut transect rows associated with UCSC PURISIMA_N site
  filter(!id %in% UCSC_PN_ids$id)

### 2) sites that were sampled at least 25% of the time and saw copper at least 25% of the time (both UCSC PURISIMA_N and _S (sampled 1 year) and UCSB PURISIMA_N and _S (sampled 3 years) get filtered out at this stage)
# Find sites sampled at least 25% of time
PISCO.CPR.transect.25 = PISCO.CPR %>%
  group_by(campus, site) %>%
  summarize(total_years = n_distinct(year))%>%
  filter(ifelse(campus == "UCSB", total_years >= UCSB_quarter, total_years >= UCSC_quarter))  # only keep those that were sampled at least a quarter of the years the relevant campus sampled

# Join this total years sampled per site back with rest of data for only those sites sampled at least 25% of the total sampling years - 22384 transects
PISCO.CPR.25 = inner_join(PISCO.CPR, PISCO.CPR.transect.25)  # keeping total_years in there for now to compare sites in the different filtering methods
CPR.pos.25 = PISCO.CPR.25 %>% filter(SCAU>0)  # 1008 transects

# Of those sites, find sites where copper were seen at least 25% of the years 
PISCO.CPR.pos.transect.25 = CPR.pos.25 %>%
  group_by(campus,site) %>%
  summarize(total_pos = n_distinct(year)) %>%
  filter(ifelse(campus == "UCSB", total_pos >= UCSB_quarter, total_pos >= UCSC_quarter))

# Filter to include just those sites that were "moderate habitat" - 9037 transects
PISCO.CPR.25 = inner_join(PISCO.CPR.25, PISCO.CPR.pos.transect.25)  # keeping total_pos in there for now to compare sites in the different filtering


### 3) sites that were sampled at least 50% of the time and saw copper at least 50% of the time  
PISCO.CPR.transect.50 = PISCO.CPR %>%
  group_by(campus, site) %>%
  summarize(total_years = n_distinct(year)) %>%
  filter(ifelse(campus == "UCSB", total_years >= UCSB_half, total_years >= UCSC_half))  # only keep those that were sampled at least half of the years the relevant campus sampled

# Join this total years sampled per site back with rest of data for only those sites sampled at least 50% of the total sampling years - 18129 transects
PISCO.CPR.50 = inner_join(PISCO.CPR, PISCO.CPR.transect.50)  # total_years is total years sampled, keeping for now to compare sites in different site filtering
CPR.pos.50 = PISCO.CPR.50 %>% filter(SCAU>0)  # see how many of the transects were positive for copper sightings  # 925 transects

# Of those sites, find sites where copper were seen at least 50% of the years (Melissa Monk's "good habitat" from gopher script)
PISCO.CPR.pos.transect.50 = CPR.pos.50 %>%
  group_by(campus,site) %>%
  summarize(total_pos = n_distinct(year)) %>%
  filter(ifelse(campus == "UCSB", total_pos >= UCSB_half, total_pos >= UCSC_half))

# Filter to include just those sites that were "good habitat"
PISCO.CPR.50 = inner_join(PISCO.CPR.50, PISCO.CPR.pos.transect.50)  # 3598 transects


##### Split by campus to build a northern and southern index for the two assessments - for now, keeping the three levels of sampling/habitat filtering
# northern index
PISCO.CPR.all.UCSC = subset(PISCO.CPR.all, campus=="UCSC")
PISCO.CPR.25.UCSC = subset(PISCO.CPR.25, campus=="UCSC")
PISCO.CPR.50.UCSC = subset(PISCO.CPR.50, campus="UCSC")

# southern index
PISCO.CPR.all.UCSB = subset(PISCO.CPR.all, campus=="UCSB")
PISCO.CPR.25.UCSB = subset(PISCO.CPR.25, campus=='UCSB')
PISCO.CPR.50.UCSB = subset(PISCO.CPR.50, campus=="UCSB")

##### Didn't add this in because it doesn't have all of the sites included but here is a version of PISCO.sites with the site names adjusted to have the sides included
# Has lat/long info and MPA info
PISCO.sites.v2 = PISCO.sites %>%
  mutate(old_site_name = site, 
         site1 = paste(old_site_name,"_",side,sep=""),  # paste together site name with side so site names match those in new data set
         site = sub("_$","",site1)) %>%  # remove the trailing _ from sites that don't have sides
  select(-site1)  # remove intermediate site name column


#################### Save output ####################
# Data for northern index
saveRDS(PISCO.CPR.all.UCSC, file=here("Outputs/Copper","PISCO.CPR.all.UCSC.RDS"))  # UCSC sites, all sites that have seen copper at least once regardless of sampling frequency or habitat quality
saveRDS(PISCO.CPR.25.UCSC, file=here("Outputs/Copper","PISCO.CPR.25.UCSC.RDS"))  # UCSC sites, sites that were both sampled and saw copper at least a quarter of UCSC sampling years 
saveRDS(PISCO.CPR.50.UCSC, file=here("Outputs/Copper","PISCO.CPR.50.UCSC.RDS"))  # UCSC sites, sites that were both sampled and saw copper at least half of UCSB sampling years

# Data for southern index
saveRDS(PISCO.CPR.all.UCSB, file=here("Outputs/Copper","PISCO.CPR.all.UCSB.RDS"))  # UCSB sites, all sites that have seen copper at least once regardless of sampling frequency or habitat quality
saveRDS(PISCO.CPR.25.UCSB, file=here("Outputs/Copper","PISCO.CPR.25.UCSB.RDS"))  # UCSB sites, sites that were both sampled and saw copper at least a quarter of UCSB sampling years
saveRDS(PISCO.CPR.50.UCSB, file=here("Outputs/Copper","PISCO.CPR.50.UCSB.RDS"))  # UCSC sites, sites that were both samplied and saw copper at least half of UCSB sampling years

# Data with both combined (for site comparison)
saveRDS(PISCO.CPR.all, file=here("Outputs/Copper","PISCO.CPR.all.RDS"))
saveRDS(PISCO.CPR.25, file=here("Outputs/Copper","PISCO.CPR.25.RDS"))
saveRDS(PISCO.CPR.50, file=here("Outputs/Copper","PISCO.CPR.50.RDS"))

# Site info with site names with side appended (though currently unused)
saveRDS(PISCO.sites.v2, file=here("Outputs/Copper","PISCO.sites.v2.RDS"))  # sites info with site names that include sides to match new data set

# Sites where copper were seen and on how many transects (for comparing with VRG data)
saveRDS(CPR.pos.site, file=here("Outputs/Copper","PISCO.CPR.pos.sites.RDS"))

#################### Data exploration and filtering justification: ####################
##### Copper are primarily caught on the bottom level of the water column
CPR = PISCO %>% filter(classcode == species_code)  # filter from dplyr does essentially the same thing as subset but I think subset can be occasionally less reliable
table(CPR$level)  # copper only really on bottom level (BOT:1537, MID:13)

##### Confirm that we only have subtidal fish sampling methods in this data set
summary(CPR$method)
table(CPR$method)  # looks like the same method (SBTL_FISH) but implemented by different groups?
# SBTL_FISH_CRANE   SBTL_FISH_HSU SBTL_FISH_PISCO   SBTL_FISH_VRG 
# 4              22            1473              51 

##### Keep all zones (though very few in mid, even when all sites that ever saw copper are included)
table((PISCO.CPR.all.UCSC %>% filter(SCAU>0))$zone)
# INMID  INNER  OUTER OUTMID 
# 39     14    289    122 

table((PISCO.CPR.all.UCSB %>% filter(SCAU>0))$zone)
# INMID  INNER    MID  OUTER OUTMID 
# 62     47      1    291    186 

##### Check for missing essential data (general sample, not yet filtered/combined to one row per transect)
summary(as.factor(PISCO$month))  # no NAs
summary(as.factor(PISCO$year))  # no NAs
summary(as.factor(PISCO$day))  # no NAs
summary(as.factor(PISCO$zone))  # no NAs
summary(as.factor(PISCO$site))  # no NAs but 129354 are listed as (Other)
summary(as.factor(PISCO$vis))  # 5027 (Other) and 16023 NAs
summary(as.factor(PISCO$surge))  # lots of NAs (39461)
summary(as.factor(PISCO$depth))  # lots of (Other) (27252) and NAs (12578)

# Check - is there a PURISMA_N and _S in both UCSC and UCSB? Yes! Should check with someone that that is right but will treat it as so for now
UCSB_PN = PISCO %>% filter(campus == "UCSB", site == "PURISIMA_N") %>% filter(classcode == species_code)
UCSB_PS = PISCO %>% filter(campus == "UCSB", site == "PURISIMA_S") %>% filter(classcode == species_code)
UCSC_PN = PISCO %>% filter(campus == "UCSC", site == "PURISIMA_N") %>% filter(classcode == species_code)  # this one sees no copper, will cut it from all sites df
UCSC_PS = PISCO %>% filter(campus == "UCSC", site == "PURISIMA_S") %>% filter(classcode == species_code)


