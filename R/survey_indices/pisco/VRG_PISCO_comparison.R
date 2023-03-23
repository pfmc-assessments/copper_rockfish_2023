# Check for potential to add/incorporate VRG data into PISCO data for SCUBA index for copper and black 2023 assessment

#################### Set up: ####################
##### Remove all variables and turn off open graphics
# rm(list=ls(all=TRUE))
# graphics.off()

##### Load packages
library(tidyverse)  # includes dplyr, tidyr, ggplot2, stringr
library(here)  # useful for providing directory paths - it sets the base at the folder level (usually - where a RProject or Git repo or other identifiers are)

##### Load data
# Raw transects file from PISCO
PISCO = read.csv(here("Data/PISCO","MLPA_fish_biomass_transect_raw.csv"))
# Copper classcode is SCAU

# PISCO site info file with lat/long and mpa data
# load(here::here("Data/PISCO","PISCO.sites.RData"))  # PISCO.sites that was saved in workspace image from PISCO_data_clean_up script

Vantuna_copper = read.csv(here("Data/Vantuna","Sebastes caurinus.csv"))

#################### Compare PISCO and Vantuna data: ####################
########## Site comparison - are there new sites in the Vantuna data we could add into the PISCO data set, since they have the same protocols?

##### Site names can be pretty different so get as far as we can easily in R
# Pull out all sites in PISCO data set (from all transects and fish observations and edit PISCO names so more comparable to Vantuna - 380
PISCO.sites.all = PISCO %>%
  select(site) %>%
  distinct() %>%
  mutate(PISCO_site_name = site,
         PISCO_edited_name = gsub("_"," ",PISCO_site_name)) %>%  # remove the underscores in the PISCO site names
  mutate(PISCO_edited_name = tolower(PISCO_edited_name))  # turn to lowercase

for(i in 1:length(PISCO.sites.all$PISCO_edited_name)) {  # recapitalize the first letter of each word
  PISCO.sites.all$PISCO_edited_name_final[i] = gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", PISCO.sites.all$PISCO_edited_name[i], perl = TRUE)  # code to recapitalize first letter of each word found here: https://statisticsglobe.com/r-capitalize-first-letter-of-character-string-containing-multiple-words
}

# See which Vantuna sites match up with PISCO sites
Vantuna.sites = VRG.sites = Vantuna_copper %>%
  select(Site)

VRG.sites.in.PISCO = Vantuna.sites %>% filter(Site %in% PISCO.sites.all$PISCO_edited_name_final) %>%
  mutate(in_PISCO = "Y")

# Add back in the original PISCO site names
VRG.sites.in.PISCO.R = left_join(VRG.sites.in.PISCO, (PISCO.sites.all %>% dplyr::select(PISCO_site_name, PISCO_edited_name_final)), by=c("Site"="PISCO_edited_name_final"))

# Remaining unmatched VRG sites
VRG.sites.remaining.R = Vantuna.sites %>% filter(!Site %in% PISCO.sites.all$PISCO_edited_name_final)

##### Save output, then do rest of checking manually
saveRDS(VRG.sites.in.PISCO, file=here("Outputs/PISCO VRG site comparison","VRG.sites.in.PISCO.R.RDS"))  # VRG sites matched to PISCO sites in R
write.csv(VRG.sites.remaining.R, file=here("Outputs/PISCO VRG site comparison","VRG.sites.remaining.R.csv"))  # VRG sites not matched to PISCO sites in R


##### Pull out lists of sites from PISCO and Vantuna
# Pull out all sites in PISCO data set (from all transects and fish observations) - 380
PISCO.sites.all = PISCO %>% 
  select(site) %>%
  distinct()

# # Pull out just PISCO sites where copper has been seen at some point - 170
# PISCO.sites.CPR = PISCO %>%
#   filter(classcode == "SCAU") %>%
#   select(site) %>%
#   distinct()
# 
# # Pull out just PISCO sites associated with VRG - 77
# PISCO.sites.VRG = PISCO %>%
#   filter(campus == "VRG") %>%
#   select(site) %>%
#   distinct()
# 
# # Pull out sites in Vantuna copper data (think these are sites where copper has been seen at some point)
# VRG.sites = Vantuna_copper %>%
#   select(Site)

# ##### Edit PISCO sites to remove side part of name, for now, to make it easier to compare to Vantuna sites
# PISCO.sites.all.no.side = PISCO.sites.all %>%
#   mutate(site_no_side = sub("_$","",site)) # this doesn't work, need to figure out how to take off the letters too...
#   
# 
# # Has lat/long info and MPA info
# PISCO.sites.v2 = PISCO.sites %>%
#   mutate(old_site_name = site, 
#          site1 = paste(old_site_name,"_",side,sep=""),  # paste together site name with side so site names match those in new data set
#          site = sub("_$","",site1)) %>%  # remove the trailing _ from sites that don't have sides
#   select(-site1)  # remove intermediate site name column

#################### Write out sites as csv: ####################
# Might be easier to just do this in Excel...
write.csv(PISCO.sites.all, here("Outputs/PISCO VRG site comparison","PISCO_sites_all.csv"))  # all PISCO sites
# write.csv(PISCO.sites.CPR, here("Outputs/PISCO VRG site comparison","PISCO_sites_CPR.csv"))  # PISCO sites where copper was seen
# write.csv(PISCO.sites.VRG, here("Outputs/PISCO VRG site comparison","PISCO_sites_VRG.csv"))  # PISCO sites associated with VRG campus
# write.csv(VRG.sites, here("Outputs/PISCO VRG site comparison","VRG_sites.csv"))  # VRG sites where copper was seen


#################### Rejoin R-matched and manual-matched sites for full list: ####################
# Load sites matched up to PISCO sites manually
VRG.sites.manual = read.csv(here("Outputs/PISCO VRG site comparison","VRG.sites.remaining.R.filtering.csv"))  

# Change column names to match between manual and R-matched files
VRG.sites.manual = rename(VRG.sites.manual, in_PISCO = In_PISCO) %>%
  select(-X)
VRG.sites.in.PISCO.R = rename(VRG.sites.in.PISCO.R, PISCO_site_name1 = PISCO_site_name)
VRG.sites.in.PISCO.R = VRG.sites.in.PISCO.R %>%
  mutate(PISCO_site_name2 = NA, 
         PISCO_site_name3 = NA)

# Join together
VRG.PISCO.sites.matchup = rbind(VRG.sites.in.PISCO.R, VRG.sites.manual)

# Save as RDS and csv
write.csv(VRG.PISCO.sites.matchup, here("Outputs/PISCO VRG site comparison","VRG.PISCO.sites.matchup.csv"))  # VRG with attempted matched PISCO sites
saveRDS(VRG.PISCO.sites.matchup, file=here("Outputs/PISCO VRG site comparison","VRG.PISCO.sites.matchup.RDS"))  # saving it as RDS too in case csv gets changed and we want to see the original

