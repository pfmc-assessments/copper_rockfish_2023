# PISCO data clean up and filtering for copper for 2023 assessment
# Based off of Melissa Monk's script PISCO_data_clean_up.R for gopher and black and yellow for 2019 assessment

# Note - sites called PURISIMA_N and PURISIMA_S appear in both UCSC and UCSB site lists - is this right?

#################### Set up: ####################
##### Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

##### Load packages
#curl::ie_proxy_info()
library(tidyverse)  # includes dplyr, tidyr, ggplot2, stringr
library(here)  # useful for providing directory paths - it sets the base at the folder level (usually - where a RProject or Git repo or other identifiers are)

##### Load data
# Raw transects file
PISCO = read.csv(here("Data/PISCO","MLPA_fish_biomass_transect_raw.csv"))

# # Site info file with lat/long and mpa data
#load(here::here("Data/PISCO","PISCO.sites.RData"))  # PISCO.sites that was saved in workspace image from PISCO_data_clean_up script
PISCO.sites = readRDS(file=here("Outputs/Copper","PISCO.sites.v2.RDS"))  # sites info with site names that include sides to match new data set

##### Set constants
species_code = "SCAU"  # classcode for copper
start_year_UCSC = 2001  # start data for UCSC in 2001 due to inconsistent sampling before that (from Melissa Monk)
start_year_UCSB = 2004  # start data for UCSB in 2004 due to inconsistent sampling before that (from Melissa Monk)

##### Set definitions for regularly sampled sites
# # 50% of sampling years
# UCSC_half = floor((2021-(start_year_UCSC-1))/2)
# UCSB_half = floor((2021-(start_year_UCSB-1))/2)
# 
# # 25% of sampling years
# UCSC_quarter = floor((2021-(start_year_UCSC-1))/4)
# UCSB_quarter = floor((2021-(start_year_UCSB-1))/4)

# 80% of sampling years
UCSC_80 = floor((2021-(start_year_UCSC-1))*.8)
UCSB_80 = floor((2021-(start_year_UCSB-1))*.8)
VRG_80 = floor((2021-2003)*.8)  # starts same year as UCSB, though skips 2006

# Size threshold for 1+
size_1_plus = 17 

# Visibility threshold
vis_thresh = 3

# Zones to exclude
zones_to_exclude = c("INNER","MID")
#zones_to_keep = c("OUTER","OUTMID")
#zones_to_keep_S = c("OUTER","OUTMID","DEEP")

#################### Process and filter data: ####################
# Filtering that needs to be done:
## Only keep sites that were sampled regularly 
## Only keep sites that saw copper at least once
# Keep only one transect per zone/site/etc. per day -- not doing this after email with JW
## Filter out transects with visibility < 3m 
## Pull out just fish above the 1+ size threshold
# See it shakes out with MPAs - not want to conflate with a management change when MPAs went in in 2007

##### Select sites that were both sampled regularly and saw copper at least once
# Find sites that were sampled regularly - defining this as sites sampled 80% of the relevant campus sampling years (after regular sampling started in 2001 for UCSC, 2004 for UCSB)
regular.sites.UCs = PISCO %>%
  filter((year>=start_year_UCSB & campus=='UCSB') | (year>=start_year_UCSC & campus=='UCSC') ) %>%
  group_by(campus, site) %>%
  summarize(total_years = n_distinct(year)) %>%
  filter(ifelse(campus == "UCSB", total_years >= UCSB_80, total_years >= UCSC_80))  # only keep those that were sampled at least 80% of the years the relevant campus sampled

regular.sites.VRG = PISCO %>%
  filter(campus == "VRG") %>%
  group_by(campus, site) %>%
  summarize(total_years = n_distinct(year)) %>%
  filter(total_years >= VRG_80)

# Check if any of the regular VRG sites are also in the UCSB sites -- none show up, looks like these are distinct sites
regular.sites.VRG %>% filter(site %in% regular.sites.UCs$site)

# Join UCSC,UCSB and VRG regular sites together to make one df
regular.sites = rbind(regular.sites.UCs, regular.sites.VRG)

# Find sites that have seen copper at least once (once regular sampling started)
CPR.sites = PISCO %>%
  filter((year>=start_year_UCSB & campus=='UCSB') | (year>=start_year_UCSC & campus=='UCSC') | campus == "VRG") %>%
  filter(classcode == species_code) %>%
  select(campus, site) %>%
  group_by(campus, site) %>%
  unique()

# Filter to sites that are both sampled regularly and have seen copper
site.list = PISCO %>%
  filter(site %in% regular.sites$site & site %in% CPR.sites$site) %>%
  select(campus, site) %>%
  unique()

##### Do some filtering
# Filter to keep only bottom transects, remove any data without a year, start UCSC in 2001 and UCSC in 2004
PISCO.filtered = PISCO %>%
  filter(!is.na(year), level=="BOT") %>%
  filter((year>=start_year_UCSB & campus=='UCSB') | (year>=start_year_UCSC & campus=='UCSC') | campus == "VRG") 

# Remove transects with visibility <3, only include sites in the regularly-sampled and copper-positive set
PISCO.filtered = PISCO.filtered %>%
  filter(vis >= vis_thresh | is.na(vis)) %>%
  filter(site %in% site.list$site)

# Group observations to individual transects, assign each transect an id - 9173 w/UCSC+UCSB; 10133 with UCSC+UCSB+VRG too 
PISCO.transect = PISCO.filtered %>%  
  select(campus, method, year, month, day, site, zone,
         level, transect) %>%
  unique() %>%
  mutate(id = row_number())   # adds a row number as a trip identifier

# Merge to add the transect id into the filtered transect dataframe
PISCO.keep = right_join(PISCO.filtered, PISCO.transect)
summary(PISCO.transect$id)  # check to make sure min/max id are the same and it looks like the join worked okay
summary(PISCO.keep$id)

# Label YOY and 1+
PISCO.keep = PISCO.keep %>%
  mutate(one_plus = if_else(fish_tl < size_1_plus, 0, 1))

##### Find counts for all copper, 1+ and YOY for each transect
# Find all the fish counts - 10133, same as length of PISCO.transect
PISCO.cnts.all  = PISCO.keep %>%  
  select(id, classcode, count) %>%  # keep just transect id, species and count
  group_by(id, classcode) %>%       # group by the id and species
  summarise(sum_cnt = sum(count)) %>%  # sum up the number of fish per transect of each species
  spread(classcode, sum_cnt) %>%   # make the data wide format rather than long (each species is column)
  replace(is.na(.), 0) %>%  # replace NA counts with 0 for species not seen
  select(id, all_of(species_code))  # select just the coppers

PISCO.cnts.all = rename(PISCO.cnts.all, SCAU_all = SCAU)

# Now find all 1+ fish
PISCO.cnts.1plus  = PISCO.keep %>%  # 9313
  filter(one_plus == 1) %>%
  select(id, classcode, count) %>%  # keep just transect id, species and count
  group_by(id, classcode) %>%       # group by the id and species
  summarise(sum_cnt = sum(count)) %>%  # sum up the number of fish per transect of each species
  spread(classcode, sum_cnt) %>%   # make the data wide format rather than long (each species is column)
  replace(is.na(.), 0) %>%  # replace NA counts with 0 for species not seen
  select(id, all_of(species_code))  # select just the coppers

PISCO.cnts.1plus = rename(PISCO.cnts.1plus, SCAU_1plus = SCAU)

# Now find all YOY 
PISCO.cnts.YOY  = PISCO.keep %>%  # 9395
  filter(one_plus == 0) %>%
  select(id, classcode, count) %>%  # keep just transect id, species and count
  group_by(id, classcode) %>%       # group by the id and species
  summarise(sum_cnt = sum(count)) %>%  # sum up the number of fish per transect of each species
  spread(classcode, sum_cnt) %>%   # make the data wide format rather than long (each species is column)
  replace(is.na(.), 0) %>%  # replace NA counts with 0 for species not seen
  select(id, all_of(species_code))  # select just the coppers

PISCO.cnts.YOY = rename(PISCO.cnts.YOY, SCAU_YOY = SCAU)

# Now join all together
PISCO.cnts = left_join(PISCO.cnts.all, PISCO.cnts.1plus, by = "id")
PISCO.cnts = left_join(PISCO.cnts, PISCO.cnts.YOY, by = "id")
PISCO.cnts = PISCO.cnts %>% replace(is.na(.), 0)  # replace NAs with 0s - those were just transects that didn't have YOY or 1+ of any species in the original df 

# ###### Existing code
# ##### These have different numbers of transects that get included, which doesn't seem right (though makes sense how it happens...). Need to figure out a better solution
# # For 1+, get fish by transect, replace NAs with 0s for transects where no coppers seen, select just the coppers
# PISCO.cnts.1plus  = PISCO.keep %>%  # 8840       old: 24550 - jumps to 43814 if group_by one_plus too
#   filter(one_plus == 1) %>%
#   select(id, classcode, count) %>%  # keep just transect id, species and count
#   group_by(id, classcode) %>%       # group by the id and species
#   summarise(sum_cnt = sum(count)) %>%  # sum up the number of fish per transect of each species
#   spread(classcode, sum_cnt) %>%   # make the data wide format rather than long (each species is column)
#   replace(is.na(.), 0) %>%  # replace NA counts with 0 for species not seen
#   select(id, all_of(species_code))  # select just the coppers
# 
# # For YOY, get fish by transect, replace NAs with 0s for transects where no coppers seen, select just the coppers
# PISCO.cnts.YOY  = PISCO.keep %>%  # 24550 - jumps to 43814 if group_by one_plus too
#   filter(one_plus == 0) %>%
#   select(id, classcode, count) %>%  # keep just transect id, species and count
#   group_by(id, classcode) %>%       # group by the id and species
#   summarise(sum_cnt = sum(count)) %>%  # sum up the number of fish per transect of each species
#   spread(classcode, sum_cnt) %>%   # make the data wide format rather than long (each species is column)
#   replace(is.na(.), 0) %>%  # replace NA counts with 0 for species not seen
#   select(id, all_of(species_code))  # select just the coppers
# 
# # Merge back in with PISCO.transect to add back rest of transect info 
# PISCO.CPR.1plus = inner_join(PISCO.transect, PISCO.cnts.1plus)  # 8440
# PISCO.CPR.YOY = inner_join(PISCO.transect, PISCO.cnts.YOY)  # 8511

# Merge back in with PISCO.transect to add back in rest of transect info
PISCO.CPR = inner_join(PISCO.transect, PISCO.cnts)  # 10133, so same as length of PISCO.transects
CPR.pos = PISCO.CPR %>% filter(SCAU_all>0)

##### Check if MPA status is uneven in sites
PISCO.MPA.info = left_join(PISCO.CPR, PISCO.sites, by = "site")

PISCO.MPA.N = PISCO.MPA.info %>% 
  filter(campus == "UCSC") %>%
  group_by(site, RESERVE) %>%
  tally()
# All but three sites (MALPASO, PESCADERO_UC, PESCADERO_DC) in MPAs

N_non_MPA_sites = c("MALPASO", "PESCADERO_UC", "PESCADERO_DC")

PISCO.MPA.S = PISCO.MPA.info %>% 
  filter(campus %in% c("UCSB","VRG")) %>%
  group_by(site, RESERVE) %>%
  tally()
# More even split In/Out of MPAs

saveRDS(PISCO.MPA.N, file=here("Outputs/Copper","PISCO.MPA.N.RDS"))
saveRDS(PISCO.MPA.S, file=here("Outputs/Copper","PISCO.MPA.S.RDS"))

##### Split by campus
### Northern index - 3461
PISCO.CPR.1plus.N = PISCO.CPR %>% 
  filter(campus == "UCSC") %>% 
  select(-SCAU_all, -SCAU_YOY)
PISCO.CPR.1plus.N = rename(PISCO.CPR.1plus.N, SCAU = SCAU_1plus)  # rename so works with index code

# Filter to exclude INNER/MID zones
PISCO.CPR.1plus.N = PISCO.CPR.1plus.N %>%  # 2607 after filtering zones
  filter(!zone %in% zones_to_exclude)

# One without non-MPA sites for comparison
PISCO.CPR.1plus.N.MPAs = PISCO.CPR.1plus.N %>%
  filter(!site %in% N_non_MPA_sites)

# YOY - haven't made any zone decisions for YOY yet
PISCO.CPR.YOY.N = PISCO.CPR %>%  # 3461
  filter(campus == "UCSC") %>%  
  select(-SCAU_all, -SCAU_1plus)

  
### Southern index - 6672
PISCO.CPR.1plus.S = PISCO.CPR %>% 
  filter(campus %in% c("UCSB", "VRG")) %>%
  select(-SCAU_all, -SCAU_YOY)
PISCO.CPR.1plus.S = rename(PISCO.CPR.1plus.S, SCAU = SCAU_1plus)  # rename so works with index code

# Filter to exclude INNER/MID zones
PISCO.CPR.1plus.S = PISCO.CPR.1plus.S %>%  # 4344 after filtering for zones
  filter(!zone %in% zones_to_exclude)

# YOY - haven't made any decisions for YOY yet
PISCO.CPR.YOY.S = PISCO.CPR %>%
  filter(campus %in% c("UCSB","VRG")) %>%
  select(-SCAU_all, -SCAU_1plus)

##### Save outputs
# Data for northern index
saveRDS(PISCO.CPR.1plus.N, file=here("Outputs/Copper","PISCO.CPR.1plus.N.RDS"))  # UCSC sites, sites that have seen copper at least once and were sampled regularly, filtered to only have 1+ fish
saveRDS(PISCO.CPR.1plus.N.MPAs, file=here("Outputs/Copper","PISCO.CPR.1plus.N.MPAs.RDS"))  # only sites that became MPAs
saveRDS(PISCO.CPR.YOY.N, file=here("Outputs/Copper","PISCO.CPR.YOY.N.RDS"))

# Data for southern index
saveRDS(PISCO.CPR.1plus.S, file=here("Outputs/Copper","PISCO.CPR.1plus.S.RDS"))  # UCSB and VRG sites, sites that have seen copper at least once and were sampled regularly, filtered to only have 1+ fish
saveRDS(PISCO.CPR.YOY.S, file=here("Outputs/Copper","PISCO.CPR.YOY.S.RDS"))


#################### Data exploration and filtering justification: ####################
##### Copper are primarily caught on the bottom level of the water column
CPR = PISCO %>% filter(classcode == species_code)  # filter from dplyr does essentially the same thing as subset but I think subset can be occasionally less reliable
table(CPR$level)  # copper only really on bottom level (BOT:1537, MID:13)

##### Confirm that we only have subtidal fish sampling methods in this data set
summary(CPR$method)
table(CPR$method)  # looks like the same method (SBTL_FISH) but implemented by different groups?
# SBTL_FISH_CRANE   SBTL_FISH_HSU SBTL_FISH_PISCO   SBTL_FISH_VRG 
# 4              22            1473              51 

########## Investigate zones
### What percent of transects that could have seen copper are done in each zone?
zones_N = data.frame(table((PISCO.CPR %>% filter(campus == "UCSC"))$zone)) %>% 
  mutate(perc_transects = Freq/length((PISCO.CPR %>% filter(campus == "UCSC"))$zone), age = "sample")
# Pretty even distribution in north: 
# Zone Freq perc_transects
# INMID 865 0.2499278
# INNER 854 0.2467495
# OUTER 867 0.2505056
# OUTMID 875 0.2528171

zones_S = data.frame(table((PISCO.CPR %>% filter(campus %in% c("UCSB","VRG")))$zone)) %>% 
  mutate(perc_transects = Freq/length((PISCO.CPR %>% filter(campus %in% c("UCSB","VRG")))$zone), age = "sample")
# Zone Freq perc_transects
# DEEP 211 0.03162470
# INMID 1023 0.15332734
# INNER 2070 0.31025180
# MID 258 0.03866906
# OUTER 2074 0.31085132
# OUTMID 1036 0.15527578

### What percent of transects in each zone see copper?
# North
# 1plus 
zones_N_1_plus_CPR = data.frame(table((CPR.pos %>% filter(campus == "UCSC") %>% filter(SCAU_1plus>0))$zone))
zones_N_1_plus_CPR = rename(zones_N_1_plus_CPR, Freq_CPR=Freq)
zones_N_1_plus_CPR = left_join(zones_N_1_plus_CPR, zones_N %>% select(Var1,Freq), by="Var1") %>%
  mutate(perc_sample_transects_pos = Freq_CPR/Freq, age = "1+")

# YOY
zones_N_YOY_CPR = data.frame(table((CPR.pos %>% filter(campus == "UCSC") %>% filter(SCAU_YOY>0))$zone))
zones_N_YOY_CPR = rename(zones_N_YOY_CPR, Freq_CPR=Freq)
zones_N_YOY_CPR = left_join(zones_N_YOY_CPR, zones_N %>% select(Var1,Freq), by="Var1") %>%
  mutate(perc_sample_transects_pos = Freq_CPR/Freq, age = "YOY")

# All CPR
zones_N_all_CPR = data.frame(table((CPR.pos %>% filter(campus == "UCSC") %>% filter(SCAU_all>0))$zone))
zones_N_all_CPR = rename(zones_N_all_CPR, Freq_CPR=Freq)
zones_N_all_CPR = left_join(zones_N_all_CPR, zones_N %>% select(Var1,Freq), by="Var1") %>%
  mutate(perc_sample_transects_pos = Freq_CPR/Freq, age = "all")

# Df for plot
zones_N_plot_df = rbind(zones_N %>% select(Var1, Freq, age),
                        zones_N_1_plus_CPR %>% rename(Freq=Freq_CPR, Freq_sample=Freq) %>% select(Var1, Freq, age),
                        zones_N_YOY_CPR %>% rename(Freq=Freq_CPR, Freq_sample=Freq) %>% select(Var1, Freq, age),
                        zones_N_all_CPR %>% rename(Freq=Freq_CPR, Freq_sampe=Freq) %>% select(Var1, Freq, age))
zones_N_perc_pos_plot_df = rbind(zones_N_1_plus_CPR, zones_N_YOY_CPR, zones_N_all_CPR)

# Quick plot
transects_zone_plot_N = ggplot(data = zones_N_plot_df, aes(x=Var1, y=Freq, color=age, fill=age)) +
  geom_bar(stat="identity", position="dodge", alpha=0.75) +
  xlab("Zone") + ylab("# transects") + ggtitle("Sample transects and positive for copper - N") +
  annotate(geom = "text", x = "INMID", y = 750,
           label = "INMID:\nYOY = 0.6%\n1+ = 2%\nall = 2.5%") +
  annotate(geom = "text", x = "INNER", y = 500,
           label = "INNER:\nYOY = 0.5%\n1+ = 0%\nall = 0.5%") +
  annotate(geom = "text", x = "OUTER", y =750,
           label = "OUTER:\nYOY = 4%\n1+ = 16%\nall = 19%") +
  annotate(geom = "text", x = "OUTMID", y = 500,
           label = "OUTMID:\nYOY = 2%\n1+ = 5%\nall = 7%") +
  theme_bw()
  
ggsave(transects_zone_plot_N, file=here("Outputs/Copper/Zone_decisions","transects_zone_plot_N.png"), height=8, width=8)

# Plot with just positive transect percents
transects_zone_perc_pos_plot_N = ggplot(data = zones_N_perc_pos_plot_df, aes(x=Var1, y=perc_sample_transects_pos*100, color=age, fill=age)) +
  geom_bar(stat="identity",position="dodge") +
  xlab("Zone") + ylab("% transects positive") + ggtitle("Percent transects positive for copper - N") +
  theme_bw()

ggsave(transects_zone_perc_pos_plot_N, file = here("Outputs/Copper/Zone_decisions","transects_zone_perc_pos_plot_N.png"), height=8, width=8)

# South
zones_S_1_plus_CPR = data.frame(table((CPR.pos %>% filter(campus %in% c("UCSB","VRG")) %>% filter(SCAU_1plus>0))$zone))
zones_S_1_plus_CPR = rename(zones_S_1_plus_CPR, Freq_CPR=Freq)
zones_S_1_plus_CPR = left_join(zones_S_1_plus_CPR, zones_S %>% select(Var1,Freq), by="Var1") %>%
  mutate(perc_sample_transects_pos = Freq_CPR/Freq, age = "1+")

# YOY
zones_S_YOY_CPR = data.frame(table((CPR.pos %>% filter(campus %in% c("UCSB","VRG")) %>% filter(SCAU_YOY>0))$zone))
zones_S_YOY_CPR = rename(zones_S_YOY_CPR, Freq_CPR=Freq)
zones_S_YOY_CPR = left_join(zones_S_YOY_CPR, zones_S %>% select(Var1,Freq), by="Var1") %>%
  mutate(perc_sample_transects_pos = Freq_CPR/Freq, age = "YOY")

# All CPR
zones_S_all_CPR = data.frame(table((CPR.pos %>% filter(campus %in% c("UCSB","VRG")) %>% filter(SCAU_all>0))$zone))
zones_S_all_CPR = rename(zones_S_all_CPR, Freq_CPR=Freq)
zones_S_all_CPR = left_join(zones_S_all_CPR, zones_S %>% select(Var1,Freq), by="Var1") %>%
  mutate(perc_sample_transects_pos = Freq_CPR/Freq, age = "all")

# Df for plot
zones_S_plot_df = rbind(zones_S %>% select(Var1, Freq, age),
                        zones_S_1_plus_CPR %>% rename(Freq=Freq_CPR, Freq_sample=Freq) %>% select(Var1, Freq, age),
                        zones_S_YOY_CPR %>% rename(Freq=Freq_CPR, Freq_sample=Freq) %>% select(Var1, Freq, age),
                        zones_S_all_CPR %>% rename(Freq=Freq_CPR, Freq_sampe=Freq) %>% select(Var1, Freq, age))
zones_S_perc_pos_plot_df = rbind(zones_S_1_plus_CPR, zones_S_YOY_CPR, zones_S_all_CPR)

# Quick plot
transects_zone_plot_S = ggplot(data = zones_S_plot_df, aes(x=Var1, y=Freq, color=age, fill=age)) +
  geom_bar(stat="identity", position="dodge", alpha=0.75) +
  xlab("Zone") + ylab("# transects") + ggtitle("Sample transects and positive for copper - S") +
  annotate(geom = "text", x = "DEEP", y = 500,
           label = "DEEP:\nYOY = 4%\n1+ = 11%\nall = 13%") +
  annotate(geom = "text", x = "INMID", y = 1300,
           label = "INMID:\nYOY = 0.4%\n1+ = 4%\nall = 4%") +
  annotate(geom = "text", x = "INNER", y = 2000,
           label = "INNER:\nYOY = 0.5%\n1+ = 2%\nall = 2%") +
  annotate(geom = "text", x = "MID", y = 500,
           label = "MID:\nYOY = 0.4%\n1+ = 0.8%\nall = 0.8%") +
  annotate(geom = "text", x = "OUTER", y =750,
           label = "OUTER:\nYOY = 3%\n1+ = 8%\nall = 10%") +
  annotate(geom = "text", x = "OUTMID", y = 1500,
           label = "OUTMID:\nYOY = 2%\n1+ = 11%\nall = 12%") +
  theme_bw()

ggsave(transects_zone_plot_S, file=here("Outputs/Copper/Zone_decisions","transects_zone_plot_S.png"), height=8, width=8)

# Plot with just positive transect percents
transects_zone_perc_pos_plot_S = ggplot(data = zones_S_perc_pos_plot_df, aes(x=Var1, y=perc_sample_transects_pos*100, color=age, fill=age)) +
  geom_bar(stat="identity",position="dodge") +
  xlab("Zone") + ylab("% transects positive") + ggtitle("Percent transects positive for copper - S") +
  theme_bw()

ggsave(transects_zone_perc_pos_plot_S, file = here("Outputs/Copper/Zone_decisions","transects_zone_perc_pos_plot_S.png"), height=8, width=8)



########## Check out size distributions and effects of zone 
##### Full size distribution (some of this size distribution prep is also in PISCO_data_exploration_copper.R)
# Filter to just copper
PISCO.CPR.size = PISCO %>%
  filter(classcode == species_code)

# Make one row per fish to do size distributions
table(PISCO.CPR.size$count)  # max 5 fish per row
# 1    2    3    4    5 
# 1462   69   15    2    2 

# Duplicate rows the number of times of the fish in the count to get a df with one fish per row
PISCO.CPR.fish.all = rbind(PISCO.CPR.size %>% filter(count == 1), 
                       PISCO.CPR.size %>% filter(count == 2), PISCO.CPR.size %>% filter(count == 2),
                       PISCO.CPR.size %>% filter(count == 3), PISCO.CPR.size %>% filter(count == 3), PISCO.CPR.size %>% filter(count == 3),
                       PISCO.CPR.size %>% filter(count == 4), PISCO.CPR.size %>% filter(count == 4), 
                       PISCO.CPR.size %>% filter(count == 4), PISCO.CPR.size %>% filter(count == 4),
                       PISCO.CPR.size %>% filter(count == 5), PISCO.CPR.size %>% filter(count == 5), PISCO.CPR.size %>% filter(count == 5),
                       PISCO.CPR.size %>% filter(count == 5), PISCO.CPR.size %>% filter(count == 5))

# Size distribution plot of all coppers
copper_size = ggplot(data=PISCO.CPR.fish.all, aes(fish_tl)) +
  geom_density() +
  xlab("Fish tl (cm)") + ylab("Density") + ggtitle("Size distribution - all coppers in PISCO data") +
  theme_bw()

ggsave(copper_size, file=here("Outputs/Copper/Size_distributions","copper_size.png"), height=8, width=8)  

##### Filtered coppers, including just OUTER/OUTMID zone
### Filter down the same way as above: just BOT level, start UCSC and UCSB at 2001/2004, remove visibility <3m, only include sites regularly sampled and that had copper at some point
PISCO.CPR.size.filtered = PISCO.CPR.size %>%
  filter(!is.na(year), level=="BOT") %>%
  filter((year>=start_year_UCSB & campus=='UCSB') | (year>=start_year_UCSC & campus=='UCSC') | campus == "VRG") %>%
  filter(vis >= vis_thresh | is.na(vis)) %>%
  filter(site %in% site.list$site) %>%
  filter(zone %in% zones_to_keep)

# Table of counts to check how to expand into rows - counts up to 4
table(PISCO.CPR.size.filtered$count)
# 1   2   3   4 
# 731  30   6   1 

# Duplicate rows the number of times of the fish in the count to get a df with one fish per row
PISCO.CPR.fish.filtered = rbind(PISCO.CPR.size.filtered %>% filter(count == 1), 
                           PISCO.CPR.size.filtered %>% filter(count == 2), PISCO.CPR.size.filtered %>% filter(count == 2),
                           PISCO.CPR.size.filtered %>% filter(count == 3), PISCO.CPR.size.filtered %>% filter(count == 3), PISCO.CPR.size.filtered %>% filter(count == 3),
                           PISCO.CPR.size.filtered %>% filter(count == 4), PISCO.CPR.size.filtered %>% filter(count == 4), 
                           PISCO.CPR.size.filtered %>% filter(count == 4), PISCO.CPR.size.filtered %>% filter(count == 4))

copper_size_filtered = ggplot(data=PISCO.CPR.fish.filtered, aes(fish_tl)) +
  geom_density() +
  xlab("Fish tl (cm)") + ylab("Density") + ggtitle("Size distribution - filtered coppers in PISCO data") +
  theme_bw()

ggsave(copper_size_filtered, file=here("Outputs/Copper/Size_distributions","copper_size_filtered.png"), height=8, width=8)  

# What about if separate YOY from 1+
# 1+
PISCO.CPR.fish.filtered.1plus = PISCO.CPR.fish.filtered %>%  # 650 1+ (this checks out with the total number of 1+ when we filter for the index, so that's good)
  filter(fish_tl >= size_1_plus)

copper_size_filtered_1plus =  ggplot(data=PISCO.CPR.fish.filtered.1plus, aes(fish_tl)) +
  geom_density() +
  xlab("Fish tl (cm)") + ylab("Density") + ggtitle("Size distribution - filtered 1+ coppers in PISCO data") +
  theme_bw()

ggsave(copper_size_filtered_1plus, file=here("Outputs/Copper/Size_distributions","copper_size_filtered_1plus.png"), height=8, width=8)  

# YOY
PISCO.CPR.fish.filtered.YOY = PISCO.CPR.fish.filtered %>%  # 650 1+ (this checks out with the total number of 1+ when we filter for the index, so that's good)
  filter(fish_tl < size_1_plus)

copper_size_filtered_YOY =  ggplot(data=PISCO.CPR.fish.filtered.YOY, aes(fish_tl)) +
  geom_density() +
  xlab("Fish tl (cm)") + ylab("Density") + ggtitle("Size distribution - filtered YOY coppers in PISCO data") +
  theme_bw()

# What about separating north from south?
# North
PISCO.CPR.fish.filtered.N = PISCO.CPR.fish.filtered %>%
  filter(campus == "UCSC")

copper_size_filtered_N =  ggplot(data=PISCO.CPR.fish.filtered.N, aes(fish_tl)) +
  geom_density() +
  xlab("Fish tl (cm)") + ylab("Density") + ggtitle("Size distribution - filtered N coppers in PISCO data") +
  theme_bw()

ggsave(copper_size_filtered_N, file=here("Outputs/Copper/Size_distributions","copper_size_filtered_N.png"), height=8, width=8)

# South
PISCO.CPR.fish.filtered.S = PISCO.CPR.fish.filtered %>%
  filter(campus %in% c("UCSB","VRG"))

copper_size_filtered_S =  ggplot(data=PISCO.CPR.fish.filtered.S, aes(fish_tl)) +
  geom_density() +
  xlab("Fish tl (cm)") + ylab("Density") + ggtitle("Size distribution - filtered S coppers in PISCO data") +
  theme_bw()

ggsave(copper_size_filtered_S, file=here("Outputs/Copper/Size_distributions","copper_size_filtered_S.png"), height=8, width=8)


########## Check out size distributions through time to see if MPA effect
# Use PISCO.CPR.fish.filtered.N and .S for now - could also try less filtered data to compare
# Updating to use new filters (changed zones included) but keeping the code above in case want to reference that just keeping OUTER/OUTMID didn't change much

##### Filtered coppers, excluding INNER/MID, also filter for 1+ size
### Filter down the same way as above: just BOT level, start UCSC and UCSB at 2001/2004, remove visibility <3m, only include sites regularly sampled and that had copper at some point
PISCO.CPR.size.filtered.1plus = PISCO.CPR.size %>%
  filter(!is.na(year), level=="BOT") %>%
  filter((year>=start_year_UCSB & campus=='UCSB') | (year>=start_year_UCSC & campus=='UCSC') | campus == "VRG") %>%
  filter(vis >= vis_thresh | is.na(vis)) %>%
  filter(site %in% site.list$site) %>%
  filter(!zone %in% zones_to_exclude) %>% 
  filter(fish_tl >= size_1_plus)

# Table of counts to check how to expand into rows - counts up to 4
table(PISCO.CPR.size.filtered.1plus$count)
# 1   2   3   
# 681  31   6    

# Duplicate rows the number of times of the fish in the count to get a df with one fish per row
PISCO.CPR.fish.filtered.1plus = rbind(PISCO.CPR.size.filtered.1plus %>% filter(count == 1), 
                                PISCO.CPR.size.filtered.1plus %>% filter(count == 2), PISCO.CPR.size.filtered.1plus %>% filter(count == 2),
                                PISCO.CPR.size.filtered.1plus %>% filter(count == 3), PISCO.CPR.size.filtered.1plus %>% filter(count == 3), PISCO.CPR.size.filtered.1plus %>% filter(count == 3))

# North
PISCO.CPR.fish.filtered.1plus.N = PISCO.CPR.fish.filtered.1plus %>% filter(campus == "UCSC")
PISCO.CPR.fish.filtered.1plus.N$year = as.factor(PISCO.CPR.fish.filtered.1plus.N$year)  # make year a factor (so it works as color in plot)

copper_size_tally_N = PISCO.CPR.fish.filtered.1plus.N %>% group_by(year) %>% tally()

fish_per_year_threshold = 15
years_with_enough_fish_N = copper_size_tally_N %>%
  filter(n >= fish_per_year_threshold)

# All years
copper_size_through_time_1plus_N = ggplot(data = PISCO.CPR.fish.filtered.1plus.N, aes(fish_tl)) +
  geom_density() +
  geom_label(aes(label=paste("n= ",n)), x = 20, y = 0.075, vjust=0, hjust=0, data=copper_size_tally_N) +
  xlab("Fish total length (cm)") + ylab("Density") + ggtitle("Size of copper by year: 1+, north") +
  theme_bw() +
  facet_wrap(~year)

ggsave(copper_size_through_time_1plus_N, file=here("Outputs/Copper/Size_distributions","copper_size_through_time_N_1plus.png"), height=8, width=10)

# Just years with at least 15 fish 
copper_size_through_time_1plus_N_select_years = ggplot(data = PISCO.CPR.fish.filtered.1plus.N %>% filter(year %in% years_with_enough_fish_N$year), aes(fish_tl)) +
  geom_density(alpha = 0.4) +
  geom_label(aes(label=paste("n= ",n)), x = 50, y = 0.04, vjust=0, hjust=0, data=copper_size_tally_N %>% filter(year %in% years_with_enough_fish_N$year)) +
  xlab("Fish total length (cm)") + ylab("Density") + ggtitle("Size of copper by year: 1+, north, years with >=15 fish") +
  theme_bw() +
  facet_wrap(~year)

ggsave(copper_size_through_time_1plus_N_select_years, file=here("Outputs/Copper/Size_distributions","copper_size_through_time_N_1plus_select_years.png"), height=8, width=10)

# South
PISCO.CPR.fish.filtered.1plus.S = PISCO.CPR.fish.filtered.1plus %>% filter(campus %in% c("UCSB","VRG"))
PISCO.CPR.fish.filtered.1plus.S$year = as.factor(PISCO.CPR.fish.filtered.1plus.S$year)  # make year a factor (so it works as color in plot)

copper_size_tally_S = PISCO.CPR.fish.filtered.1plus.S %>% group_by(year) %>% tally()
years_with_enough_fish_S = copper_size_tally_S %>%
  filter(n >= fish_per_year_threshold)

copper_size_through_time_1plus_S = ggplot(data = PISCO.CPR.fish.filtered.1plus.S, aes(fish_tl)) +
  geom_density(alpha = 0.4) +
  geom_label(aes(label=paste("n= ",n)), x = 35, y = 0.08, vjust=0, hjust=0, data=copper_size_tally_S) +
  xlab("Fish total length (cm)") + ylab("Density") + ggtitle("Size of copper by year: 1+, south") +
  theme_bw() +
  facet_wrap(~year)

ggsave(copper_size_through_time_1plus_S, file=here("Outputs/Copper/Size_distributions","copper_size_through_time_S_1plus.png"), height=8, width=10)


##############################


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




# ########## What about multiple passes (need to double-check that those aren't multiple transects)
# # Group together to see how many transects appear duplicated
# PISCO.CPR.1plus.transect = PISCO.CPR.1plus %>%
#   group_by(campus, method, year, month, day, site, zone, level) %>%
#   summarize(n.transects = n())
# 
# # Spot check to see if the number is always the same for a few site/zone/level combos
# table((PISCO.CPR.1plus.transect %>% filter(site == "SRI_CLUSTER_POINT_S", level == "BOT", zone == "INMID"))$n.transects)  
# # 1 2 3 
# # 2 7 4 
# table((PISCO.CPR.1plus.transect %>% filter(site == "SCI_HAZARDS_CEN", level == "BOT", zone == "OUTER"))$n.transects)  
# # 2  3  4 
# # 3  3 13 
# 
# # What about in full PISCO data set? (b/c some transect passes might have gotten filtered out)
# PISCO.transect.grouping = PISCO %>%
#   select(campus, method, year, month, day, site, zone, level, transect) %>%
#   unique() 
# 
# PISCO.transect.grouping %>% filter(site == "SRI_CLUSTER_POINT_S", level == "BOT", zone == "INMID") %>%
#   filter(year == 2018, month == 8, day == 21) # 6 on 9/1/2004, 3 on 8/21/2018
# 
# PISCO.transect.grouping.test = PISCO.transect.grouping %>%
#   group_by(campus, method, year, month, day, site, zone, level) %>%
#   summarize(n.transects = n())
# table((PISCO.transect.grouping %>% filter(site == "SRI_CLUSTER_POINT_S", level == "BOT", zone == "INMID"))$n.transects)  
# # 2  3  6 
# # 1 16  1
# 
# ##### Select one transect per day/month/year - select the lowest-number transect
# PISCO.CPR.1plus.1transect = PISCO.CPR.1plus %>%
#   group_by(campus, method, year, month, day, site, zone, level) %>%
#   arrange(transect) %>%
#   filter(row_number() == 1)
# 
# # Test that that worked
# PISCO.1plus.transect.grouping.test = PISCO.CPR.1plus.1transect %>%
#   group_by(campus, method, year, month, day, site, zone, level) %>%
#   summarize(n.transects = n())
# table((PISCO.1plus.transect.grouping.test %>% filter(site == "SRI_CLUSTER_POINT_S", level == "BOT", zone == "OUTER"))$n.transects)  
# table(PISCO.1plus.transect.grouping.test$n.transects)  # all are 1, looks good - 2098 rows
# 
# # Same thing for YOY
# PISCO.CPR.YOY.1transect = PISCO.CPR.YOY %>%
#   group_by(campus, method, year, month, day, site, zone, level) %>%
#   arrange(transect) %>%
#   filter(row_number() == 1)
# 
# # Split by campus
# PISCO.CPR.1plus.1transect.N = PISCO.CPR.1plus.1transect %>% filter(campus == "UCSC")
# PISCO.CPR.YOY.1transect.N = PISCO.CPR.1plus.1transect %>% filter(campus == "UCSC")
# PISCO.CPR.1plus.1transect.S = PISCO.CPR.1plus.1transect %>% filter(campus == "UCSB")
# PISCO.CPR.YOY.1transect.S = PISCO.CPR.1plus.1transect %>% filter(campus == "UCSB")
# 
# ##### Save output for one transect per combo
# # Northern index
# saveRDS(PISCO.CPR.1plus.1transect.N, file=here("Outputs/Copper","PISCO.CPR.1plus.1transect.N.RDS"))  # filtered down to one transect pass
# saveRDS(PISCO.CPR.YOY.1transect.N, file=here("Outputs/Copper","PISCO.CPR.YOY.1transect.N.RDS"))  # filtered down to one transect pass
# 
# # Southern index
# saveRDS(PISCO.CPR.1plus.1transect.S, file=here("Outputs/Copper","PISCO.CPR.1plus.1transect.S.RDS"))
# saveRDS(PISCO.CPR.YOY.1transect.S, file=here("Outputs/Copper","PISCO.CPR.YOY.1transect.S.RDS"))
# 

# ############ Old code (with various tests and bits of new code mixed in...)
# 
# # # Sites that have seen 1+ copper
# # CPR.pos.site = PISCO.CPR %>%  # just see which sites had CPR and on how many transects they were seen
# #   filter(SCAU>0) %>%   # get positives and look at which transects ever saw them
# #   group_by(campus, site) %>%  # group and tally by site - include campus so the two PURISMA_N and _S sites are separated
# #   tally()
# # 
# # PISCO.CPR.transect = PISCO.CPR.all %>%
# #   group_by(campus, site) %>%
# #   summarize(total_years = n_distinct(year))%>%
# #   filter(ifelse(campus == "UCSB", total_years >= UCSB_80, total_years >= UCSC_80))  # only keep those that were sampled at least 80% of the years the relevant campus sampled
# # 
# 
# # Filter to keep only bottom transects, remove any data without a year, and start UCSC in 2001 and UCSC in 2004
# PISCO.filtered = PISCO %>%
#   filter(!is.na(year), level=="BOT") %>%
#   filter((year>=start_year_UCSB & campus=='UCSB') | (year>=start_year_UCSC & campus=='UCSC')) 
# 
# # Group observations to individual transects, assign each transect an id
# PISCO.transect = PISCO.filtered %>%  
#   select(campus, method, year, month, day, site, zone,
#          level, transect) %>%
#   unique() %>%
#   mutate(id = row_number())   # adds a row number as a trip identifier
# 
# PISCO.vis = PISCO.filtered %>%
#   select(campus, method, year, month, day, site, zone, level, transect, vis) %>%
#   unique()
# 
# PISCO.keep.2 = right_join(PISCO.filtered, PISCO.transect)
# 
# # Merge to add the transect id into the filtered transect dataframe
# PISCO.keep = right_join(PISCO.filtered, PISCO.transect)
# summary(PISCO.transect$id)  # check to make sure min/max id are the same and it looks like the join worked okay
# summary(PISCO.keep$id)
# 
# # Separate out YOY from 1+
# PISCO.keep = PISCO.keep.2 %>%
#   mutate(one_plus = if_else(fish_tl < size_1_plus, 0, 1))
# 
# # Still need to figure out how to get count and one plus in here!!!
# 
# # Maybe I can filter out visibility right at the top (like when I do bottom, etc.), do two PISCO.cnts, one 
# # for each YOY and 1-plus? Then either add them together or keep them separate, depending what makes most sense for rest of filtering
# 
# 
# # Get fish by transect for just 1+, replace NAs with 0s for transects no coppers scene, and select just the coppers
# PISCO.cnts  = PISCO.keep %>%  # 24550 - jumps to 43814 if group_by one_plus too
#   #filter(one_plus == 1) %>%
#   select(id, classcode, count) %>%  # keep just transect id, species and count
#   group_by(id, classcode) %>%       # group by the id and species
#   summarise(sum_cnt = sum(count)) %>%  # sum up the number of fish per transect of each species
#   spread(classcode, sum_cnt) %>%   # make the data wide format rather than long (each species is column)
#   replace(is.na(.), 0) %>%  # replace NA counts with 0 for species not seen
#   select(id, all_of(species_code))  # select just the coppers
# 
# ##### Having some issues here... not sure what is going on with these joins - they produce wildly different numbers of rows
# # Merge back in with PISCO.transect to add back rest of transect info 21341
# PISCO.CPR = inner_join(PISCO.transect, PISCO.cnts)  # PISCO.CPR at this point has been filtered to just BOT, post-2003/2000 for grouped by transect id, just has copper counts of 1+s with zeros for transects where coppers not seen
# PISCO.CPR2 = inner_join(PISCO.keep, PISCO.cnts) %>%
#   filter(classcode == species_code)
# PISCO.CPR3 = left_join(PISCO.cnts, PISCO.keep)
# 
# # Sites that have seen 1+ copper
# CPR.pos.site = PISCO.CPR %>%  # just see which sites had CPR and on how many transects they were seen
#   filter(SCAU>0) %>%   # get positives and look at which transects ever saw them
#   group_by(campus, site) %>%  # group and tally by site - include campus so the two PURISMA_N and _S sites are separated
#   tally()
# 
# # Sites that have seen copper at least once - 18033 transects before 1+ filtering added, 14469 after
# PISCO.CPR.all <- PISCO.CPR %>%
#   filter(site %in% CPR.pos.site$site)  # both UCSC and UCSB PURISIMA_N and PURISIMA_S sites still present - need to cut UCSC PURISIMA_N b/c no copper seen there
# 
# # Sites that were sampled regularly throughout the time period
# PISCO.CPR.transect = PISCO.CPR.all %>%
#   group_by(campus, site) %>%
#   summarize(total_years = n_distinct(year))%>%
#   filter(ifelse(campus == "UCSB", total_years >= UCSB_80, total_years >= UCSC_80))  # only keep those that were sampled at least 80% of the years the relevant campus sampled
# 
# # Join this total years sampled per site back with rest of data for only those sites sampled at least 80% of the total sampling years - 7835
# PISCO.CPR.transect = inner_join(PISCO.CPR.all, PISCO.CPR.transect)  # keeping total_years in there for now to compare sites in the different filtering methods
# CPR.pos = PISCO.CPR.transect %>% filter(SCAU>0)  # 562
# 
# # Check which sites remain in 
# table((PISCO.CPR.transect %>% filter(campus == "UCSC"))$site)
# # From UCSC: BLUEFISH_DC, BLUEFISH_UC, HOPKINS_DC, HOPKINS_UC, MACABEE_DC, MACABEE_UC, MALPASO,
# # MONASTERY_DC, MONASTERY_UC, PESCADERO_DC, PESCADERO_UC, STILLWATER_DC, STILLWATER_UC, WESTON_DC, WESTON_UC
# 
# table((PISCO.CPR.transect %>% filter(campus == "UCSB"))$site)
# # From UCSB: ANACAPA_WEST_ISLE_W, NAPLES_CEN, NAPLES_E, NAPLES_W, SCI_COCHE_POINT_E, SCI_COCHE_POINT_W,
# # SCI_FORNEY_E, SCI_FORNEY_W, SCI_GULL_ISLE_E, SCI_GULL_ISLE_W, SCI_HAZARDS_E, SCI_HAZARDS_W,
# # SCI_PAINTED_CAVE_E, SCI_PAINTED_CAVE_W, SCI_SCORPION_W, SCI_VALLEY_CEN,
# # SCI_YELLOWBANKS_CEN, SCI_YELLOWBANKS_W, SMI_CUYLER_E, SMI_CUYLER_W,
# # SMI_HARRIS_PT_RESERVE_E, SMI_HARRIS_PT_RESERVE_W, SRI_CLUSTER_POINT_N, SRI_CLUSTER_POINT_S,
# # SRI_JOHNSONS_LEE_SOUTH_E, SRI_JOHNSONS_LEE_SOUTH_W, SRI_SOUTH_POINT_E, SRI_SOUTH_POINT_W
# 
# # ones not in original filtering but in filtering up top: SCI_HAZARDS_CEN, SCI_PELICAN_CEN, SCI_SCORPION_E, SCI_CAVERN_POINT_E
# 
# # Remove transects with low visibility and high surge
# PISCO.CPR.transect = PISCO.CPR.transect %>%
#   filter()
# 
# 
# ##### Three levels for filtering sites: 
# ##### 1) all sites that have seen copper at least once
# ##### 2) sites that were sampled in at least 25% of years and saw copper at least 25% of the time
# ##### 3) sites that were sampled in at least 50% of years and saw copper at least 25% of the time
# 
# ### 1) sites that have seen copper at least once - 18033 transects 
# PISCO.CPR.all <- PISCO.CPR %>%
# filter(site %in% CPR.pos.site$site)  # both UCSC and UCSB PURISIMA_N and PURISIMA_S sites still present - need to cut UCSC PURISIMA_N b/c no copper seen there
# 
# # Need to remove UCSC site PURISIMA_N b/c no copper seen there
# UCSC_PN_ids = PISCO.CPR %>% filter(site == "PURISIMA_N", campus == "UCSC")  # find the transect ids of the transects to remove
# PISCO.CPR.all = PISCO.CPR.all %>%  # cut transect rows associated with UCSC PURISIMA_N site
#   filter(!id %in% UCSC_PN_ids$id)
# 
# ### 2) sites that were sampled at least 25% of the time and saw copper at least 25% of the time (both UCSC PURISIMA_N and _S (sampled 1 year) and UCSB PURISIMA_N and _S (sampled 3 years) get filtered out at this stage)
# # Find sites sampled at least 25% of time
# PISCO.CPR.transect.25 = PISCO.CPR %>%
#   group_by(campus, site) %>%
#   summarize(total_years = n_distinct(year))%>%
#   filter(ifelse(campus == "UCSB", total_years >= UCSB_quarter, total_years >= UCSC_quarter))  # only keep those that were sampled at least a quarter of the years the relevant campus sampled
# 
# # Join this total years sampled per site back with rest of data for only those sites sampled at least 25% of the total sampling years - 22384 transects
# PISCO.CPR.25 = inner_join(PISCO.CPR, PISCO.CPR.transect.25)  # keeping total_years in there for now to compare sites in the different filtering methods
# CPR.pos.25 = PISCO.CPR.25 %>% filter(SCAU>0)  # 1008 transects
# 
# # Of those sites, find sites where copper were seen at least 25% of the years 
# PISCO.CPR.pos.transect.25 = CPR.pos.25 %>%
#   group_by(campus,site) %>%
#   summarize(total_pos = n_distinct(year)) %>%
#   filter(ifelse(campus == "UCSB", total_pos >= UCSB_quarter, total_pos >= UCSC_quarter))
# 
# # Filter to include just those sites that were "moderate habitat" - 9037 transects
# PISCO.CPR.25 = inner_join(PISCO.CPR.25, PISCO.CPR.pos.transect.25)  # keeping total_pos in there for now to compare sites in the different filtering
# 
# 
# ### 3) sites that were sampled at least 50% of the time and saw copper at least 50% of the time  
# PISCO.CPR.transect.50 = PISCO.CPR %>%
#   group_by(campus, site) %>%
#   summarize(total_years = n_distinct(year)) %>%
#   filter(ifelse(campus == "UCSB", total_years >= UCSB_half, total_years >= UCSC_half))  # only keep those that were sampled at least half of the years the relevant campus sampled
# 
# # Join this total years sampled per site back with rest of data for only those sites sampled at least 50% of the total sampling years - 18129 transects
# PISCO.CPR.50 = inner_join(PISCO.CPR, PISCO.CPR.transect.50)  # total_years is total years sampled, keeping for now to compare sites in different site filtering
# CPR.pos.50 = PISCO.CPR.50 %>% filter(SCAU>0)  # see how many of the transects were positive for copper sightings  # 925 transects
# 
# # Of those sites, find sites where copper were seen at least 50% of the years (Melissa Monk's "good habitat" from gopher script)
# PISCO.CPR.pos.transect.50 = CPR.pos.50 %>%
#   group_by(campus,site) %>%
#   summarize(total_pos = n_distinct(year)) %>%
#   filter(ifelse(campus == "UCSB", total_pos >= UCSB_half, total_pos >= UCSC_half))
# 
# # Filter to include just those sites that were "good habitat"
# PISCO.CPR.50 = inner_join(PISCO.CPR.50, PISCO.CPR.pos.transect.50)  # 3598 transects
# 
# 
# ##### Split by campus to build a northern and southern index for the two assessments - for now, keeping the three levels of sampling/habitat filtering
# # northern index
# PISCO.CPR.all.UCSC = subset(PISCO.CPR.all, campus=="UCSC")
# PISCO.CPR.25.UCSC = subset(PISCO.CPR.25, campus=="UCSC")
# PISCO.CPR.50.UCSC = subset(PISCO.CPR.50, campus="UCSC")
# 
# # southern index
# PISCO.CPR.all.UCSB = subset(PISCO.CPR.all, campus=="UCSB")
# PISCO.CPR.25.UCSB = subset(PISCO.CPR.25, campus=='UCSB')
# PISCO.CPR.50.UCSB = subset(PISCO.CPR.50, campus=="UCSB")
# 
# ##### Didn't add this in because it doesn't have all of the sites included but here is a version of PISCO.sites with the site names adjusted to have the sides included
# # Has lat/long info and MPA info
# PISCO.sites.v2 = PISCO.sites %>%
#   mutate(old_site_name = site, 
#          site1 = paste(old_site_name,"_",side,sep=""),  # paste together site name with side so site names match those in new data set
#          site = sub("_$","",site1)) %>%  # remove the trailing _ from sites that don't have sides
#   select(-site1)  # remove intermediate site name column
# 

# #################### Save output ####################
# # Data for northern index
# saveRDS(PISCO.CPR.all.UCSC, file=here("Outputs/Copper","PISCO.CPR.all.UCSC.RDS"))  # UCSC sites, all sites that have seen copper at least once regardless of sampling frequency or habitat quality
# saveRDS(PISCO.CPR.25.UCSC, file=here("Outputs/Copper","PISCO.CPR.25.UCSC.RDS"))  # UCSC sites, sites that were both sampled and saw copper at least a quarter of UCSC sampling years 
# saveRDS(PISCO.CPR.50.UCSC, file=here("Outputs/Copper","PISCO.CPR.50.UCSC.RDS"))  # UCSC sites, sites that were both sampled and saw copper at least half of UCSB sampling years
# 
# # Data for southern index
# saveRDS(PISCO.CPR.all.UCSB, file=here("Outputs/Copper","PISCO.CPR.all.UCSB.RDS"))  # UCSB sites, all sites that have seen copper at least once regardless of sampling frequency or habitat quality
# saveRDS(PISCO.CPR.25.UCSB, file=here("Outputs/Copper","PISCO.CPR.25.UCSB.RDS"))  # UCSB sites, sites that were both sampled and saw copper at least a quarter of UCSB sampling years
# saveRDS(PISCO.CPR.50.UCSB, file=here("Outputs/Copper","PISCO.CPR.50.UCSB.RDS"))  # UCSC sites, sites that were both samplied and saw copper at least half of UCSB sampling years
# 
# # Data with both combined (for site comparison)
# saveRDS(PISCO.CPR.all, file=here("Outputs/Copper","PISCO.CPR.all.RDS"))
# saveRDS(PISCO.CPR.25, file=here("Outputs/Copper","PISCO.CPR.25.RDS"))
# saveRDS(PISCO.CPR.50, file=here("Outputs/Copper","PISCO.CPR.50.RDS"))
# 
# # Site info with site names with side appended (though currently unused)
# saveRDS(PISCO.sites.v2, file=here("Outputs/Copper","PISCO.sites.v2.RDS"))  # sites info with site names that include sides to match new data set
# 
# # Sites where copper were seen and on how many transects (for comparing with VRG data)
# saveRDS(CPR.pos.site, file=here("Outputs/Copper","PISCO.CPR.pos.sites.RDS"))

# #################### Data exploration and filtering justification: ####################
# ##### Copper are primarily caught on the bottom level of the water column
# CPR = PISCO %>% filter(classcode == species_code)  # filter from dplyr does essentially the same thing as subset but I think subset can be occasionally less reliable
# table(CPR$level)  # copper only really on bottom level (BOT:1537, MID:13)
# 
# ##### Confirm that we only have subtidal fish sampling methods in this data set
# summary(CPR$method)
# table(CPR$method)  # looks like the same method (SBTL_FISH) but implemented by different groups?
# # SBTL_FISH_CRANE   SBTL_FISH_HSU SBTL_FISH_PISCO   SBTL_FISH_VRG 
# # 4              22            1473              51 
# 
# ##### Keep all zones (though very few in mid, even when all sites that ever saw copper are included)
# table((PISCO.CPR.all.UCSC %>% filter(SCAU>0))$zone)
# # INMID  INNER  OUTER OUTMID 
# # 39     14    289    122 
# 
# table((PISCO.CPR.all.UCSB %>% filter(SCAU>0))$zone)
# # INMID  INNER    MID  OUTER OUTMID 
# # 62     47      1    291    186 
# 
# ##### Check for missing essential data (general sample, not yet filtered/combined to one row per transect)
# summary(as.factor(PISCO$month))  # no NAs
# summary(as.factor(PISCO$year))  # no NAs
# summary(as.factor(PISCO$day))  # no NAs
# summary(as.factor(PISCO$zone))  # no NAs
# summary(as.factor(PISCO$site))  # no NAs but 129354 are listed as (Other)
# summary(as.factor(PISCO$vis))  # 5027 (Other) and 16023 NAs
# summary(as.factor(PISCO$surge))  # lots of NAs (39461)
# summary(as.factor(PISCO$depth))  # lots of (Other) (27252) and NAs (12578)
# 
# # Check - is there a PURISMA_N and _S in both UCSC and UCSB? Yes! Should check with someone that that is right but will treat it as so for now
# UCSB_PN = PISCO %>% filter(campus == "UCSB", site == "PURISIMA_N") %>% filter(classcode == species_code)
# UCSB_PS = PISCO %>% filter(campus == "UCSB", site == "PURISIMA_S") %>% filter(classcode == species_code)
# UCSC_PN = PISCO %>% filter(campus == "UCSC", site == "PURISIMA_N") %>% filter(classcode == species_code)  # this one sees no copper, will cut it from all sites df
# UCSC_PS = PISCO %>% filter(campus == "UCSC", site == "PURISIMA_S") %>% filter(classcode == species_code)
# 
# 
