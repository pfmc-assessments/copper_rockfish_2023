# Explore differences between different levels of site filtering 

# Note - sites called PURISIMA_N and PURISIMA_S appear in both UCSC and UCSB site lists - is this right?

#################### Set up: ####################
##### Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

##### Load packages
library(tidyverse)  # includes dplyr, tidyr, ggplot2, stringr
library(here)  # useful for providing directory paths - it sets the base at the folder level (usually - where a RProject or Git repo or other identifiers are)

##### Load data
# Raw transects file
PISCO = read.csv(here("Data/PISCO","MLPA_fish_biomass_transect_raw.csv"))

# Filtered data files from the various levels of site filtering - copper
PISCO.CPR.all = readRDS(file=here("Outputs/Copper","PISCO.CPR.all.RDS"))
PISCO.CPR.25 = readRDS(file=here("Outputs/Copper","PISCO.CPR.25.RDS"))
PISCO.CPR.50 = readRDS(file=here("Outputs/Copper","PISCO.CPR.50.RDS"))

# Filtered data files from the various levels of site filtering - black
PISCO.BLK.all = readRDS(file=here("Outputs/Black","PISCO.BLK.all.RDS"))
PISCO.BLK.25 = readRDS(file=here("Outputs/Black","PISCO.BLK.25.RDS"))
PISCO.BLK.50 = readRDS(file=here("Outputs/Black","PISCO.BLK.50.RDS"))

# Read in PISCO-VRG site match-up (this still hasn't been looked at by anyone but me...)
VRG.PISCO.sites.matchup = readRDS(file=here("Outputs/PISCO VRG site comparison","VRG.PISCO.sites.matchup.RDS")) 

# VRG data 
Vantuna_copper = read.csv(here("Data/Vantuna","Sebastes caurinus.csv"))

#################### Site comparison for PISCO data filtering: ####################

############### Copper ###############
########## Find the sites that each filtering keeps (and number of years sampled, number of years with copper)
##### All sites that have ever seen copper
# Number of total sampling years, number of total copper seen
sites.all.years = PISCO.CPR.all %>%  # length(table(PISCO.CPR.all$site)) only gives 144, this df has 146 rows -?? Found it - PURISIMA_S and PURISIMA_S are coded as both UCSC and UCSB so depending whether you group by campus, it gets included once or twice
  group_by(campus, site) %>%
  summarize(total_years = n_distinct(year),
            total_CPR = sum(SCAU))

# Number of years copper seen 
sites.all.CPR = PISCO.CPR.all %>%
  filter(SCAU>0) %>%
  group_by(campus, site) %>%
  summarize(total_pos = n_distinct(year))

# Join
sites.all = left_join(sites.all.years, sites.all.CPR, by = c("site","campus"))

##### 25% sampling sites
# Find the number of CPR seen at each site 
sites.25.CPR = PISCO.CPR.25 %>%
  group_by(campus,site) %>%
  summarize(total_CPR = sum(SCAU))

# Find the number of years sampled and years positive
sites.25.years = PISCO.CPR.25 %>%
  distinct(campus, site, total_years, total_pos)

# Add back in total number of CPR seen there
sites.25 = left_join(sites.25.years, sites.25.CPR, by = c("site", "campus"))

##### 50% sampling sites
# Find the number of CPR seen at each site 
sites.50.CPR = PISCO.CPR.50 %>%
  group_by(campus, site) %>%
  summarize(total_CPR = sum(SCAU))

# Find the number of years sampled and years positive
sites.50.years = PISCO.CPR.50 %>%
  distinct(campus, site, total_years, total_pos)

# Add back in the total number of CPR seen there
sites.50 = left_join(sites.50.years, sites.50.CPR, by = c("site", "campus"))

##### Do some comparison
# Find the percent positive for each filtering, add filtering type for comparison
sites.all = sites.all %>%
  mutate(perc_pos = total_pos/total_years,
         filtering = "all")
sites.25 = sites.25 %>%
  mutate(perc_pos = total_pos/total_years,
         filtering = "25%")
sites.50 = sites.50 %>%
  mutate(perc_pos = total_pos/total_years,
         filtering = "50%")

# Combine into one df
sites.comp = rbind(sites.all, sites.25, sites.50)

# Visualize which sites are included, include total number of copper
sites_included_UCSB = ggplot(data = sites.comp %>% filter(campus == "UCSB"), aes(x=filtering, y=site, size=total_CPR, color=perc_pos, fill=perc_pos)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  theme_bw()

sites_included_UCSC = ggplot(data = sites.comp %>% filter(campus == "UCSC"), aes(x=filtering, y=site, size=total_CPR, color=perc_pos, fill=perc_pos)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  theme_bw()

ggsave(sites_included_UCSB, file=here("Outputs/Copper","sites_included_UCSB.png"), height=10)  # included color for perc_pos on this one, not sure if it is very useful...
ggsave(sites_included_UCSC, file=here("Outputs/Copper","sites_included_UCSC.png"), height=10)

# Plot distribution of positive sites - not very informative...
site_pos_dist_plot = ggplot(data = sites.comp, aes(x = perc_pos*100, color = filtering, fill = filtering)) +
  geom_freqpoly(bins=10) +
  xlab("Percent of transects seeing copper") +
  theme_bw()

########## Show years sampled for each site 
#### All sites that saw copper at some point
# Group by campus, site, year
PISCO.CPR.all.sampling.years = PISCO.CPR.all %>%
  group_by(campus, site, year) %>%
  summarize(n_CPR = sum(SCAU))

# Plot showing years sampled, UCSC sites
UCSC_sites_sampling_CPR = ggplot(data = PISCO.CPR.all.sampling.years %>% filter(campus == "UCSC"),
                                 aes(x=year, y=site)) +
  geom_point() +
  xlab("") + ylab("") +
  ggtitle("Years sites sampled: UCSC sites, copper") +
  theme_bw()

# Plot showing years sampled, UCSB sites
UCSB_sites_sampling_CPR = ggplot(data = PISCO.CPR.all.sampling.years %>% filter(campus == "UCSB"),
                                 aes(x=year, y=site)) +
  geom_point() +
  xlab("") + ylab("") +
  ggtitle("Years sites sampled: UCSB sites, copper") +
  theme_bw()

ggsave(UCSC_sites_sampling_CPR, file=here("Outputs/Copper","UCSC_sites_sampling_CPR.png"), height=18, width=10)  
ggsave(UCSB_sites_sampling_CPR, file=here("Outputs/Copper","UCSB_sites_sampling_CPR.png"), height=10, width=10)


#################### Add in VRG sites: ####################
##### Edit VRG data so in a more useful format
VRG.data = Vantuna_copper %>%  # there must be a faster/automated way to do this - figure that out at some point
  mutate('2004' = X2004,
         '2007' = X2007,
         '2008' = X2008,
         '2009' = X2009,
         '2010' = X2010,
         '2011' = X2011,
         '2012' = X2012,
         '2013' = X2013,
         '2014' = X2014,
         '2015' = X2015,
         '2016' = X2016,
         '2017' = X2017,
         '2018' = X2018,
         '2019' = X2019,
         '2020' = X2020,
         '2021' = X2021) %>%
  select(-X2004,-X2007,-X2008,-X2009,-X2010,-X2011,-X2012,-X2013,-X2014,-X2015,-X2016,-X2017,-X2018,-X2019,-X2020,-X2021)
VRG.data$Site <- factor(VRG.data$Site)

VRG.data.long = gather(VRG.data, year, copper, '2004':'2021', factor_key=TRUE)

##### VRG sites not in PISCO sites
# Pull out the VRG sites that don't match up with a PISCO site
VRG.not.PISCO.sites = VRG.PISCO.sites.matchup %>%
  filter(in_PISCO == "N")

# Sum up the total number of copper seen in surveys at each site
VRG.data.not.PISCO = VRG.data.long %>%
  filter(Site %in% VRG.not.PISCO.sites$Site) %>%
  filter(!is.na(copper)) %>%  # only keep years and sites where did surveys
  group_by(Site) %>%
  mutate(pos_copper = if_else(copper>0, 1, 0)) %>%
  summarize(total_CPR = sum(copper), total_years_sampled = n_distinct(year), total_years_copper = sum(pos_copper)) %>%
  mutate(perc_pos = total_years_copper/total_years_sampled)

# Pull out just the sites that saw copper at some point
VRG.data.not.PISCO.saw.copper = VRG.data.not.PISCO %>%
  filter(total_CPR > 0)

# Plot all the sites
VRG_sites_not_in_PISCO = ggplot(data = VRG.data.not.PISCO, aes(x="", y=Site, size=total_CPR, color=total_years_sampled, fill=total_years_sampled)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  ggtitle("VRG sites not in PISCO: copper") +
  theme_bw()

ggsave(VRG_sites_not_in_PISCO, file=here("Outputs/Copper","VRG_sites_not_in_PISCO.png"), height=18, width=6)

# Plot just the ones that saw copper at some point
VRG_sites_not_in_PISCO_copper = ggplot(data = VRG.data.not.PISCO.saw.copper, aes(x="", y=Site, size=total_CPR, color=total_years_sampled, fill=total_years_sampled)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  ggtitle("VRG sites not in PISCO, saw copper") +
  theme_bw()

ggsave(VRG_sites_not_in_PISCO_copper, file=here("Outputs/Copper","VRG_sites_not_in_PISCO_saw_copper.png"), height=10, width=6)

##### VRG sites that match to PISCO sites - for now, including both Y and M
# Pull out VRG sites with PISCO matches
PISCO.and.VRG.sites = VRG.PISCO.sites.matchup %>%
  filter(in_PISCO %in% c("Y","M"))

# Get info about years surveyed and copper seen in surveys at each site
VRG.data.PISCO = VRG.data.long %>%
  filter(Site %in% PISCO.and.VRG.sites$Site) %>%
  filter(!is.na(copper)) %>%  # only keep years and sites where did surveys
  group_by(Site) %>%
  mutate(pos_copper = if_else(copper>0, 1, 0)) %>%
  summarize(total_CPR = sum(copper), total_years_sampled = n_distinct(year), total_years_copper = sum(pos_copper)) %>%
  mutate(perc_pos = total_years_copper/total_years_sampled)

# Make a row for each PISCO site (since sometimes multiple match to VRG)
PISCO.and.VRG.sites$Site = factor(PISCO.and.VRG.sites$Site)
PISCO.and.VRG.sites.long = gather(PISCO.and.VRG.sites, PISCO_site_no, PISCO_site_name, PISCO_site_name1:PISCO_site_name3, factor_key = TRUE)

# Add in info about copper seen and years sampled
PISCO.and.VRG.sites.data = left_join(PISCO.and.VRG.sites.long, VRG.data.PISCO, by="Site") %>%
  filter(!is.na(PISCO_site_name)) %>%
  filter(PISCO_site_name != "")

# Filter out sites where copper wasn't ever seen
PISCO.and.VRG.sites.data.copper.seen <- PISCO.and.VRG.sites.data %>%
  filter(total_CPR > 0)

# Add in PISCO campus
# Some VRG sites are marked as UCSB in PISCO data, others as VRG so get campus from full PISCO site list
PISCO_campus <- PISCO %>%
  group_by(site, campus) %>%
  distinct(site, campus) %>%
  select(site, campus)

PISCO.and.VRG.sites.data.copper.seen <- left_join(PISCO.and.VRG.sites.data.copper.seen, PISCO_campus, by=c("PISCO_site_name" = "site"))  # 3 sites marked as both VRG and UCSB - maybe over different periods of time?


# Rework data frame so can join in with sites.all 
PISCO.VRG.df = PISCO.and.VRG.sites.data.copper.seen %>%
  mutate(site = PISCO_site_name,
         total_years = total_years_sampled,
         total_pos = total_years_copper,
         filtering = "VRG data") %>%
  select(campus, site, total_years, total_CPR, total_pos, perc_pos, filtering)

# Make one big data frame - make sure the sites.comp is the copper one before joining!
sites.comp.VRG = rbind(sites.comp, PISCO.VRG.df)  

# And remake the plots with VRG sites as a column too
sites_included_UCSB_VRG = ggplot(data = sites.comp.VRG %>% filter(campus == "UCSB"), aes(x=filtering, y=site, size=total_CPR, color=perc_pos, fill=perc_pos)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  ggtitle("Site filtering: UCSB sites, copper") +
  theme_bw()

# Sites that are included in PISCO set but listed as VRG (3 are also listed as UCSB)
sites_included_VRG = ggplot(data = sites.comp.VRG %>% filter(campus == "VRG"), aes(x="", y=site, size=total_CPR, color=perc_pos, fill=perc_pos)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  ggtitle("Site filtering: VRG sites in full PISCO list") +
  theme_bw()

ggsave(sites_included_UCSB_VRG, file=here("Outputs/Copper","sites_included_UCSB_VRG.png"), height=10)  # included color for perc_pos on this one, not sure if it is very useful...
ggsave(sites_included_VRG, file=here("Outputs/Copper","sites_included_VRG.png"), height=10)



######################### Black #########################
########## Find the sites that each filtering keeps (and number of years sampled, number of years with black)
##### All sites that have ever seen black
# Number of total sampling years, number of total black seen
sites.all.years = PISCO.BLK.all %>%  
  group_by(campus, site) %>%
  summarize(total_years = n_distinct(year),
            total_BLK = sum(SMEL))

# Number of years black seen 
sites.all.BLK = PISCO.BLK.all %>%
  filter(SMEL>0) %>%
  group_by(campus, site) %>%
  summarize(total_pos = n_distinct(year))

# Join
sites.all = left_join(sites.all.years, sites.all.BLK, by = c("site","campus"))

##### 25% sampling sites
# Find the number of BLK seen at each site 
sites.25.BLK = PISCO.BLK.25 %>%
  group_by(campus,site) %>%
  summarize(total_BLK = sum(SMEL))

# Find the number of years sampled and years positive
sites.25.years = PISCO.BLK.25 %>%
  distinct(campus, site, total_years, total_pos)

# Add back in total number of BLK seen there
sites.25 = left_join(sites.25.years, sites.25.BLK, by = c("site", "campus"))

##### 50% sampling sites
# Find the number of BLK seen at each site 
sites.50.BLK = PISCO.BLK.50 %>%
  group_by(campus, site) %>%
  summarize(total_BLK = sum(SMEL))

# Find the number of years sampled and years positive
sites.50.years = PISCO.BLK.50 %>%
  distinct(campus, site, total_years, total_pos)

# Add back in the total number of CPR seen there
sites.50 = left_join(sites.50.years, sites.50.BLK, by = c("site", "campus"))

##### Do some comparison
# Find the percent positive for each filtering, add filtering type for comparison
sites.all = sites.all %>%
  mutate(perc_pos = total_pos/total_years,
         filtering = "all")
sites.25 = sites.25 %>%
  mutate(perc_pos = total_pos/total_years,
         filtering = "25%")
sites.50 = sites.50 %>%
  mutate(perc_pos = total_pos/total_years,
         filtering = "50%")

# Combine into one df
sites.comp = rbind(sites.all, sites.25, sites.50)

# Visualize which sites are included, include total number of black
sites_included_UCSB = ggplot(data = sites.comp %>% filter(campus == "UCSB"), aes(x=filtering, y=site, size=total_BLK, color=perc_pos, fill=perc_pos)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  ggtitle("Site filtering: UCSB sites, black") +
  theme_bw()

sites_included_UCSC = ggplot(data = sites.comp %>% filter(campus == "UCSC"), aes(x=filtering, y=site, size=total_BLK, color=perc_pos, fill=perc_pos)) +
  geom_point(alpha=0.65) +
  xlab("") + ylab("") +
  ggtitle("Site filtering: UCSC sites, black") +
  theme_bw()

ggsave(sites_included_UCSB, file=here("Outputs/Black","sites_included_UCSB_BLK.png"), height=10, width=5)  # included color for perc_pos on this one, not sure if it is very useful...
ggsave(sites_included_UCSC, file=here("Outputs/Black","sites_included_UCSC_BLK.png"), height=18, width=5)


########## Show years sampled for each site 
#### All sites that saw black at some point
# Group by campus, site, year
PISCO.BLK.all.sampling.years = PISCO.BLK.all %>%
  group_by(campus, site, year) %>%
  summarize(n_BLK = sum(SMEL))

# Plot showing years sampled, UCSC sites
UCSC_sites_sampling_BLK = ggplot(data = PISCO.BLK.all.sampling.years %>% filter(campus == "UCSC"),
                                 aes(x=year, y=site)) +
  geom_point() +
  xlab("") + ylab("") +
  ggtitle("Years sites sampled: UCSC sites, black") +
  theme_bw()

# Plot showing years sampled, UCSB sites
UCSB_sites_sampling_BLK = ggplot(data = PISCO.BLK.all.sampling.years %>% filter(campus == "UCSB"),
                                 aes(x=year, y=site)) +
  geom_point() +
  xlab("") + ylab("") +
  ggtitle("Years sites sampled: UCSB sites, black") +
  theme_bw()

ggsave(UCSC_sites_sampling_BLK, file=here("Outputs/Black","UCSC_sites_sampling_BLK.png"), height=18, width=10)  
ggsave(UCSB_sites_sampling_BLK, file=here("Outputs/Black","UCSB_sites_sampling_BLK.png"), height=10, width=10)






