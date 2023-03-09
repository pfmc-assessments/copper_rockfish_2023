##################################################################################################
# Contains SQL connections to read in the following recreational length data
# for the cooperative recreational sampling and the 2015-2019 PCO samples
# for Copper Rockfish 2023
#	
# Written by Melissa Monk
#
##################################################################################################

library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
library(DBI)
library(glue)

dir <- file.path(here(), "data", "rec_bds")
setwd(dir)
dir.create(file.path(dir, "plots"))

# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "pinniger"

#Set your species
sppAbbr <- "COPP"

#load in cooperative recreational length comps
# connect to database with windows authentication
# SWFSC groundfish collections; 2022 recreational cooperative 
con_swc_gfish <- DBI::dbConnect(odbc::odbc(),
                                driver = db_driver,
                                server = swc_server,
                                database = "SWFSC_Groundfish_Collections",
                                Trusted_Connection = "yes"
)
query_coop <- glue::glue_sql(
"SELECT SampleID, CaptureMonth, Cooperative_Recreational_CPFV_Collections.Vessel, 
ForkLengthMM, Sex, Maturity,Stage, District, DurationType,TripType, area, cdfwblockid
FROM Cooperative_Recreational_CPFV_Collections
inner join Cooperative_Recreational_CPFV_Collections_Port_Lookup 
	  on Cooperative_Recreational_CPFV_Collections.Vessel = Cooperative_Recreational_CPFV_Collections_Port_Lookup.Vessel
    where speciescode = {sppAbbr} and forklengthmm is not null",
                             .con = con_swc_gfish)
coop <- dbGetQuery(con_swc_gfish, query_coop)
dbDisconnect(con_swc_gfish)

#modify columns
coop <- coop %>%
  mutate(lengthcm = ForkLengthMM/10) %>%
  filter(District < 3)
  

#Match cdfw onboard observer locations to blocks for retained catch
#retained catch is only recorded at the end of the trip
con_pco <- DBI::dbConnect(odbc::odbc(),
                      driver = "SQL Server",
                      server = "pinniger",
                      database = "CDFW_CPFV_Onboard_2015_on",
                      Trusted_Connection = "yes"
)

query_pco_blocks <- glue::glue_sql(
  "SELECT PCO_Site.RefNum, DurationType, District, year,
  cdfwblockid, count(*) drifts_in_block
  FROM PCO_Site
  Left join PCO_Distinct_Trips on PCO_Distinct_Trips.RefNum = PCO_Site.RefNum
  where cdfwblockid is not null and district <3 and cdfwblockid > 0
  group by PCO_Site.RefNum, cdfwblockid , DurationType, District, year
  order by refnum", .con = con_pco)
pco_trips <- dbGetQuery(con_pco, query_pco_blocks)
pco_trips <- pco_trips %>%
  filter(drifts_in_block >1)
length(unique(pco_trips$RefNum))

query_copp_lengths <- glue::glue_sql(
  "SELECT *
  FROM PCO_Biodata
  where species = 'rfcop' and stopnum is null", .con = con_pco)
pco_lengths <- dbGetQuery(con_pco, query_copp_lengths)
fish_lengths <- pco_lengths %>%
  dplyr::select(RefNum, SampleNum, Species, Length, CatchBioDetailID)


#get the blocks most visited per trip
#if tie take the first one
pco_count <- pco_trips %>%
  group_by(RefNum) %>%
  filter(drifts_in_block == max(drifts_in_block)) %>%
  slice_head(n = 1)

#join to lengths
fish_lengths <- left_join(pco_lengths, pco_count) %>%
  filter(!is.na(cdfwblockid)) %>%
  mutate(lengthcm = Length/10) %>%
  rename(SampleID = RefNum)
fish_lengths$cdfwblockid = as.factor(fish_lengths$cdfwblockid)  

#remove blocks with fewer than 30 fish
block_freq <- fish_lengths %>%
  group_by(cdfwblockid) %>%
  tally() %>%
  filter(n>29)

fish_lengths <- fish_lengths %>%
  filter(cdfwblockid %in% block_freq$cdfwblockid)


##Look at trip type by block and duration
crfs_trips <- fish_lengths %>%
  dplyr::select(RecFinSurveyEventID, year, DurationType, District,
                cdfwblockid) %>%
  unique() 
 
#can probably ignore this for blue
block_duration <- crfs_trips %>%
  group_by(DurationType, cdfwblockid) %>%
  tally() %>%
  pivot_wider(names_from = DurationType, values_from = n)



crfs_fish <- fish_lengths %>%
  group_by(DurationType, cdfwblockid) %>%
  tally() %>%
  pivot_wider(names_from = DurationType, values_from = n)
  
#-------------------------------------------------------------
#
#
#-------------------------------------------------------------
#join coop and crfs data
fish_lengths$program <- "crfs"
coop$program <- "coop_cpfv"
coop$year <- 2022

crfs_lengths <- fish_lengths %>%
  dplyr::select(SampleID, lengthcm, program, cdfwblockid, year, DurationType)
coop_lengths <- coop %>%
  dplyr::select(SampleID, lengthcm, program, cdfwblockid, year, DurationType) %>%
  mutate(cdfwblockid = as.factor(cdfwblockid))
all_lengths <- full_join(crfs_lengths, coop_lengths)


#boxplot of lengths by block for crfs data
 ggplot(fish_lengths, aes(x = cdfwblockid, y = lengthcm)) + 
   geom_boxplot() +
   facet_wrap(~as.factor(DurationType)) +
   xlab("Length") + ylab("") +
   theme(axis.text = element_text(size = 12),
         axis.title = element_text(size = 12),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 12),
         strip.text.y = element_text(size = 14)) 
 ggsave(filename = file.path(dir, "plots", "rec_coop_crfs_lengths_by_blocks.png"),
        width = 10, height = 8)
 
#only keep the blocks where both have samples 
 all_lengths <- all_lengths %>%
   filter(cdfwblockid %in% coop_lengths$cdfwblockid)



# #block 687
# block687 <- lengths1 %>%
#   filter(cdfwblockid ==687)
# ggplot(block687, aes(x = lengthcm,
#                      colour = as.factor(program),
#                      fill = as.factor(program))) +
#   geom_density(alpha = 0.5) +
#   facet_wrap(~year, ncol = 1) 
all_lengths$program = as.factor(all_lengths$program)

#plots
ggplot(all_lengths, aes(x = cdfwblockid, y = lengthcm,
                          colour = program)) + 
   geom_boxplot() + 
  facet_wrap(~DurationType) +
  facet_wrap(~as.factor(DurationType)) +
  xlab("Length") + ylab("") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) 
ggsave(filename = file.path(dir, "plots", "rec_coop_crfs_lengths_by_blocks.png"),
       width = 10, height = 8)


ggplot(all_lengths, aes(x = lengthcm,
                     colour = program)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~cdfwblockid)  +
  xlab("Length") + ylab("") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) 
ggsave(filename = file.path(dir, "plots", "rec_coop_crfs_lengths_by_blocks_boxplot.png"),
       width = 10, height = 8)

#table of sample sizes
length_sample_size <- all_lengths %>%
  group_by(program, cdfwblockid) %>%
  tally()


#length distributed by the catch
#what fraction of of trips going to each block
#what fraction of the catch froms from 
#a particular area
#dome shaped selectivity - doesn't fix
#weighting issue
 
#color code median by block


# #add in the matching columns for the mode and year to the crfs data
# coop$mode <- 'cpfv'
# coop$year <- 2022
# coop$lengthcm <- coop$ForkLengthMM/10
# 
# aggregate(lengthcm~mode+area, crfs_cpfv, quantile)
# aggregate(lengthcm~mode+area+TripType, coop, quantile)
# 
# 
# ggplot(crfs_cpfv, aes(lengthcm, fill = area, color = area)) + 
#   geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
#   xlab("Length (cm)") + ylab("Density") +
#   facet_grid(area~.) 
# ggsave(filename = file.path(dir, "plots", "crfs_length_dist_by_mode_area.png"),
#        width = 10, height = 7)
# 


save(rec_coop_bds, file = file.path(dir, "rec_bds", "rec_bds_filtered.rdata"))



