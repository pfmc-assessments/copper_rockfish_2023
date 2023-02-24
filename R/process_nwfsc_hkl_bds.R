###################################################################################
#
#   Processs length and CAAL data from the NWFSC Hook & Line Survey
#                       Copper Rockfish 2023
#                         Chantel Wetzel
#
#############################################################################################

library(nwfscSurvey)
library(ggplot2)
library(dplyr)
library(HandyCode)
library(here)

dir <- file.path(here::here(), "data", "nwfsc_hkl")

hkl_all <- read.csv(file.path(dir, "hookandline_2004_2021_draft_data.csv"))
hkl_all$lat <- hkl_all$drop_latitude_degrees
hkl_all$lon <- hkl_all$drop_longitude_degrees
hkl_all$area <- ifelse(hkl_all$site_number >= 500, "CCA", "Outside CCA")
hkl_all$fathom_bin <- plyr::round_any(hkl_all$drop_depth_fathoms, 5, floor)
#hkl_all$length_bin <- plyr::round_any(hkl_all$length_cm, 2, floor)
hkl_all$count <- 0
ind <- which(hkl_all$common_name == "Copper Rockfish")
hkl_all[ind, 'count'] <- 1
# Filter down to only copper obervations
hkl <- hkl_all[ind, ]

hkl_all_site <- hkl_all %>%
  group_by(site_number) %>%
  summarise(
    site_lat = mean(lat),
    site_lon = -1*mean(lon),
    site_depth = mean(drop_depth_fathoms),
    site_area = unique(area),
    total_count = sum(count)
  )

# Remove records that have been identified to have issues by the survey team
# All fish in the data set accidently given a value of include_fish in 2021
hkl[hkl$year == 2021, "include_fish"] <- 1
hkl <- hkl[hkl$include_fish == 1, ]
hkl$set_id_drop <- paste0(hkl$set_id, hkl$drop_number)

# There area only 3 unsexed fish all from different years, dropping them from the comps
hkl <- hkl[hkl$sex != "U", ]

samples_area <- hkl %>%
  group_by(year, area) %>%
  reframe(
    unique_set_by_site = length(unique(set_id_drop)),
    n_copper = sum(count)
  )
write.csv(samples_area, file = file.path(dir, "forSS", "sample_number_by_site_cca.csv"), row.names = FALSE)

samples_all <- hkl %>%
  group_by(year) %>%
  reframe(
    unique_set_by_site = length(unique(set_id_drop)),
    n_copper = sum(count)
  )

samples_non_cca_only <- hkl[hkl$area == "Outside CCA", ] %>%
  group_by(year) %>%
  reframe(
    unique_set_by_site = length(unique(set_id_drop)),
    n_copper = sum(count)
  )

samples_cca_only <- hkl[hkl$area == "CCA", ] %>%
  group_by(year) %>%
  reframe(
    unique_set_by_site = length(unique(set_id_drop)),
    n_copper = sum(count)
  )


#==================================================================
# Create unexpanded length composition data
#==================================================================
length_bins <- seq(10, 54, 2)

# All observation inside and outside CCAs
lfs <-  UnexpandedLFs.fn(
  datL = hkl, 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 9)

lfs_all <- lfs$sexed

lfs$sexed[, "InputN"] <- samples_all[, "unique_set_by_site"]

write.csv(lfs$sexed, 
            file = file.path(dir, "forSS",  "nwfsc_hkl_all_not_expanded_length_comp_sex_3.csv"),
            row.names = FALSE) 

# Observations inside CCA
lfs <-  UnexpandedLFs.fn(
  datL = hkl[hkl$area == "CCA", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 9)
lfs_cca <- lfs$sexed

lfs$sexed[, "InputN"] <- samples_cca_only[, "unique_set_by_site"]

write.csv(lfs$sexed, 
          file = file.path(dir, "forSS",  "nwfsc_hkl_cca_only_not_expanded_length_comp_sex_3.csv"),
          row.names = FALSE) 

# Observations outside CCA
lfs <-  UnexpandedLFs.fn(
  datL = hkl[hkl$area != "CCA", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 9)

lfs$sexed[, "InputN"] <- samples_non_cca_only[, "unique_set_by_site"]

write.csv(lfs$sexed, 
          file = file.path(dir, "forSS",  "nwfsc_hkl_outside_cca_only_not_expanded_length_comp_sex_3.csv"),
          row.names = FALSE) 

#====================================================================
# Plot the composition data
#====================================================================

plot_comps(
  dir = dir,
  data = lfs_all, 
  add_save_name = "nwfsc_hkl_all",
  add_0_ylim = FALSE
)

plot_comps(
  dir = dir,
  data = lfs_cca, 
  add_save_name = "nwfsc_hkl_cca_only",
  add_0_ylim = FALSE
)

hkl$Length_cm <- hkl$length_cm
hkl$Sex <- hkl$sex
PlotSexRatio.fn(
  dir = dir,
  dat = hkl,
  data.type = "length"
)


lbins <- seq(10, 54, 2)
abins <- 1:20
sim <- data.frame(
  year = round(runif(100, 1999, 2002),0),
  length = round(runif(100, 10, 54),0),
  age = round(runif(100, 1, 40),0)
)

l_use_bins <- c(-999, lbins, Inf)
a_use_bins <- c(-999, abins, Inf)
# In case there a fish with decimal lengths round them down for processing
sim$Ls <- l_use_bins[findInterval(sim[, "length"], l_use_bins, all.inside = T)]
sim$As <- a_use_bins[findInterval(sim[, "age"], a_use_bins, all.inside = T)]

test <- sim %>%
  group_by(year) %>%
  count(Ls, As, sort = TRUE, .drop = FALSE)


