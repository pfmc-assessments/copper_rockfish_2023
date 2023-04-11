##################################################################################################
#
#	        Process CCFRP Length Data
# for use in the Copper Rockfish Assessent in 2023
# 					
#			    Written by Chantel Wetzel
#
##################################################################################################

library(here)
library(dplyr)
library(nwfscSurvey)
# Note of the nwfscSurvey package: I did revisions to the unexpandeLf.fn 
# to work with our data. This function is available in the unexpand_comps
# branch on github.
library(ggplot2)

dir <- file.path(here(), "data", "survey_indices", "ccfrp")

# Load in data from various sources
# CRFS 2004-2022 - both areas
south_len <- read.csv(file.path(dir, "south", "CCFRP_lengths.csv"))
north_len <- read.csv(file.path(dir, "north", "CCFRP_lengths.csv"))
# The lengths have already been filtered based on the retained records 
# load(file.path(dir, "south", "Filtered_data_CCFRP.RData"))
# south_filtered <- dat
# load(file.path(dir, "north", "Filtered_data_CCFRP.RData"))
# north_filtered <- dat

south_len$area <- 'south'
north_len$area <- 'north'

ggplot(north_len) +
  geom_bar(aes(x = lengthcm, colour = site)) +
  #geom_density(aes(x = lengthcm, color = site)) +
  facet_wrap("year")

ggplot(south_len) +
  #geom_bar(aes(x = lengthcm, colour = site)) +
  geom_density(aes(x = lengthcm, color = site)) +
  facet_wrap("year")

#===============================================================================
# Sample size calculation and input N
#===============================================================================

all_len <- rbind(south_len, north_len)

sample_by_group <- all_len %>%
  dplyr::group_by(year, area, name, site) %>%
  dplyr::summarise(
    drifts = length(unique(driftID)),
    n = length(lengthcm))

colnames(sample_by_group) <- c("Year", "Area", "Location", "Site", "Drifts", "Lengths")
write.csv(sample_by_group[sample_by_group$Area == "north", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "north", "forSS", "north_ccfrp_samples_location_sites.csv"), row.names = FALSE)

write.csv(sample_by_group[sample_by_group$Area == "south", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "south", "forSS", "south_ccfrp_samples_location_sites.csv"), row.names = FALSE)


sample_by_group <- all_len %>%
  dplyr::group_by(year, area, site) %>%
  dplyr::summarise(
    drifts = length(unique(driftID)),
    n = length(lengthcm))

colnames(sample_by_group) <- c("Year", "Area",  "Site", "Drifts", "Lengths")
write.csv(sample_by_group[sample_by_group$Area == "north", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "north", "forSS", "north_ccfrp_samples_sites.csv"), row.names = FALSE)

write.csv(sample_by_group[sample_by_group$Area == "south", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "south", "forSS", "south_ccfrp_samples_sites.csv"), row.names = FALSE)

# Read in the age data frame to add
# The ages did not come back with identifiers to link back to the length or drift data that I could see
load(file = here("data", "ages", "formatted_age_files", "ccfrp_ages.rdata"))
samples <- ccfrp_ages %>%
  dplyr::group_by(year, area) %>%
  dplyr::summarise(
    n = length(age))

samples_len <- all_len %>%
  dplyr::group_by(year, area) %>%
  dplyr::summarise(
    drifts = length(unique(driftID)),
    n = length(lengthcm))

all <- left_join(samples_len, samples, by = c("year", "area"))

colnames(all) <- c("Year", "Area", "Drifts", "Lengths", "Ages")
all$Ages[is.na(all$Ages)] <- 0
write.csv(all[all$Area == "north", colnames(all) != "Area"],
          file = file.path(dir, "north", "forSS", "north_ccfrp_drifts_length_ages.csv"), row.names = FALSE)

write.csv(all[all$Area == "south", colnames(all) != "Area"],
          file = file.path(dir, "south", "forSS", "south_ccfrp_drifts_length_ages.csv"), row.names = FALSE)

#===============================================================================
# Process the Lengths
#===============================================================================
length_bins <- seq(10, 54, 2)

all_len$sex[!all_len$sex %in% c("F", "M")] <- "U"

ccfrp_ages %>% group_by(area, year) %>%
  reframe(
    females = sum(sex == "F"),
    males = sum(sex == "M"),
    unsexed = sum(sex == "U")
  )

all_len %>% group_by(area, year) %>%
  filter(year >= 2017) %>%
  reframe(
    females = sum(sex == "F"),
    males = sum(sex == "M"),
    unsexed = sum(sex == "U")
  )  

# There are no sexes recorded for these lengths so set all to unsexed
table(all_len$year, all_len$sex, all_len$site, useNA = "always")
# Since there are only 41æ sexed lengths across all years in the north and that the sexed fish
# by area (ref/mpa) varies which creates challenges in weighting the composition data going to
# set all to unsexed.
all_len$sex <- "U"
all_len$age <- NA

# There is only one sexed length in the south data (in spite of the ages having a sex)
#all_len$sex[all_len$sex %in% c("F", "M") & all_len$area == "south"] <- "U"
#all_len$sex_group <- "U"
#all_len$sex_group[all_len$sex %in% c("F", "M")] <- "B"

n <- all_len %>%
  dplyr::group_by(year, area, site) %>%
  dplyr::summarise(
    drifts = length(unique(driftID)))

# North First
lfs_mpa <-  UnexpandedLFs.fn(
  datL = all_len[all_len$area == "north" & all_len$site == "MPA", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 7)

#mpa_sexed <- lfs_mpa$sexed
mpa_unsexed <- lfs_mpa$unsexed
mpa_unsexed[,"InputN"] <- n[n$area == "north" & n$site == "MPA", 'drifts']
#mpa_sexed[,"InputN"] <- n[n$area == "north" & n$site == "MPA" & n$sex_group == "B", 'drifts']

lfs_ref <-  UnexpandedLFs.fn(
  datL = all_len[all_len$area == "north" & all_len$site == "REF", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 7)
#ref_sexed <- lfs_ref$sexed
ref_unsexed <- lfs_ref$unsexed
ref_unsexed[,"InputN"] <- n[n$area == "north" & n$site == "REF", 'drifts']
#ref_sexed[,"InputN"] <- n[n$area == "north" & n$site == "REF" & n$sex_group == "B", 'drifts']


protect_n <- 0.20; open_n <- 1 - protect_n
ind <- 6:ncol(ref_unsexed)
tmp <- mpa_unsexed[, ind] * protect_n + ref_unsexed[, ind] * open_n
#tmp_sexed <- mpa_sexed[, ind] * protect_n + ref_sexed[, ind] * open_n

first <- 2:(length(length_bins) + 1)
second <- (length(length_bins) + 2):ncol(tmp)
# This is for unsexed composition data only - sexed you would want to to calc the
# proportions across the whole row
lfs <- cbind(round(100 * tmp[, first] / apply(tmp[, first], 1, sum), 4), 
             round(100 * tmp[, second] / apply(tmp[, second], 1, sum), 4))
out <- cbind(ref_unsexed[, 1:5], floor(tmp[,1]), lfs)
write.csv(out, file = file.path(dir, "north", "forSS", "ccfrp_north_weighted_length_comps_unsexed.csv"), row.names = FALSE)


# South ================================================================
lfs_mpa <-  UnexpandedLFs.fn(
  datL = all_len[all_len$area == "south" & all_len$site == "MPA", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 7)$unsexed
lfs_mpa[,"InputN"] <- n[n$area == "south" & n$site == "MPA", 'drifts']

lfs_ref <-  UnexpandedLFs.fn(
  datL = all_len[all_len$area == "south" & all_len$site == "REF", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 7)$unsexed
lfs_ref[,"InputN"] <- n[n$area == "south" & n$site == "REF", 'drifts']

protect_s <- 0.08; open_s <- 1 - protect_s
ind <- 6:ncol(lfs_mpa)
tmp <- lfs_mpa[, ind] * protect_s + lfs_ref[, ind] * open_s

first <- 2:(length(length_bins) + 1)
second <- (length(length_bins) + 2):ncol(tmp)
# This is for unsexed composition data only - sexed you would want to to calc the
# proportions across the whole row
lfs <- cbind(round(100 * tmp[, first] /  apply(tmp[, first], 1, sum), 4), 
             round(100 * tmp[, second] / apply(tmp[, second], 1, sum), 4))
out <- cbind(lfs_ref[,1:5], floor(tmp[,1]), lfs)
write.csv(out, file = file.path(dir, "south", "forSS", "ccfrp_south_weighted_length_comps_unsexed.csv"), row.names = FALSE)

