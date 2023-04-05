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
load(file.path(dir, "south", "Filtered_data_CCFRP.RData"))
south_filtered <- dat
load(file.path(dir, "north", "Filtered_data_CCFRP.RData"))
north_filtered <- dat

south <- left_join(south_len[ind,], south_filtered[, c("driftID", "year")], by = "driftID") %>%
  filter(year >= 2017)
north <- left_join(north_len[ind,], north_filtered[, c("driftID", "year")], by = "driftID") %>%
  filter(year >= 2017)

south <- south[south$driftID %in% south_filtered$driftID, ]
north <- north[north$driftID %in% north_filtered$driftID, ]
south$area <- 'south'
north$area <- 'north'

ggplot(north) +
  geom_bar(aes(x = lengthcm, colour = site)) +
  facet_wrap("year")

ggplot(south) +
  geom_bar(aes(x = lengthcm, colour = site)) +
  facet_wrap("year")

#===============================================================================
# Sample size calculation and input N
#===============================================================================

all_len <- rbind(south, north)

sample_by_group <- all_len %>%
  dplyr::group_by(year, area, name, site) %>%
  dplyr::summarise(
    drifts = length(unique(driftID)),
    n = length(lengthcm))

colnames(sample_by_group) <- c("Year", "Area", "Location", "Site", "Drifts", "Lengths")
write.csv(sample_by_group[sample_by_group$Area == "north", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "north", "forSS", "north_ccfrp_samples_sites.csv"), row.names = FALSE)

write.csv(sample_by_group[sample_by_group$Area == "south", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "south", "forSS", "south_ccfrp_samples_sites.csv"), row.names = FALSE)

# Read in the age data frame to add
# The ages did not come back with identifiers to link back to the length or drift data that I could see
load(file = here("data", "ages", "formatted_age_files", "ccfrp_ages.rdata"))
sample_by_group <- ccfrp_ages %>%
  dplyr::group_by(year, area, location) %>%
  dplyr::summarise(
    n = length(age))

colnames(sample_by_group) <- c("Year", "Area", "Location", "Ages")
write.csv(sample_by_group[sample_by_group$Area == "north", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "north", "forSS", "north_ccfrp_samples_ages.csv"), row.names = FALSE)

write.csv(sample_by_group[sample_by_group$Area == "south", colnames(sample_by_group) != "Area"],
          file = file.path(dir, "south", "forSS", "south_ccfrp_samples_ages.csv"), row.names = FALSE)

#===============================================================================
# Process the Lengths
#===============================================================================
length_bins <- seq(10, 54, 2)

# There are no sexes recorded for these lengths so set all to unsexed
table(all_len$sex)
all_len$sex <- "U"
all_len$age <- NA

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
  month = 7)$unsexed
lfs_mpa[,"InputN"] <- n[n$area == "north" & n$site == "MPA", 'drifts']

lfs_ref <-  UnexpandedLFs.fn(
  datL = all_len[all_len$area == "north" & all_len$site == "REF", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 5, 
  month = 7)$unsexed
lfs_ref[,"InputN"] <- n[n$area == "north" & n$site == "REF", 'drifts']

protect_n <- 0.20; open_n <- 1 - protect_n
ind <- 6:ncol(lfs_mpa)
tmp <- lfs_mpa[, ind] * protect_n + lfs_ref[, ind] * open_n

first <- 2:(length(length_bins) + 1)
second <- (length(length_bins) + 2):ncol(tmp_n)
# This is for unsexed composition data only - sexed you would want to to calc the
# proportions across the whole row
lfs <- cbind(round(100 * tmp[, first] / apply(tmp[, first], 1, sum), 4), 
             round(100 * tmp[, second] / apply(tmp[, second], 1, sum), 4))
out <- cbind(lfs_ref[,], floor(tmp[,1]), lfs)
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
out <- cbind(lfs_ref[,], floor(tmp[,1]), lfs)
write.csv(out, file = file.path(dir, "south", "forSS", "ccfrp_south_weighted_length_comps_unsexed.csv"), row.names = FALSE)

