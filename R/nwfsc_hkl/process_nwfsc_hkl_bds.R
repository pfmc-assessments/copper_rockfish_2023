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

load(file.path(dir, "nwfsc_hkl_2004-2022.rdata"))
hkl_all <- hkl
hkl_all$lat <- hkl_all$drop_latitude_degrees
hkl_all$lon <- hkl_all$drop_longitude_degrees
hkl_all$area <- ifelse(hkl_all$site_number >= 500, "CCA", "Outside CCA")
hkl_all$fathom_bin <- plyr::round_any(hkl_all$drop_depth_fathoms, 5, floor)
hkl_all$depth_bin <- plyr::round_any(hkl_all$drop_depth_meters, 5, floor)
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
hkl$set_id_drop <- paste0(hkl$set_id, hkl$drop_number)
hkl <- hkl[hkl$include_fish == 1, ]

# There area only 4 unsexed fish all from different years, dropping them from the comps
hkl <- hkl[hkl$sex != "U", ]

samples_area <- hkl %>%
  group_by(year, area) %>%
  reframe(
    Drops = length(unique(set_id_drop)),
    Observations = sum(count)
  )
colnames(samples_area)[1:2] <- c("Year", "Area")
write.csv(samples_area, file = file.path(dir, "forSS", "sample_number_by_site_cca.csv"), row.names = FALSE)


sample_summary <- hkl %>%
  group_by(year) %>%
  reframe(
    unique_set_by_site = length(unique(set_id_drop)),
    lengths = sum(!is.na(length_cm)),
    ages = sum(!is.na(age_years))
  )
colnames(sample_summary) <- c("Year", "Effective Sample Size", "Lengths", "Ages")
write.csv(sample_summary, file = file.path(dir, "forSS", "nwfsc_hkl_effn_lengths_ages.csv"), row.names = FALSE)

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


#==============================================================================
# Create marginal ages
#==============================================================================
age_bins <- 1:50
source(file.path(here(), "R", "get_caal.R"))

afs <-  UnexpandedAFs.fn(
  datA = hkl, 
  ageBins = age_bins,
  partition = 0, 
  fleet = 5, 
  month = 9)

afs_all <- afs$sexed
afs$sexed[, "InputN"] <- samples_all[, "unique_set_by_site"]
write.csv(afs$sexed, 
          file = file.path(dir, "forSS",  "nwfsc_hkl_all_not_expanded_age_comp_sex_3.csv"),
          row.names = FALSE) 

# Observations inside CCA
afs <-  UnexpandedAFs.fn(
  datA = hkl[hkl$area == "CCA", ], 
  ageBins = age_bins,
  partition = 0, 
  fleet = 5, 
  month = 9)

afs_cca <- afs$sexed
afs$sexed[, "InputN"] <- samples_cca_only[, "unique_set_by_site"]
write.csv(afs$sexed, 
          file = file.path(dir, "forSS",  "nwfsc_hkl_cca_only_not_expanded_age_comp_sex_3.csv"),
          row.names = FALSE) 

# Observations outside CCA
afs <-  UnexpandedAFs.fn(
  datA = hkl[hkl$area != "CCA", ], 
  ageBins = age_bins,
  partition = 0, 
  fleet = 5, 
  month = 9)

afs$sexed[, "InputN"] <- samples_non_cca_only[, "unique_set_by_site"]
write.csv(afs$sexed, 
          file = file.path(dir, "forSS",  "nwfsc_hkl_outside_cca_only_not_expanded_age_comp_sex_3.csv"),
          row.names = FALSE) 

#====================================================================
# Plot the age composition data
#====================================================================

plot_comps(
  dir = dir,
  data = afs_all, 
  add_save_name = "nwfsc_hkl_all_age",
  add_0_ylim = FALSE
)

plot_comps(
  dir = dir,
  data = afs_cca, 
  add_save_name = "nwfsc_hkl_cca_only_age",
  add_0_ylim = FALSE
)

#==============================================================================
# Create conditional-age-at-length ages
#==============================================================================

out <- get_caal(
  data = hkl, 
  len_bins = length_bins,
  age_bins = age_bins,
  month = 9, 
  fleet = 5)

name <- paste0("CAAL_len_", min(length_bins), "_", max(length_bins), "_age_",
  min(age_bins), "_", max(age_bins), ".csv")
write.csv(out, file = file.path(dir, "forSS", name), row.names = FALSE)

#====================================================================
# create tables
#====================================================================
library(kableExtra)

doc_loc <- file.path(here(), "documents", "sca", "tex_tables")

hkl_all$depth_bin <- plyr::round_any(hkl_all$drop_depth_meters, 10, floor)
hkl_all$set_id_drop <- paste0(hkl_all$set_id, hkl_all$drop_number)

dat <- hkl_all %>%
  mutate(Targetbin = as.numeric(count > 0))
  
sample.sizes.depth <- dat %>%
  dplyr::group_by(depth_bin) %>%
  dplyr::summarise(
    Positive.samples = sum(Targetbin),
    Samples = length(unique(set_id_drop))
  ) %>%
  mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1))

colnames(sample.sizes.depth) <- c("Depth (m)", "Positive Samples", "Samples", "Percent Positive")
write.csv(sample.sizes.depth, 
    file = file.path(dir, "forSS", "positive_samples_by_depth.csv"),
    row.names = FALSE)

# make table
table.depth <- kableExtra::kbl(sample.sizes.depth,
  booktabs = TRUE,
  caption = paste(
    "Positive samples of copper rockfish in the NWFSC Hook and Line survey by depth (fm)."
  ),
  label = paste0("tab-depth-nwfschkl")
) %>%
  kable_styling(latex_options = "striped", full_width = FALSE)



#-------------------------------------------------------------------------------
# tab-year
# samples by year
sample.sizes.year <- dat %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    Positive.samples = sum(Targetbin),
    Samples = length(unique(set_id_drop))
  ) %>%
  mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1))

colnames(sample.sizes.year) <- c("Year", "Positive Samples", "Samples", "Percent Positive")
write.csv(sample.sizes.year, 
    file = file.path(dir, "forSS", "positive_samples_by_year.csv"),
    row.names = FALSE)
model.region = "NWFSC Hook and Line survey"
# make table
table.year <- kableExtra::kbl(sample.sizes.year,
  booktabs = TRUE,
  caption = paste(
    "Samples of copper rockfish in the",
    model.region, "by year."
  ),
  label = paste0("tab-year-nwfschl")
) %>%
  kable_styling(latex_options = "striped", full_width = FALSE)

#-------------------------------------------------------------------------------
# tab-depth by area
# samples by year
depth.by.site <- dat %>%
  dplyr::group_by(area, depth_bin) %>%
  dplyr::summarise(
    Positive.samples = sum(Targetbin),
    Samples = length(unique(set_id_drop))
  ) %>%
mutate(percent.pos = scales::percent(Positive.samples / Samples, accuracy = 1)) %>%
  dplyr::select(area, depth_bin, percent.pos) %>%
  tidyr::pivot_wider(names_from = depth_bin, values_from = percent.pos) %>%
  #relocate("(25,50]", .before = "(75,100]") %>%
  #relocate("(50,75]", .before = "(75,100]") %>%
  rename("Area name" = area)

table.depth.by.site <- kableExtra::kbl(depth.by.site,
                                booktabs = TRUE,
                                caption = paste(
                                  "Samples of", params$species.name, "in the
                                  NWFSC hook-and-line survey by area and 
                                  depth bins (ft)."
                                ),
                                label = paste0("tab-depthsite-nwfschl")
  ) %>%
  kable_styling(latex_options = c("striped", "scale_down"), full_width = FALSE)


#================================================================================


ggplot(hkl, aes(x = depth_bin, fill = area)) +
  geom_bar(position="stack") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  scale_fill_viridis_d(begin = 0, end = 0.50) +
  xlim(c(25, 130)) + 
  xlab("Depth (m)") + ylab("Number of Observations")
ggsave(file = file.path(dir, "plots", "nwfsc_hkl_observations_by_depth.png"),
       width = 7, height = 7)
