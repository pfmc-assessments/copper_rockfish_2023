#############################################################
#       Process the CDFW ROV survey lengths
#                 for Copper Rockfish 
#                   Chantel Wetzel
#############################################################

library(here)
library(ggplot2)
library(dplyr)
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")

dir <- file.path(here(), "data", "survey_indices", "rov")

load(file.path(dir, "rov_south_data_used_for_index_creation.rdata"))
load(file.path(dir, "rov_north_data_used_for_index_creation.rdata"))

rov_length <- read.csv(file.path(dir, "final_from_JB", "Copper_StereoSizes_3_11_23.csv"))
rov_length$length_cm <- rov_length$StereoSize
rov_length$area <- "north"
rov_length$area[rov_length$MonitoringRegion %in% c("South")] <- "south"
temp <- as.Date(rov_length$SurveyDate, "%m/%d/%Y")
rov_length$year <- as.numeric(stringr::str_sub(temp, start = 1, end = 4))
rov_length$sex <- "U"
rov_length$age <- NA
rov_length <- rov_length[rov_length$DesignationType != "SMCA/Outside",  ]
rov_length$designation <- "MPA"
rov_length$designation[rov_length$DesignationType == "Reference"] <- "Reference"

# Only keep the lengths associated with transect lines used for the index
find <- which(rov_length$LineID %in% c(rov_north$LineID, rov_south$LineID))
ggplot(rov_length[-find, ], aes(x = length_cm)) +
  geom_histogram() + facet_grid(area~.)
rov_length <- rov_length[find, ]


# There are some lengths (n = 57) with notes on them about precision
# Filtering out via the notes to remove samples with > 2 cm precision would
# be really difficult, so going to remove all with a precision note on them
remove_note <- which(rov_length$note != "")
remove_flag <- which(!is.na(rov_length$remove))

lengths_all <- rov_length
lengths_sub <- rov_length[-remove_flag, ]

#===========================================================================
# Visualize the data
#===========================================================================

ggplot(lengths_sub, aes(y = length_cm, x = year, group = year)) +
  geom_boxplot() + 
  facet_wrap(c("area", "designation")) + 
  xlab("Year") + ylab("Length (cm)") +
  theme_bw() + 
  theme(axis.text = element_text(size=18), 
        axis.title=element_text(size=18,face="bold"),
        strip.text.y = element_text(size = 18))  
ggsave(filename = file.path(dir, "plots", "rov_length_by_area_year.png"),
       width = 10, height = 10)

ggplot(lengths_sub, aes(length_cm, fill = designation, color = designation)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_grid(area~.) +
  theme_bw() + 
  theme(axis.text = element_text(size=18), 
        axis.title=element_text(size=18,face="bold"),
        strip.text.y = element_text(size = 18),
        legend.text=element_text(size=18),
        legend.title=element_text(size=18))  
ggsave(filename = file.path(dir, "plots", "rov_length_by_area_designation.png"),
       width = 10, height = 10)

#===========================================================================
# Calculate the sample sizes
#===========================================================================

sample_size <- lengths_sub %>%
  dplyr::group_by(area, designation, year) %>%
  dplyr::summarise(
    transect = length(unique(LineID)), 
    n = length(length_cm))
samp_north_mpa <- sample_size[sample_size$area == "north" & sample_size$designation == "MPA", 'transect']
samp_north_ref <- sample_size[sample_size$area == "north" & sample_size$designation == "Reference", 'transect']
samp_south_mpa <- sample_size[sample_size$area == "south" & sample_size$designation == "MPA", 'transect']
samp_south_ref <- sample_size[sample_size$area == "south" & sample_size$designation == "Reference", 'transect']

colnames(sample_size) <- c("Area", "Designation",  "Year", "Transects", "Samples")
write.csv(sample_size, file = file.path(dir, "forSS", "sample_size.csv"))


#===========================================================================
# Process the length samples
#===========================================================================

length_bins <- c(seq(10, 54, 2))
  
lfs_north_mpa <-  UnexpandedLFs.fn(
  datL = lengths_all[lengths_all$area == "north" & lengths_all$designation == "MPA", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = "rov", 
  month = 7)$unsexed

lfs_north_ref <-  UnexpandedLFs.fn(
  datL = lengths_all[lengths_all$area == "north" & lengths_all$designation == "Reference", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = "rov", 
  month = 7)$unsexed

lfs_south_mpa <-  UnexpandedLFs.fn(
  datL = lengths_all[lengths_all$area == "south" & lengths_all$designation == "MPA", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = "rov", 
  month = 7)$unsexed

lfs_south_ref <-  UnexpandedLFs.fn(
  datL = lengths_all[lengths_all$area == "south" & lengths_all$designation == "Reference", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = "rov", 
  month = 7)$unsexed

protect_n <- 0.20; open_n <- 1 - protect_n
protect_s <- 0.08; open_s <- 1 - protect_s

ind <- 7:ncol(lfs_north_mpa)

tmp_n <- lfs_north_mpa[, ind] * protect_n + lfs_north_ref[, ind] * open_n
tmp_s <- lfs_south_mpa[, ind] * protect_s + lfs_south_ref[, ind] * open_s

first <- 1:length(length_bins)
second <- (length(length_bins) + 1):ncol(tmp_n)

prop_n <- cbind(round(100 * tmp_n[, first] / apply(tmp_n[, first], 1, sum), 4), 
                round(100 * tmp_n[, second] / apply(tmp_n[, second], 1, sum), 4))
prop_s <- cbind(round(100 * tmp_s[, first] / apply(tmp_s[, first], 1, sum), 4), 
                round(100 * tmp_s[, second] / apply(tmp_s[, second], 1, sum), 4))

n <- protect_n * samp_north_mpa + open_n * samp_north_ref
north <- cbind(lfs_north_mpa[, 1:5], n, prop_n)
colnames(north)[6] <- "InputN"
write.csv(north, file = file.path(dir, "forSS", "north_weighted_lengths_rov.csv"), row.names = FALSE)

n <- protect_s * samp_south_mpa + open_s * samp_south_ref
south <- cbind(lfs_south_mpa[, 1:5], n, prop_s)
colnames(south)[6] <- "InputN"
write.csv(south, file = file.path(dir, "forSS", "south_weighted_lengths_rov.csv"), row.names = FALSE)


plot_comps(
  data = north,
  dir = dir,
  add_save_name = "north"
)

plot_comps(
  data = south,
  dir = dir,
  add_save_name = "south"
)