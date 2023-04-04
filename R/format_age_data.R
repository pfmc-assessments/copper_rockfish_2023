##################################################################
#   Double check the formatted ages released by CAP for 
#         copper rockfish and format into rdata
#                 files for processing. 
#             Chantel Wetzel 3/31/2023
##################################################################

library(dplyr)
library(lubridate)
library(here)

dir <- here("data", "ages", "formatted_age_files")

# Load in all the age data files - this does not include any ages associatted with
# the NWFSC HKL or WCGBT surveys.
crfs <- read.csv(file = file.path(dir, "copper_crfs_recreational_rfg.csv"))
abrams <- read.csv(file = file.path(dir, "copper_abrams_research_ages_2010-2011.csv"))
ccfrp <- read.csv(file = file.path(dir, "copper_ccfrp_ages_2017-2022.csv"))
cdfw <- read.csv(file = file.path(dir, "copper_cdfw_pilot_efi_carcass_ages_2018_2019_2021.csv"))
coop <- read.csv(file = file.path(dir, "copper_cpfv_coop_ages_2022.csv"))
pearson <- read.csv(file = file.path(dir, "copper_don_pearson_research_ages_2001-2007.csv"))

commercial <-  read.csv(file = file.path(dir, "copper_commercial_ages_2018-2022.csv"))
hist_rec <-  read.csv(file = file.path(dir, "copper_historical_rec_1975_1978_1981_1984.csv"))
unknown <- read.csv(file = file.path(dir, "copper_unknown_source_ages_1978.csv"))

# Check all the data for duplicate records
length(unique(crfs$Newport.ID)) == dim(crfs)[1]
length(unique(abrams$Newport.Specimen.ID)) == dim(abrams)[1]
length(unique(ccfrp$Newport.ID)) == dim(ccfrp)[1]
length(unique(cdfw$NewportSpecimenID)) == dim(cdfw)[1]
length(unique(coop$Newport.Structure.ID)) == dim(coop)[1]
length(unique(pearson$Newport.Specimen.ID)) == dim(pearson)[1]
length(unique(commercial$Newport.ID)) == dim(commercial)[1]
length(unique(hist_rec$specimen_id)) == dim(hist_rec)[1]
length(unique(unknown$specimen_id)) == dim(unknown)[1]
unique(hist_rec$specimen_id) %in% unique(unknown$specimen_id)

# Looks like the CCFRP samples with the same Newport.ID are unique
# and may be based on sample day
tmp <- table(ccfrp$Newport.ID)
duplicate <- rownames(tmp)[which(tmp != 1)]
table(ccfrp[ccfrp$Newport.ID %in% duplicate, "Fork.Length_mm"],
      ccfrp[ccfrp$Newport.ID %in% duplicate, "Best_Age"],
      ccfrp[ccfrp$Newport.ID %in% duplicate, "Sex"])
# All of the current ages look to be unique records.

#===============================================================================
# Create formatted rdata files by data source
#===============================================================================

ccfrp_ages <- data.frame(
  program = ccfrp$Program,
  institution = ccfrp$Institution,
  area = "north",
  date = ccfrp$Date,
  month = ccfrp$Month,
  year = ccfrp$Year,
  sex = ccfrp$Sex,
  maturity = ccfrp$Maturity,
  weight_kg = ccfrp$Weight_g / 1000,
  length_cm = ccfrp$Fork.Length_mm / 10,
  age = ccfrp$Best_Age
)
ccfrp_ages$area[ccfrp_ages$institution %in% c("UCSB", "Scripps")] <- "south"

save(ccfrp_ages, file = file.path(dir, "ccfrp_ages.rdata"))

# Abrams Research ages
abrams_ages <- data.frame(
  program = "abrams",
  date = abrams$sample_Date,
  area = "north",
  location = abrams$Location,
  gear = abrams$Gear_Type,
  depth_fm = abrams$Mean_Depth_F,
  distance_nm = abrams$Distance_nm,
  percent_rock = abrams$Perc_Rock,
  sampler = abrams$Sampler,
  sex = abrams$Sex,
  maturity = abrams$Maturity,
  length_cm = abrams$Fork_Length_mm / 10,
  age = abrams$Best_Age,
  notes = abrams$Notes
)
date <- abrams_ages %>%
  tidyr::separate(date, sep="/", into = c("day", "month", "year"))
abrams_ages$year <- date$year

save(abrams_ages, file = file.path(dir, "abrams_ages.rdata"))

# Cooperate ages
coop_ages <- data.frame(
  program = "CPFV_COOP_collections",
  vessel = coop$Vessel,
  date = coop$Date_of_Capture,
  sex = coop$Sex,
  maturity = coop$Maturity..0.Immature..1..Mature.,
  stage = coop$Stage,
  length_cm = coop$Fork_length_mm / 10,
  carcass_length_cm = coop$Carcass_fork_length_mm /10,
  age = coop$Best_Age
)
coop_ages <- coop_ages %>%
  tidyr::separate(date, sep="/", into = c("day", "month", "year"))
coop_ages$area <- "south"
coop_ages$area[coop_ages$vessel %in% c("Legacy", "Salty Lady", "Sea Wolf")] <- "north"
rm <- which(is.na(coop_ages$length_cm) & is.na(coop_ages$carcass_length_cm)) # <- 1 fish no length
coop_ages <- coop_ages[-rm, ]

save(coop_ages, file = file.path(dir, "coop_ages.rdata"))

# Don Pearson Research ages
pearson_ages <- data.frame(
  program = pearson$Program,
  month = pearson$Month,
  year = pearson$Year,
  area = "north",
  latitude = pearson$START_LAT / 100,
  longitude = -1*pearson$START_LONG / 100,
  depth_fm = pearson$START_DEPTH_FATH,
  sex = pearson$Sex,
  maturity = pearson$Maturity,
  length_cm = pearson$Fork_Length_mm / 10,
  age = pearson$Best_Age,
  notes = pearson$Notes
)
pearson_ages$area[pearson_ages$latitude < 34.47] <- "south"
pearson_ages <- pearson_ages[!is.na(pearson_ages$age), ] #3 fish unable to be aged
save(pearson_ages, file = file.path(dir, "pearson_ages.rdata"))

# CRFS ages
crfs_ages <- data.frame(
  program = "CRFS",
  port = crfs$Port.Code,
  mode = crfs$Mode,
  area = "north",
  date = crfs$Date,
  sex = crfs$Sex,
  length_cm = as.numeric(crfs$Fork_Length_mm) / 10,
  age = crfs$Best_Age
)
crfs_ages$area[crfs_ages$port %in% c("OSD")] <- "south"
crfs_ages <- crfs_ages %>%
  tidyr::separate(date, sep="/", into = c("day", "month", "year"))
crfs_ages$sex[crfs_ages$sex == 1] <- "M"
crfs_ages$sex[crfs_ages$sex == 2] <- "F"
crfs_ages$sex[crfs_ages$sex == 9] <- "U"
# Remove the two ages from seized fish
crfs_ages <- crfs_ages[crfs_ages$mode %in% c("PC", "PR"), ]

save(crfs_ages, file = file.path(dir, "crfs_ages.rdata"))

# CDFW non-random sampled ages
cdfw_ages <- data.frame(
  program = cdfw$Program,
  institution = cdfw$Sampling.Institution,
  port = cdfw$Location,
  area = "north",
  gear = cdfw$Gear,
  date = cdfw$Sample.Date,
  year = cdfw$Year,
  sex = cdfw$Sex,
  weight_kg = cdfw$Weight_kg,
  length_cm = cdfw$Fork_Length_mm / 10,
  age = cdfw$Best_Age,
  notes = cdfw$Concern.About.Otolith
)
# Remove the carcass lengths
cdfw_ages <- cdfw_ages[cdfw_ages$program != "Carcass Sampling", ]

save(cdfw_ages, file = file.path(dir, "cdfw_ages.rdata"))

# Commercial Ages
commercial_ages <- data.frame(
  program = "commercial",
  sample_no = commercial$sample_no,
  cluster = commercial$clust_no,
  port = commercial$cal_port,
  area = "north",
  gear = commercial$gear,
  date = commercial$sample_date,
  sex = commercial$Sex,
  length_cm = commercial$Fork_Length_cm,
  age = commercial$Best_Age,
  notes = commercial$comments
)

commercial_ages$area[commercial_ages$port == "SB"] = "south"
commercial_ages <- commercial_ages %>%
  tidyr::separate(date, sep="-", into = c("day", "month", "year"))
commercial_ages$year <- as.numeric(paste0( 20, commercial_ages$year))
commercial_ages$sex[commercial_ages$sex == 9] <- "U"
commercial_ages$sex[commercial_ages$sex == 1] <- "M"
commercial_ages$sex[commercial_ages$sex == 2] <- "F"

save(commercial_ages, file = file.path(dir, "commercial_ages.rdata"))


# Historical Recreational Samples
hist_rec_ages <- data.frame(
  program = hist_rec$sample_type,
  sample_id = hist_rec$sample_id,
  area = "unknown",
  date = hist_rec$Year,
  sex = hist_rec$Sex,
  length_cm = hist_rec$Fork_Length_cm,
  age = hist_rec$Best_Age
)

hist_rec_ages$sex[hist_rec_ages$sex == 3] <- "U"
hist_rec_ages$sex[hist_rec_ages$sex == 1] <- "M"
hist_rec_ages$sex[hist_rec_ages$sex == 2] <- "F"

save(hist_rec_ages, file = file.path(dir, "historical_rec_ages.rdata"))

# Unknown Historical Samples
unknown_ages <- data.frame(
  program = unknown$sample_type,
  sample_id = unknown$sample_id,
  area = "unknown",
  date = unknown$Year,
  sex = unknown$Sex,
  length_cm = unknown$Fork_Length_cm,
  age = unknown$Best_Age
)

unknown_ages$sex[unknown_ages$sex == 1] <- "M"
unknown_ages$sex[unknown_ages$sex == 2] <- "F"

save(unknown_ages, file = file.path(dir, "unknown_historical_ages.rdata"))


# Lets total up the number of read ages
nrow(coop_ages) + nrow(pearson_ages) + nrow(ccfrp_ages) + nrow(cdfw_ages) + nrow(crfs_ages) + 
  nrow(abrams_ages) + nrow(commercial_ages) + nrow(hist_rec_ages) + nrow(unknown_ages)
# 2,340 which does not include either of the NWFSC survey ages 
# NWFSC HKL = 1,151
# NWFSC WCGBT = 864
# 4,355 ages total

#===============================================================================
# Create visualization of the data to see if there are specific records that we
# may want to exclude.
#===============================================================================
library(ggplot2)

ggplot(coop_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "coop_ages.png"), height = 7, width = 10)

ggplot(pearson_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d(begin = 0, end = 0.5) +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") +facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "pearson_ages.png"), height = 7, width = 10)

ggplot(abrams_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d(begin = 0, end = 0.5) +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "abrams_ages.png"), height = 7, width = 10)

ggplot(ccfrp_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "ccfrp_ages.png"), height = 7, width = 10)

ggplot(cdfw_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "cdfw_ages.png"), height = 7, width = 10)

ggplot(crfs_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "crfs_ages.png"), height = 7, width = 10)

ggplot(commercial_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "commercial_ages.png"), height = 7, width = 10)

ggplot(hist_rec_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "historical_rec_ages.png"), height = 7, width = 10)

ggplot(unknown_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "unknown_source_ages.png"), height = 7, width = 10)

#===============================================================================
# Read in the NWFSC survey ages and visualize
#===============================================================================

wcgbt_dir <- file.path(here(), "data", "wcgbt")
load(file.path(wcgbt_dir, "bio_2003-2004_w_ages.rdata"))
bio_orig <- bio_orig[bio_orig$Latitude_dd < 42, ]
bio_orig$area <- 'north'
bio_orig[bio_orig$Latitude_dd < 34.47, 'area'] <- 'south'
bio_orig$program <- "NWFSC_WCGBT"
bio_orig$sex <- bio_orig$Sex
bio_orig$length_cm <- bio_orig$Length_cm
bio_orig$age <- bio_orig$Age

ggplot(bio_orig) + geom_point(aes(y = Length_cm, x = Age, color = Sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "wcgbt_ages.png"), height = 7, width = 10)


hkl_dir <- file.path(here::here(), "data", "nwfsc_hkl")
load(file.path(hkl_dir, "nwfsc_hkl_2004-2022.rdata"))
ind <- which(hkl_all$common_name == "Copper Rockfish")
hkl_all[ind, 'count'] <- 1
hkl <- hkl_all[ind, ]
hkl <- hkl[hkl$include_fish == 1, ]
hkl$area <- 'south'
hkl$program <- "NWFSC_HKL"

ggplot(hkl) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "nwfsc_hkl_ages.png"), height = 7, width = 10)

#===============================================================================
# Throw everything into a single data frame for visualization
#===============================================================================

col_names <- c('program', 'area', 'sex', 'length_cm', 'age')
all_ages <- rbind(
  ccfrp_ages[, col_names],
  hkl[, col_names], 
  bio_orig[, col_names],
  abrams_ages[, col_names],
  pearson_ages[, col_names],
  cdfw_ages[, col_names],
  crfs_ages[, col_names],
  coop_ages[, col_names],
  commercial_ages[, col_names],
  hist_rec_ages[, col_names],
  unknown_ages[, col_names]
)
save(all_ages, file = file.path(dir, "all_copper_ages.rdata"))

ggplot(all_ages) + geom_point(aes(y = length_cm, x = age, color = sex)) +
  scale_color_viridis_d() +
  ylim(c(0, 60)) + xlim(c(0, 55)) + xlab("Age") + ylab("Length (cm)") + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "all_ages.png"), height = 7, width = 10)

