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
abrams <- read.csv(file = file.path(dir, "copper_abrams_research_ages_2021.csv"))
ccfrp <- read.csv(file = file.path(dir, "copper_ccfrp_ages_2021-2023.csv"))
cdfw <- read.csv(file = file.path(dir, "copper_cdfw_pilot_efi_carcass_ages_2021.csv"))
coop <- read.csv(file = file.path(dir, "copper_cpfv_coop_ages_2023.csv"))
pearson <- read.csv(file = file.path(dir, "copper_don_pearson_research_ages_2021.csv"))

# Check all the data for duplicate records
length(unique(crfs$Newport.ID)) == dim(crfs)[1]
length(unique(abrams$Newport.Specimen.ID)) == dim(abrams)[1]
length(unique(ccfrp$Newport.ID)) == dim(ccfrp)[1]
length(unique(cdfw$NewportSpecimenID)) == dim(cdfw)[1]
length(unique(coop$Newport.Structure.ID)) == dim(coop)[1]
length(unique(pearson$Newport.Specimen.ID)) == dim(pearson)[1]

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


# Lets total up the number of read ages
nrow(coop_ages) + nrow(pearson_ages) + nrow(ccfrp_ages) + nrow(cdfw_ages)+ nrow(crfs_ages) + nrow(abrams_ages)
