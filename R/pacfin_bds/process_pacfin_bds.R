##################################################################################################
#
#	Process and Expand PacFIN Data for Copper Rockfish 2023
# 					
#				Written by Chantel Wetzel
#
##################################################################################################

library(PacFIN.Utilities)
library(ggplot2)
library(here)
library(dplyr)

dir <- file.path(here(), "data", "pacfin_bds")
catch_dir <- file.path(here(), "data", "pacfin_catch")
dir.create(file.path(dir, "plots"))
dir.create(file.path(dir, "forSS"))

bds.file <- "PacFIN.COPP.bds.20.Mar.2023.RData"
load(file.path(dir, bds.file))
# Filter down to only California
bds.pacfin <- bds.pacfin[bds.pacfin$AGENCY_CODE == "C", ]

# Load in the current weight-at-length estimates by sex
# These estimates were created in 2021 from survey data
fa = 9.60e-6; fb = 3.19 
ma = 1.012e-5; mb = 3.15    
ua = (fa + ma)/2;  ub = (fb + mb)/2        

catch_north = read.csv(file.path(catch_dir, "forSS", "commercial_landings_north_for_expansions.csv"))
catch_south = read.csv(file.path(catch_dir, "forSS", "commercial_landings_south_for_expansions.csv"))
# Merge them into one file and treat each area and cond as 
# a separate fleet
catch_file <- data.frame(
	year = catch_south$year, 
	south.live = catch_south$live,
	south.dead = catch_south$dead,
	north.live = catch_north$live,
	north.dead = catch_north$dead
)

data <- cleanPacFIN(
	Pdata = bds.pacfin, 
	CLEAN = TRUE,
	verbose = TRUE)
# Gear groupings reflect those in the table at
# https://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr.txt
# GRID was assigned to geargroup with the following names:
#  HKL  NET  POT  TLS  TWL  TWS 
# 7229   56  178   21  215   36 
# There are 0 records for which the state (i.e., 'CA', 'OR', 'WA')
# could not be assigned and were labeled as 'UNK'.
#   CA 
# 7735 
# The following length types were kept in the data:
# output
#    F 
# 7454 
# Lengths range from 174 to 616 (mm).
# N SAMPLE_TYPEs changed from M to S for special samples from OR: 0
# N not in keep_sample_type (SAMPLE_TYPE): 0
# N with SAMPLE_TYPE of NA: 0
# N not in keep_sample_method (SAMPLE_METHOD): 0
# N with SAMPLE_NO of NA: 0
# N without length: 281
# N without Age: 7735
# N without length and Age: 7735
# N sample weights not available for OR: 0
# N records: 7735
# N remaining if CLEAN: 7735
# N removed if CLEAN: 0

# There are 281 records with no lengths, 108 are from 2018 
no_lengths <- which(is.na(data$lengthcm))
data <- data[-no_lengths, ]

############################################################################################################
# Create areas based on areas being modeled
##########################################################################################################

south_ca <- c("DNA","HNM","LGB","NWB","OBV","OLA","OSD","OXN","SB","SD","SP","TRM","VEN","WLM")
north_ca <- c("ALB","ALM","ARE","AVL", "BCR","BDG","BKL","BOL","BRG","CRS","CRZ","ERK",
			 "FLN","MNT","MOS","MRO","OAK","OCA","OCM","OCN","ODN","OHB","OMD","OSF","OSL","OSM","OWC","PRN","RCH","RYS","SF","SLT","TML","TRN")

area_grouping <- list(south_ca, north_ca)
area_names <- c("south", "north")
data$state_area <- NA
for (a in 1:length(area_grouping)){
	get <- paste(area_grouping[[a]], collapse = "|")
	find <- grep(get, data$PCID, ignore.case = TRUE)
	data$state_area[find] <- area_names[a]
}

data$cond <- "dead"
data$cond[data$COND == "A"] <- "live"
sum(is.na(data$area))

data$fleet <- paste0(data$state_area, ".", data$cond)

save(data, file = file.path(dir, "pacfin_cleaned_data.rdata"))

#################################################################################
# Length samples and trips by area and fleet
#################################################################################

trips_samples_dead <- data %>%
  filter(fleet == "north.dead") %>%
  group_by(fleet, year) %>%
  summarise(
    Trips = length(unique(SAMPLE_NO)),
    Lengths = length(lengthcm)
  )
colnames(trips_samples_dead)[2] <- "Year"
write.csv(trips_samples_dead[, -1], row.names = FALSE, file = file.path(dir, "forSS", "north_dead_trips_and_samples.csv"))

trips_samples_live <- data %>%
  filter(fleet == "north.live") %>%
  group_by(fleet, year) %>%
  summarise(
    Trips = length(unique(SAMPLE_NO)),
    Lengths = length(lengthcm)
  )
colnames(trips_samples_live)[2] <- "Year"
write.csv(trips_samples_live[, -1], 
          row.names = FALSE, 
          file = file.path(dir, "forSS", "north_live_trips_and_samples.csv"))

trips_samples_dead <- data %>%
  filter(fleet == "south.dead") %>%
  group_by(fleet, year) %>%
  summarise(
    Trips = length(unique(SAMPLE_NO)),
    Lengths = length(lengthcm)
  )
colnames(trips_samples_dead)[2] <- "Year"
write.csv(trips_samples_dead[, -1], 
          row.names = FALSE,
          file = file.path(dir, "forSS", "south_dead_trips_and_samples.csv"))

trips_samples_live <- data %>%
  filter(fleet == "south.live") %>%
  group_by(fleet, year) %>%
  summarise(
    Trips = length(unique(SAMPLE_NO)),
    Lengths = length(lengthcm)
  )
colnames(trips_samples_live)[2] <- "Year"
write.csv(trips_samples_live[, -1], 
          file = file.path(dir, "forSS", "south_live_trips_and_samples.csv"),
          row.names = FALSE)


#################################################################################
# Length comp expansions
#################################################################################

MasterData <- data
data$stratification <- data$fleet 

# This uses the same weight length relationship for both areas
data_exp <- getExpansion_1(
	Pdata = data,
	fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub,
	plot = file.path(dir, "plots"))

data_exp <- getExpansion_2(
	Pdata = data_exp, 
	Catch = catch_file, 
	Units = "MT")

data_exp$Final_Sample_Size <- capValues(
	data_exp$Expansion_Factor_1_L * data_exp$Expansion_Factor_2, maxVal = 0.80)
# Maximum expansion capped at 0.8 quantile: 77.3078 

# Look for consistency between lengths and ages of sampled fish
length_bins <- seq(10, 54, 2)

# There are very few sexed fish in California
# table(data_exp$year, data$state_area, data_exp$SEX)
# table(data_exp$fleet, data_exp$SEX)
#                F    M    U
#  north.dead  178  169 3249
#  north.live    0    0 1139
#  south.dead   10    7 2145
#  south.live    0    0  557
# The majority of sexed fish occur in the north in 2019+

Lcomps = getComps(
	Pdata = data_exp, 
	Comps = "LEN")

writeComps(
	inComps = Lcomps, 
	fname = file.path(dir,  "forSS", paste0("PacFIN_Length_comps.", bds.file, ".csv")), 
	lbins = length_bins, 
	partition = 2, 
	sum1 = TRUE,
	digits = 4)


##############################################################################################################
# Format and rewrite
##############################################################################################################
out <- read.csv(
	file.path(dir, "forSS", paste0("PacFIN_Length_comps.", bds.file, ".csv")), 
	skip = 3, header = TRUE)
# Grab unsexed first
start <- which(as.character(out[,1]) %in% c(" Usexed only ")) + 2
end   <- nrow(out)
cut_out <- out[start:end,]
# For some reason the columns with have M and F but are reading the unsexed 
# probably a column name issue

# reading the csv resultsi n in the colnames being the same as the top of the file
ind <- which(colnames(cut_out) %in% paste0("F", min(length_bins))):which(colnames(cut_out) %in% paste0("M", max(length_bins)))
format <- cbind(cut_out$year, cut_out$month, cut_out$fleet, cut_out$sex, cut_out$partition, 
			   cut_out$InputN, cut_out[,ind])
colnames(format) <- c("year", "month", "fleet", "sex", "part", "InputN", colnames(cut_out[ind]))
format <- format[format$year != 2023, ]

grab <- grep("south", format$fleet)
south_unsexed_comps <- format[grab, ]
grab <- grep("north", format$fleet)
north_unsexed_comps <- format[grab, ]

# Now grab the sexed composition data
start <- 1
end   <- which(as.character(out[,1]) %in% c(" Females only ")) - 1
cut_out <- out[start:end,]

ind <- which(colnames(cut_out) %in% paste0("F", min(length_bins))):which(colnames(cut_out) %in% paste0("M", max(length_bins)))
format <- cbind(cut_out$year, cut_out$month, cut_out$fleet, cut_out$sex, cut_out$partition, 
			   cut_out$InputN, cut_out[,ind])
colnames(format) <- c("year", "month", "fleet", "sex", "part", "InputN", colnames(cut_out[ind]))
format <- format[format$year != 2023, ]

grab <- grep("south", format$fleet)
south_sexed_comps <- format[grab, ]
grab <- grep("north", format$fleet)
north_sexed_comps <- format[grab, ]

south_comps <- rbind(south_sexed_comps, south_unsexed_comps)
north_comps <- rbind(north_sexed_comps, north_unsexed_comps)

write.csv(south_comps, file = file.path(dir, "forSS", paste0("PacFIN_south_Lcomps_formatted", bds.file, ".csv")), row.names = FALSE)
write.csv(north_comps, file = file.path(dir, "forSS", paste0("PacFIN_north_Lcomps_formatted", bds.file, ".csv")), row.names = FALSE)

##############################################################################################################
# Plot the comps
##############################################################################################################

library(nwfscSurvey)

south <- read.csv(file.path(dir, "forSS", paste0("PacFIN_south_Lcomps_formatted", bds.file, ".csv")))
north <- read.csv(file.path(dir, "forSS", paste0("PacFIN_north_Lcomps_formatted", bds.file, ".csv")))

south_mf <- south[south$sex == 3, ]
south_u <- south[south$sex == 0, ]
index <- grep("InputN", colnames(south_u)) + 1
colnames(south_u)[index:ncol(south_u)] <- c(paste0('U', length_bins), paste0("U.", length_bins)) 

south_ulive <- south_u[south_u$fleet == "south.live", ]
south_udead <- south_u[south_u$fleet == "south.dead", ]

nwfscSurvey::plot_comps(
  data = south_ulive, 
  dir = dir, 
  add_save_name = "south_live",
  add_0_ylim = TRUE)

nwfscSurvey::plot_comps(
  data = south_udead, 
  dir = dir, 
  add_save_name = "south_dead",
  add_0_ylim = TRUE)

nwfscSurvey::plot_comps(
  data = south_mf, 
  dir = dir, 
  add_save_name = "south",
  add_0_ylim = TRUE)

colnames(north)[1] = "year"
north_mf <- north[north$sex == 3, ]
north_u  <- north[north$sex == 0, ]
index <- grep("InputN", colnames(north_u)) + 1
colnames(north_u)[index:ncol(north_u)] <- c(paste0('U', length_bins), paste0("U.", length_bins)) 

north_ulive <- north_u[north_u$fleet == "north.live", ]
north_udead <- north_u[north_u$fleet == "north.dead", ]

nwfscSurvey::plot_comps(
  data = north_ulive, 
  dir = dir, 
  add_save_name = "north_live",
  add_0_ylim = TRUE)

nwfscSurvey::plot_comps(
  data = north_udead, 
  dir = dir, 
  add_save_name = "north_dead",
  add_0_ylim = TRUE)

nwfscSurvey::plot_comps(
  data = north_mf, 
  dir = dir, 
  add_save_name = "north",
  add_0_ylim = TRUE)