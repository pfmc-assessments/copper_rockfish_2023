##################################################################################################
#
#	Process and Expand PacFIN Data for Copper Rockfish 2023
# 					
#				Written by Chantel Wetzel
#
##################################################################################################

library(PacFIN.Utilities)
library(ggplot2)

dir <- "C:/Assessments/2023/copper_rockfish_2023/data/pacfin_bds"
catch_dir <- "C:/Assessments/2023/copper_rockfish_2023/data/pacfin_catch"
dir.create(file.path(dir, "plots"))
dir.create(file.path(dir, "forSS"))

bds.file <- "PacFIN.COPP.bds.07.Nov.2022.RData"
load(file.path(dir, bds.file))

# Load in the current weight-at-length estimates by sex
# These estimates were created in 2021 from survey data
fa = 9.56e-6; fb = 3.19 
ma = 1.08e-5; mb = 3.15    
ua = (fa + ma)/2;  ub = (fb + mb)/2        

catch_north = read.csv(file.path(catch_dir, "forSS", "commercial_landings_north_for_expansions.csv"))
catch_south = read.csv(file.path(catch_dir, "forSS", "commercial_landings_south_for_expansions.csv"))
catch_file <- data.frame(
	year = catch_south$year, 
	south.live = catch_south$live,
	south.dead = catch_south$dead,
	north.live = catch_north$live,
	north.dead = catch_north$dead
)

data <- cleanPacFIN(
	Pdata = bds.pacfin[bds.pacfin$AGENCY_CODE == "C",], 
	CLEAN = TRUE,
	verbose = TRUE)

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
data$state_areas <- NA
for (a in 1:length(area_grouping)){
	get <- paste(area_grouping[[a]], collapse = "|")
	find <- grep(get, data$PCID, ignore.case = TRUE)
	data$state_area[find] <- area_names[a]
}

data$cond <- "dead"
data$cond[data$COND == "A"] <- "live"

data$fleet <- paste0(data$state_area, ".", data$cond)

#################################################################################
# Length comp expansions
#################################################################################

MasterData <- data
data$stratification <- data$fleet 

# This uses the same weight length relationship for both areas
data_exp <- getExpansion_1(
	Pdata = data,
	fa = fa, fb = fb, ma = ma, mb = mb, ua = ua, ub = ub)

data_exp <- getExpansion_2(
	Pdata = data_exp, 
	Catch = catch_file, 
	Units = "MT",
	maxExp = 0.80)

data_exp$Final_Sample_Size <- capValues(data_exp$Expansion_Factor_1_L * data_exp$Expansion_Factor_2, maxVal = 0.80)
# Maximum expansion capped at 0.8 quantile: 77.3078 

# Look for consistency between lengths and ages of sampled fish
length_bins <- c(seq(8, 56, 2))

# There are very few sexed fish in California
# table(data_exp$fleet, data_exp$SEX)
#                F    M    U
#  north.dead  136  132 3269
#  north.live    0    0 1354
#  south.dead    2    7 2126
#  south.live    0    0  554

Lcomps = getComps(
	data_exp, 
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

# reading the csv resultsin in the colnames being the same as the top of the file
ind <- which(colnames(cut_out) %in% paste0("F", min(length_bins))):which(colnames(cut_out) %in% paste0("M", max(length_bins)))
format <- cbind(cut_out$fishyr, cut_out$month, cut_out$fleet, cut_out$sex, cut_out$partition, 
			   cut_out$InputN, cut_out[,ind])
colnames(format) <- c("fishyr", "month", "fleet", "sex", "part", "InputN", colnames(cut_out[ind]))
format <- format[format$fishyr != 2023, ]

grab <- grep("south", format$fleet)
south_unsexed_comps <- format[grab, ]
grab <- grep("north", format$fleet)
north_unsexed_comps <- format[grab, ]

# Now grab the sexed composition data
start <- 1
end   <- which(as.character(out[,1]) %in% c(" Females only ")) - 1
cut_out <- out[start:end,]

ind <- which(colnames(cut_out) %in% paste0("F", min(length_bins))):which(colnames(cut_out) %in% paste0("M", max(length_bins)))
format <- cbind(cut_out$fishyr, cut_out$month, cut_out$fleet, cut_out$sex, cut_out$partition, 
			   cut_out$InputN, cut_out[,ind])
colnames(format) <- c("fishyr", "month", "fleet", "sex", "part", "InputN", colnames(cut_out[ind]))
format <- format[format$fishyr != 2023, ]

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