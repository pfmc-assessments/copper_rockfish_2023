############################################################################################
#	  Filter Recreational Data for Copper Rockfish
#
#	          		November, 2022
#           		Chantel Wetzel
############################################################################################

# copper rockfish species code = 8826010108

dir <- "C:/Assessments/2023/copper_rockfish_2023/data"
code <- "C:/Assessments/2023/copper_rockfish_2023/R"
source(file.path(code, "recfin_areas.R"))
source(file.path(code, "recfin_modes.R"))

# Recreational catch data ======================================
# early MRFSS catch data

# Using the file from 2021 due to issues of not getting the complete
# file when downloading from RecFIN
mrfss_bds_all <- read.csv(file.path(dir, "rec_bds", "ca_type3.csv"))

keep <- which(mrfss_bds_all$ST == 6 & mrfss_bds_all$SP_CODE == "8826010108")
mrfss_bds <- mrfss_bds_all[keep, ]

keep <- colnames(mrfss_bds)[which(apply(mrfss_bds, 2, function(x) sum(is.na(x))) != dim(mrfss_bds)[1])]
mrfss_bds <- mrfss_bds[, keep]

# Remove 350 records with no CNTY listed
mrfss_bds <- mrfss_bds[!is.na(mrfss_bds$CNTY), ] 
# Overlap year with crfss in 2004
mrfss_bds <- mrfss_bds[mrfss_bds$YEAR != 2004, ] 
# Remove NA lengths: 282
remove <- which(is.na(mrfss_bds$LNGTH) & is.na(mrfss_bds$T_LEN))
mrfss_bds <- mrfss_bds[-remove, ]

south <- c(37, 59, 73, 37, 111, 83) # 79 is San Luis Obispo which is north
north <- unique(mrfss_bds[!mrfss_bds$CNTY %in% sout, "CNTY"]) 

mrfss_bds <- recfin_areas(
	data = mrfss_bds,
	area_grouping = list(south, north), 
	area_names = c("south", "north"), 
	column_name = "CNTY")
# for some reason CNTY 111 is not going to the south
find <- which(mrfss_bds$CNTY == 111)
mrfss_bds[find,"area"] <- "south"

mrfss_bds <- recfin_modes(
	data = mrfss_bds, 
	mode_grouping = list(c(1, 2, 4, 5), c(6, 7), 8), 
	mode_names = c("shoreside", "cpfv", "private"),
	column_name = "MODE_F")

mrfss_bds$year <- mrfss_bds$YEAR
mrfss_bds$length_cm <- mrfss_bds$LNGTH / 10

save(mrfss_bds, file = file.path(dir, "rec_bds", "mrfss_bds_filtered.rdata"))

#==================================================================
crfss_bds <- read.csv(file.path(dir, "rec_bds", "SD501-CALIFORNIA-1983---2021.csv"))
# Remove 7 records from 2003
crfss_bds <- crfss_bds[crfss_bds$RECFIN_YEAR != 2003, ]
# Remove NA lengths
crfss_bds <- crfss_bds[!is.na(crfss_bds$AGENCY_LENGTH), ]

crfss_bds <- recfin_areas(
	data = crfss_bds,
	area_grouping = list(c("CHANNEL", "SOUTH"), c("BAY AREA", "WINE", "CENTRAL", "REDWOOD"), "NOT KNOWN"),
	area_names = c("south", "north", "no_area"),
	column_name = "RECFIN_PORT_NAME"
)

# Remove 53 records from unknown areas
crfss_bds <- crfss_bds[crfss_bds$area != "no_area", ]

crfss_bds <- recfin_modes(
	data = crfss_bds, 
	mode_grouping = list(c("BEACH","MAN-MADE"), "PARTY", "PRIVATE"), 
	mode_names = c("shoreside", "cpfv", "private")
)

crfss_bds$year <- crfss_bds$RECFIN_YEAR
crfss_bds$lengthcm <- crfss_bds$AGENCY_LENGTH / 10

aggregate(lengthcm~mode+area, crfss_bds, quantile)


save(crfss_bds, file = file.path(dir, "rec_bds", "crfss_bds_filtered.rdata"))
