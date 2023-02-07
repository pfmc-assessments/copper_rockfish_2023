##############################################################################################################
#
# 	Data processing of commercial landings data for
#		the 2023 copper rockfish assessment 
#			  by Chantel Wetzel 
#					
##############################################################################################################

library(here)

dir = getwd() #"C:/Assessments/2023/copper_rockfish_2023/data/pacfin_catch"
pac_dir <- file.path(dir, "data", "pacfin_catch")
dir.create(file.path(pac_dir, "forSS"), showWarnings = FALSE)

# Load the data items
load(file.path(pac_dir, "PacFIN.COPP.CompFT.06.Feb.2023.RData"))

# Areas in historical commercial reconstruction:
# region: defined as the CDFW block / 100
# 0	unknown
# 2	crescent city, eureka, fort bragg
# 4	bodega bay, san francisco, half moon bay
# 5	santa cruz, moss, landing, montery
# 6	morro bay, port san luis, santa barbara
# 7	los angeles
# 8	san diego
com_hist_early <- read.csv(file.path(pac_dir,  "ca_hist_commercial_1916_1968_ej.csv"))
com_hist_late <- read.csv(file.path(pac_dir, "ca_hist_commercial_1969_1980_ej.csv"))
# Based on Cowcod (2007) approach
com_ratio <- read.csv(file.path(pac_dir, "historical_landings_tables_2020_revised_Avila_1953.csv"))

# Discard rate based on GEMM analysis look at areas south of 40 10 for select sectors:
# "LE Fixed Gear DTL - Hook & Line", "Nearshore", "CS - Hook & Line",
# " OA Fixed Gear - Hook & Line", "OA Fixed Gear - Pot", "LE Fixed Gear DTL - Pot"
load(file.path(dir, "data", "gemm", "gemm_processed_south_4010.Rdata"))
wcgop_discard_rate <- as.data.frame(gemm_data)
# Set the historical rate equal to 0.03 based on the median from the GEMM data
hist_discard_rate <- median(wcgop_discard_rate$rate)

# Filter the PacFIN catches to only CA
# =================================================================
pacfin <- catch.pacfin[catch.pacfin$AGENCY_CODE == "C" & catch.pacfin$PACFIN_YEAR < 2023, ]


# Add column to the PacFIN for ease of processing the data
# ================================================================
pacfin$landing <- pacfin$ROUND_WEIGHT_MTONS
pacfin$year <- pacfin$PACFIN_YEAR
pacfin$gear <- tolower(pacfin$PACFIN_GROUP_GEAR_CODE)
pacfin$cond <- 'dead'
pacfin$cond[pacfin$DISPOSITION_CODE == "F"] <- "live"

pacfin$area <- "north"
# All areas south of Point Conception
south <- c("DNA","HNM","LGB","NWB","OBV", "OLA","OSD","OCN", "OXN","SB","SD","SP","TRM","VEN","WLM")
pacfin$area[which(pacfin$PACFIN_PORT_CODE %in% south)] = "south"

table(pacfin$area, pacfin$SUBREGION_NAME)
table(pacfin$area, pacfin$COUNTY_NAME)

pacfin <- dplyr::left_join(pacfin, wcgop_discard_rate[, c("year", "rate")], by = 'year')
pacfin$rate[is.na(pacfin$rate)] <- hist_discard_rate
pacfin$catch <- pacfin$landing + pacfin$rate*pacfin$landing

pacfin_catch_by_year <- aggregate(catch ~ area + cond + year, pacfin, FUN = sum, drop = FALSE)
pacfin_catch_by_year[is.na(pacfin_catch_by_year)] <- 0

pacfin_landing_by_year <- aggregate(landing ~ area + cond + year, pacfin, FUN = sum, drop = FALSE)
pacfin_landing_by_year[is.na(pacfin_landing_by_year)] <- 0

# =============================================================================
# Process the historical catch data
# =============================================================================

# Set NAs to 0 in the 1969 - 1980 
com_hist_late[is.na(com_hist_late)] = 0

# Convert to mt from pounds and add area total catch column
com_hist_late$landing_mt_south <- 0.000453592 * (com_hist_late$OSB + com_hist_late$OLA + com_hist_late$OSD)
com_hist_late$landing_mt_north <- 0.000453592 * com_hist_late$Grand.Total - com_hist_late$landing_mt_south
com_hist_late$catch_mt_south <- com_hist_late$landing_mt_south + hist_discard_rate*com_hist_late$landing_mt_south
com_hist_late$catch_mt_north <- com_hist_late$landing_mt_north + hist_discard_rate*com_hist_late$landing_mt_north

com_hist_late$catch_live <- 0 

# Early reconstruction catch are grouped into area numbers 0, 2, 4, 5, 6, 8 
# 2 Crescent City, Eureka, Fort Bragg 
# 4 Bodega Bay, San Francisco, Half Moon Bay 
# 5 Santa Cruz, Moss Landing, Monterey 
# 6 Morro Bay, Port San Luis, Santa Barbara  <- needs to be split north and south of Pt. Conception
# 7 Los Angeles <- not in the data for copper rockfish
# 8 San Diego 

# E.J. - For the 2007 cowcod assessment, I went through all the Fish 
# Bulletins and found port-specific landings of total rockfish in 
# the Santa Barbara (SB) area. This allowed me to create an 
# "adjusted SB" estimate of total rockfish landings that excluded 
# landings in Morro Bay and Avila (i.e., SB ports south of Point 
# Conception; see attached Excel sheet). I used ratio estimates to 
# impute missing years.
# 
# To address your question, I calculated the ratio of the "adjusted SB" 
# rockfish landings to the original SB estimates, which we could use as 
# an estimate of the fraction of total rockfish landed south of Point 
# Conception in the SB area (Area 6 in Ralston et al.). I've highlighted 
# this ratio in yellow in the "Table 2" tab of the attached spreadsheet. 
# Rockfish landings weren't reported at the species level, but we could 
# partition the Area 6 estimates in Ralston et al. into N/S or Pt. 
# Conception using these ratios. Many, many assumptions there, but I don't 
# know what else we can do.

# allocate catch in area 0 north and south
com_hist_early$landing_mt_north <- apply(com_hist_early[,c("area2", "area4", "area5")], 1, sum) + 
							(1 - com_ratio[,"Ratio"]) * com_hist_early[,"area6"]
com_hist_early$landing_mt_south = com_hist_early[,"area8"] + com_ratio[,"Ratio"] * com_hist_early[,"area6"]

split <- com_hist_early$landing_mt_south / com_hist_early$landing_mt_north
com_hist_early$landing_mt_south  <- com_hist_early$landing_mt_south + com_hist_early[, "area0"] * split
com_hist_early$landing_mt_north <- com_hist_early$landing_mt_north + com_hist_early[, "area0"] * (1 - split)

# Make sure the split catch equals the catch by area sums
sum(com_hist_early[, 2:7]) == sum(com_hist_early$landing_mt_south + com_hist_early$landing_mt_north)

com_hist_early$catch_mt_south <- com_hist_early$landing_mt_south + com_hist_early$landing_mt_south * hist_discard_rate
com_hist_early$catch_mt_north <- com_hist_early$landing_mt_north + com_hist_early$landing_mt_north * hist_discard_rate

com_hist_early$catch_live <- 0

# ========================================================================
# Combine the catch from each period into area data frames needed for expansions
# ========================================================================

landings_south <- data.frame(
	year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
	dead = c(com_hist_early$landing_mt_south, com_hist_late$landing_mt_south, 
		pacfin_landing_by_year[pacfin_landing_by_year$cond == 'dead' & pacfin_landing_by_year$area == "south", 'landing']),
	live = c(com_hist_early$catch_live, com_hist_late$catch_live, 
		pacfin_landing_by_year[pacfin_landing_by_year$cond == 'live' & pacfin_landing_by_year$area == "south", 'landing'])
)

landings_north <- data.frame(
	year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
	dead = c(com_hist_early$catch_mt_north, com_hist_late$catch_mt_north, 
		pacfin_landing_by_year[pacfin_landing_by_year$cond == 'dead' & pacfin_landing_by_year$area == "north", 'landing']),
	live = c(com_hist_early$catch_live, com_hist_late$catch_live, 
		pacfin_landing_by_year[pacfin_landing_by_year$cond == 'live' & pacfin_landing_by_year$area == "north", 'landing'])
)

write.csv(landings_south, file = file.path(pac_dir, "forSS", "commercial_landings_south_for_expansions.csv"), row.names = FALSE)
write.csv(landings_north, file = file.path(pac_dir, "forSS", "commercial_lanidngs_north_for_expansions.csv"), row.names = FALSE)


catch_south <- data.frame(
  year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
  dead = c(com_hist_early$catch_mt_south, com_hist_late$catch_mt_south, 
           pacfin_catch_by_year[pacfin_catch_by_year$cond == 'dead' & pacfin_catch_by_year$area == "south", 'catch']),
  live = c(com_hist_early$catch_live, com_hist_late$catch_live, 
           pacfin_catch_by_year[pacfin_landing_by_year$cond == 'live' & pacfin_catch_by_year$area == "south", 'catch'])
)

catch_north <- data.frame(
  year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
  dead = c(com_hist_early$catch_mt_north, com_hist_late$catch_mt_north, 
           pacfin_catch_by_year[pacfin_catch_by_year$cond == 'dead' & pacfin_catch_by_year$area == "north", 'catch']),
  live = c(com_hist_early$catch_live, com_hist_late$catch_live, 
           pacfin_catch_by_year[pacfin_catch_by_year$cond == 'live' & pacfin_catch_by_year$area == "north", 'catch'])
)

write.csv(catch_south, file = file.path(pac_dir, "forSS", "commercial_catch_south_for_expansions.csv"), row.names = FALSE)
write.csv(catch_north, file = file.path(pac_dir, "forSS", "commercial_catch_north_for_expansions.csv"), row.names = FALSE)

# ========================================================================
# Format catch for Stock Synthesis
# ========================================================================
ind <- which(
		pacfin_catch_by_year$cond == 'live' & 
		pacfin_catch_by_year$area == "south" &
		pacfin_catch_by_year$catch > 0)


catch_south_ss3 <- data.frame(
	year = c(catch_south$year, pacfin_catch_by_year[ind, 'year']),
	fleet = c(rep('dead', length(catch_south$year)),
			rep('live', length(pacfin_by_year[ind, 'year']))),
	season = 1,
	catch = c(com_hist_early$catch_mt_south, com_hist_late$catch_mt_south, 
		pacfin_catch_by_year[pacfin_by_year$cond == 'dead' & pacfin_catch_by_year$area == "south", 'catch'],
		pacfin_catch_by_year[ind, 'catch']),
	se = 0.01
)

ind <- which(
		pacfin_catch_by_year$cond == 'live' & 
		pacfin_catch_by_year$area == "north" &
		pacfin_catch_by_year$catch > 0)


catch_north_ss3 <- data.frame(
	year = c(catch_north$year, pacfin_catch_by_year[ind, 'year']),
	fleet = c(rep('dead', length(catch_north$year)),
			rep('live', length(pacfin_catch_by_year[ind, 'year']))),
	season = 1,
	catch = c(com_hist_early$catch_mt_north, com_hist_late$catch_mt_north, 
		pacfin_catch_by_year[pacfin_by_year$cond == 'dead' & pacfin_catch_by_year$area == "north", 'catch'],
		pacfin_catch_by_year[ind, 'catch']),
	se = 0.01
)

write.csv(catch_south_ss3, file = file.path(pac_dir, "forSS", "commercial_catch_south_for_ss3.csv"), row.names = FALSE)
write.csv(catch_north_ss3, file = file.path(pac_dir, "forSS", "commercial_catch_north_for_ss3.csv"), row.names = FALSE)

#================================================================
# Plot the catch for each area 
#================================================================
library(ggplot2)

check_conf <- aggregate(VESSEL_ID ~ year + area + cond, pacfin, function(x) length(unique(x)))
min(check_conf[, "VESSEL_ID"])

s <- cbind(catch_south_ss3, "south")
n <- cbind(catch_north_ss3, "north")
colnames(s)[ncol(s)] <- colnames(n)[ncol(n)] <- "area"
df <- rbind(s, n)

ggplot(df, aes(x = year, y = catch, fill = fleet)) +
	geom_bar(stat = 'identity') +
	scale_fill_viridis_d() + 
	facet_grid('area') +
	theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
	ylab("Catch (mt)") + xlab("Year")
ggsave(filename = file.path(pac_dir, "plots", "catch_by_fleet_area_feb_2023.png"), 
      width = 13, height = 10, units = 'in')

ggplot(landings_south_ss3, aes(x = year, y = catch, fill = fleet)) +
	geom_bar(stat = 'identity') +
	scale_fill_viridis_d() + 
	ylab("Catch (mt)") + xlab("Year")
ggsave(filename = file.path(pac_dir, "plots", "catch_south_by_fleet_feb_2023.png"), 
      width = 13, height = 10, units = 'in')

ggplot(catch_north_ss3, aes(x = year, y = catch, fill = fleet)) +
	geom_bar(stat = 'identity') +
	scale_fill_viridis_d() + 
	ylab("Catch (mt)") + xlab("Year")
ggsave(filename = file.path(pac_dir, "plots", "catch_north_by_fleet_feb_2023.png"), 
      width = 13, height = 10, units = 'in')

pacfin$group_gear <- "other"
pacfin$group_gear[pacfin$gear == "hkl"] = "hkl"
check_conf <- aggregate(VESSEL_ID ~ year + area + group_gear, pacfin, function(x) length(unique(x)))
min(check_conf[check_conf$area == "north", "VESSEL_ID"])
min(check_conf[check_conf$area == "south", "VESSEL_ID"])

ggplot(pacfin[pacfin$area == "north", ], aes(x = year, y = catch, fill = group_gear)) +
	geom_bar(stat = 'identity') +
	scale_fill_viridis_d() + 
	ylab("Catch (mt)") + xlab("Year")
