##############################################################################################################
#
# 	Data explorations of landings data for
#	  the 2023 copper rockfish assessment 
#			  by Chantel Wetzel 
#					
##############################################################################################################

dir = "C:/Assessments/2023/copper_rockfish_2023/data/pacfin_catch"
dir.create(file.path(dir, "forSS"))

# Load the data items
load(file.path(dir, "PacFIN.COPP.CompFT.07.Nov.2022.RData"))
com_hist_early <- read.csv(file.path(dir,  "ca_hist_commercial_1916_1968_ej.csv"))
com_hist_late <- read.csv(file.path(dir, "ca_hist_commercial_1969_1980_ej.csv"))
com_ratio <- read.csv(file.path(dir, "historical_landings_tables_2020_revised_Avila_1953.csv"))

# Discard rate based on GEMM analysis look at areas
# south of 40 10
discard_rate <- 0.05

# Filter the PacFIN catches to only CA
# =================================================================
pacfin <- catch.pacfin[catch.pacfin$AGENCY_CODE == "C", ]

# Add column to the PacFIN for ease of processing the data
# ================================================================
pacfin$catch <- pacfin$ROUND_WEIGHT_MTONS
pacfin$year <- pacfin$PACFIN_YEAR
pacfin$gear <- tolower(pacfin$PACFIN_GROUP_GEAR_CODE)
pacfin$cond <- 'dead'
pacfin$cond[pacfin$DISPOSITION_CODE == "F"] <- "live"

pacfin$area <- "north"
# All areas south of Point Conception
south <- c("DNA","HNM","LGB","NWB","OBV", "OLA","OSD","OXN","SB","SD","SP","TRM","VEN","WLM")
pacfin$area[which(pacfin$PACFIN_PORT_CODE %in% south)] = "south"

pacfin_by_year <- aggregate(catch ~ area + cond + year, pacfin[pacfin$year < 2023, ], FUN = sum, drop = FALSE)
pacfin_by_year[is.na(pacfin_by_year)] <- 0

# =============================================================================
# Process the historical catch data
# =============================================================================

# Set NAs to 0 in the 1969 - 1980 
com_hist_late[is.na(com_hist_late)] = 0

# Convert to mt from pounds and add area total catch column
com_hist_late$catch_mt_south <- 0.000453592 * (com_hist_late$OSB + com_hist_late$OLA + com_hist_late$OSD)
com_hist_late$catch_mt_north <- 0.000453592 * com_hist_late$Grand.Total - com_hist_late$catch_mt_south
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

com_hist_early$catch_mt_north <- apply(com_hist_early[,c("area0", "area2", "area4", "area5")], 1, sum) + 
							(1 - com_ratio[,"Ratio"]) * com_hist_early[,"area6"]
com_hist_early$catch_mt_south = com_hist_early[,"area8"] + com_ratio[,"Ratio"] * com_hist_early[,"area6"]
com_hist_early$catch_live <- 0

# Make sure the split catch equals the catch by area sums
sum(com_hist_early[, 2:7]) == sum(com_hist_early$catch_mt_south + com_hist_early$catch_mt_north)

# ========================================================================
# Combine the catch from each period into area data frames needed for expansions
# ========================================================================

landings_south <- data.frame(
	year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
	dead = c(com_hist_early$catch_mt_south, com_hist_late$catch_mt_south, 
		pacfin_by_year[pacfin_by_year$cond == 'dead' & pacfin_by_year$area == "south", 'catch']),
	live = c(com_hist_early$catch_live, com_hist_late$catch_live, 
		pacfin_by_year[pacfin_by_year$cond == 'live' & pacfin_by_year$area == "south", 'catch'])
)

landings_north <- data.frame(
	year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
	dead = c(com_hist_early$catch_mt_north, com_hist_late$catch_mt_north, 
		pacfin_by_year[pacfin_by_year$cond == 'dead' & pacfin_by_year$area == "north", 'catch']),
	live = c(com_hist_early$catch_live, com_hist_late$catch_live, 
		pacfin_by_year[pacfin_by_year$cond == 'live' & pacfin_by_year$area == "north", 'catch'])
)

write.csv(landings_south, file = file.path(dir, "forSS", "commercial_landings_south_for_expansions.csv"), row.names = FALSE)
write.csv(landings_north, file = file.path(dir, "forSS", "commercial_landings_north_for_expansions.csv"), row.names = FALSE)

# Add discard assumptions
catch_south <- data.frame(
	year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
	dead = discard_rate * c(com_hist_early$catch_mt_south, com_hist_late$catch_mt_south, 
		pacfin_by_year[pacfin_by_year$cond == 'dead' & pacfin_by_year$area == "south", 'catch']),
	live = discard_rate * c(com_hist_early$catch_live, com_hist_late$catch_live, 
		pacfin_by_year[pacfin_by_year$cond == 'live' & pacfin_by_year$area == "south", 'catch'])
)

catch_north <- data.frame(
	year = c(com_hist_early$Year, com_hist_late$Year, sort(unique(pacfin$year))),
	dead = discard_rate * c(com_hist_early$catch_mt_north, com_hist_late$catch_mt_north, 
		pacfin_by_year[pacfin_by_year$cond == 'dead' & pacfin_by_year$area == "north", 'catch']),
	live = discard_rate * c(com_hist_early$catch_live, com_hist_late$catch_live, 
		pacfin_by_year[pacfin_by_year$cond == 'live' & pacfin_by_year$area == "north", 'catch'])
)

write.csv(catch_south, file = file.path(dir, "forSS", "commercial_catch_5percent_discard_south_for_expansions.csv"), row.names = FALSE)
write.csv(catch_north, file = file.path(dir, "forSS", "commercial_catch_5percent_discard_north_for_expansions.csv"), row.names = FALSE)

# ========================================================================
# Format catch for Stock Synthesis
# ========================================================================
ind <- which(
		pacfin_by_year$cond == 'live' & 
		pacfin_by_year$area == "south" &
		pacfin_by_year$catch > 0)


landings_south_ss3 <- data.frame(
	year = c(landings_south$year, pacfin_by_year[ind, 'year']),
	fleet = c(rep('dead', length(landings_south$year)),
			rep('live', length(pacfin_by_year[ind, 'year']))),
	season = 1,
	catch = c(com_hist_early$catch_mt_south, com_hist_late$catch_mt_south, 
		pacfin_by_year[pacfin_by_year$cond == 'dead' & pacfin_by_year$area == "south", 'catch'],
		pacfin_by_year[ind, 'catch']),
	se = 0.01
)

ind <- which(
		pacfin_by_year$cond == 'live' & 
		pacfin_by_year$area == "north" &
		pacfin_by_year$catch > 0)


landings_north_ss3 <- data.frame(
	year = c(landings_north$year, pacfin_by_year[ind, 'year']),
	fleet = c(rep('dead', length(landings_north$year)),
			rep('live', length(pacfin_by_year[ind, 'year']))),
	season = 1,
	catch = c(com_hist_early$catch_mt_north, com_hist_late$catch_mt_north, 
		pacfin_by_year[pacfin_by_year$cond == 'dead' & pacfin_by_year$area == "north", 'catch'],
		pacfin_by_year[ind, 'catch']),
	se = 0.01
)

write.csv(landings_south_ss3, file = file.path(dir, "forSS", "commercial_landings_south_for_ss3.csv"), row.names = FALSE)
write.csv(landings_north_ss3, file = file.path(dir, "forSS", "commercial_landings_north_for_ss3.csv"), row.names = FALSE)

catch_north_ss3 <- landings_north_ss3
catch_north_ss3$catch <- catch_north_ss3$catch * discard_rate
catch_south_ss3 <- landings_south_ss3
catch_south_ss3$catch <- catch_south_ss3$catch * discard_rate

write.csv(catch_south_ss3, file = file.path(dir, "forSS", "commercial_catch_south_for_ss3.csv"), row.names = FALSE)
write.csv(catch_north_ss3, file = file.path(dir, "forSS", "commercial_catch_north_for_ss3.csv"), row.names = FALSE)

#================================================================
# Plot the catch for each area 
#================================================================
library(ggplot2)

check_conf <- aggregate(VESSEL_ID ~ year + area + cond, pacfin, function(x) length(unique(x)))
min(check_conf[, "VESSEL_ID"])

s <- cbind(landings_south_ss3, "south")
n <- cbind(landings_north_ss3, "north")
colnames(s)[ncol(s)] <- colnames(n)[ncol(n)] <- "area"
df <- rbind(s, n)

ggplot(df, aes(x = year, y = catch, fill = fleet)) +
	geom_bar(stat = 'identity') +
	scale_fill_viridis_d() + 
	facet_grid('area') +
	ylab("Catch (mt)") + xlab("Year")
ggsave(filename = file.path(dir, "plots", "landings_by_fleet_area.png"), 
      width = 13, height = 10, units = 'in')

ggplot(landings_south_ss3, aes(x = year, y = catch, fill = fleet)) +
	geom_bar(stat = 'identity') +
	scale_fill_viridis_d() + 
	ylab("Catch (mt)") + xlab("Year")
ggsave(filename = file.path(dir, "plots", "landings_south_by_fleet.png"), 
      width = 13, height = 10, units = 'in')

ggplot(landings_north_ss3, aes(x = year, y = catch, fill = fleet)) +
	geom_bar(stat = 'identity') +
	scale_fill_viridis_d() + 
	ylab("Catch (mt)") + xlab("Year")
ggsave(filename = file.path(dir, "plots", "landings_north_by_fleet.png"), 
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
