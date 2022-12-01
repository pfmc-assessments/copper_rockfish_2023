##############################################################################################################
#
# 	Data explorations of landings data for
#	  the 2023 copper rockfish assessment 
#			  by Chantel Wetzel 
#					
##############################################################################################################

dir = "C:/Assessments/2023/copper_rockfish_2023/data"
library(ggplot2)

# ===================================================================
# Load all data from recent years
load(file.path(dir, "pacfin_catch", "PacFIN.COPP.CompFT.07.Nov.2022.RData"))
load(file.path(dir, "rec_catch", "mrfss_catch_filtered.rdata"))
load(file.path(dir, "rec_catch", "crfss_catch_filtered.rdata"))
# objects are catch.pacfin, mrfss, crfss

# Read the historical catches for both commercial and recreational
rec_hist <- read.csv(file.path(dir, "rec_catch", "ca_hist_recreational_1928_1980_ej.csv"))
com_hist_early <- read.csv(file.path(dir, "pacfin_catch", "ca_hist_commercial_1916_1968_ej.csv"))
com_hist_late <- read.csv(file.path(dir, "pacfin_catch", "ca_hist_commercial_1969_1980_ej.csv"))
hist_ratio <- read.csv(file.path(dir, "pacfin_catch", "historical_landings_tables_2020_revised_Avila_1953.csv"))

# Filter the PacFIN catches to only CA
pacfin <- catch.pacfin[catch.pacfin$AGENCY_CODE == "C", ]

# ==========================================================================================
# Recreational Data Issues:
# California - MRFSS years were 1980 - 1989 split wrong, 
# need to move catches from south to north of pt conception
# 1980 should be replaced with historical reconstruction
# Missing 1990-1992 catches - need to linear interpolate
# 1993 - 2004 CRFSS years which are split north and south correctly at pt. concep
#
# Commercial Data Issues:
# The historical 1916 -1968 commercial catches are also not 
# split correctly north and south of point conception. 
# The historical analysis using Avila data provides ratios
# of how to shift catches between areas. 
# ===========================================================================================

# ===============================================================================
# Start with the commercial catches in PacFIN
# Determine the area, live vs dead, and gear
# ===============================================================================
pacfin$catch <- pacfin$ROUND_WEIGHT_MTONS
pacfin$year <- pacfin$PACFIN_YEAR
pacfin$gear <- tolower(pacfin$PACFIN_GROUP_GEAR_CODE)
pacfin$cond <- 'dead'
pacfin$cond[pacfin$DISPOSITION_CODE == "F"] <- "live"

pacfin$area <- "north"
# All areas south of Point Conception
south <- c("DNA","HNM","LGB","NWB","OBV", "OLA","OSD","OXN","SB","SD","SP","TRM","VEN","WLM")
pacfin$area[which(pacfin$PACFIN_PORT_CODE %in% south)] = "south"

table(pacfin$area, pacfin$cond)
#            B       F      H      I      P      S      U
#  north      1  35856 102000      1    632     14      2
#  south      3   1248  23630      3    353      1      0
#DISPOSITION_CODE in PacFIN                                                                   
#             B      bait                                                                                                                      
#             F      landed live (human food eventually)                                             
#             H      human food                                                                      
#             I      investigation (research)                                                                                                              
#             P      personal use                                                                                                                                       
#             S      seized (illegal)                                                                                                                                   
#             U      unspecified 

aggregate(catch ~ cond + area, pacfin, function(x) round(sum(x),1))                                                                    
# Nearly all removals are from the live of human food fishery
# 2/3 of commercial catches are occuring north of point conception
# 17% of mortality in the north is from the live fishery (133.1 / 788.6)
# 20% of mortality in the south is from the live fishery (53.3 / 263.8)

# Look at the catch by gear
aggregate(catch ~ gear + area + cond, pacfin, function(x) round(sum(x),1))
#    gear  area catch
#     hkl north 682.9 (130.6 live)
#     msc north   0.8
#     net north  31.5
#     pot north   3.0
#     tls north   5.4
#     twl north  65.0
#     tws north   0.0
#     hkl south 252.2 (52.1 live)
#     msc south   0.1
#     net south   8.1
#     pot south   2.0
#     tls south   0.0
#     twl south   1.1
#     tws south   0.3

ggplot(pacfin, aes(x = year, y = catch, fill = gear)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "cond")) +
	scale_fill_viridis_d()

ggsave(filename = file.path(dir, "pacfin_catch", "plots", "catch_by_area_cond_gear.png"), 
      width = 13, height = 10, units = 'in')

# ===============================================================================
# Switch to looking at CRFSS era catches
# Determine the area and mode
# ===============================================================================

crfss$year <- crfss$RECFIN_YEAR
crfss$catch_mt <- crfss$TOTAL_MORTALITY_MT
crfss$catch_num <- crfss$TOTAL_MORTALITY_NUM

# Confirm the area assignment
table(crfss$RECFIN_SUBREGION_NAME, crfss$area)

aggregate(catch_mt ~ area + mode, crfss, function(x) round(sum(x),1) )
#  area      mode catch_mt
# north      cpfv    370.4
# south      cpfv    664.7
# north   private    484.7
# south   private    158.6
# north shoreside      5.0
# south shoreside      0.4

ggplot(crfss, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Catch (mt)")

ggsave(filename = file.path(dir, "rec_catch", "plots", "crfss_catch_mt_by_area_mode.png"), 
      width = 13, height = 10, units = 'in')

ggplot(crfss, aes(x = year, y = catch_num / 1000, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Catch (1000s of fish)")

ggsave(filename = file.path(dir, "rec_catch", "plots", "crfss_catch_num_by_area_mode.png"), 
      width = 13, height = 10, units = 'in')

# Look at the RecFIN water area records by area
table(crfss$RECFIN_WATER_AREA_NAME, crfss$area)
#                  north south
#  OCEAN <= 3 MILES  1490  1478
#  OCEAN > 3 MILES    610   864

ggplot(crfss, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "RECFIN_WATER_AREA_NAME")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Catch (mt)")

ggsave(filename = file.path(dir, "rec_catch", "plots", "crfss_catch_mt_by_area_water_area.png"), 
      width = 13, height = 10, units = 'in')

ggplot(crfss, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "RECFIN_MONTH")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Catch (mt)")

ggsave(filename = file.path(dir, "rec_catch", "plots", "crfss_catch_mt_by_month_area.png"), 
      width = 13, height = 10, units = 'in')

# ===============================================================================
# Switch to looking at MRFSS era catches
# Determine the area and mode
# ===============================================================================

mrfss$year <- mrfss$YEAR_
mrfss$catch_mt <- 0.001 * mrfss$WGT_AB1 # convert from kg to mt

SOURCE_AREA_NAME 