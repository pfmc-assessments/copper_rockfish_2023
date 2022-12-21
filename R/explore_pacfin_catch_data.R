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

# Do a quick check for confidentiality
conf_check <- aggregate(VESSEL_ID ~ year + area, pacfin, function(x) length(unique(x)))
min(conf_check$VESSEL_ID) # 12 by area - good to go

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

ggplot(pacfin, aes(x = year, y = catch, fill = cond)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "pacfin_catch", "plots", "catch_by_area_cond.png"), 
      width = 13, height = 10, units = 'in')

catch_cond <- aggregate(catch~year + area + cond, pacfin, sum, drop = FALSE)
percent_dead_n <- 
	catch_cond[catch_cond$area == "north" & catch_cond$cond == "dead", "catch"] / 
	(catch_cond[catch_cond$area == "north" & catch_cond$cond == "dead", "catch"] + catch_cond[catch_cond$area == "north" & catch_cond$cond == "live", "catch"])
percent_dead_s <- 
	catch_cond[catch_cond$area == "south" & catch_cond$cond == "dead", "catch"] / 
	(catch_cond[catch_cond$area == "south" & catch_cond$cond == "dead", "catch"] + catch_cond[catch_cond$area == "south" & catch_cond$cond == "live", "catch"])

png(filename = file.path(dir, "pacfin_catch", "plots", "percent_dead_landings.png"), width = 7, height = 7, units = 'inches')
plot(sort(unique(catch_cond$year)), percent_dead_n, type = 'b',
	ylab = "Proportion of Fish Landed Dead ", xlab = "Year", ylim = c(0,1))
lines(sort(unique(catch_cond$year)), percent_dead_s, lty = 2)
points(sort(unique(catch_cond$year)), percent_dead_s, pch = 17)
legend("topleft", bty = 'n', legend = c("North", "South"), 
	lty = 1:2, pch = c(1, 17))
dev.off()

out <- cbind(sort(unique(catch_cond$year)), percent_dead_n, percent_dead_s)
colnames(out) <- c("year", "percent_dead_north", "percent_dead_south") 
write.csv(out, file = file.path(dir, "pacfin_catch", "percent_landed_dead.csv"))

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

# The trawl catch in the early years are associated
# with a larget number of unique fish tickets
# year  area FISH_TICKET_ID
# 1982 north            584 - 3.8 mt landed from 51 unique vessels
# 1983 north            492 - 26.2 mt from 42 vessels
# 1984 north            326 - 15.8 mt from 28 vessels
# 1985 north            393 - 8.9 mt from 45 vessels
# 1982 south            183 - 0.05 mt from 22 vessels
# 1983 south            204 - 0.12 mt from 21 vessels
# 1984 south             89 - 0.11 mt from 20 vessels
find <- which(pacfin$gear == "twl" & pacfin$year %in% 1982:1985)
twl <- pacfin[find, ]

# table(twl$MARKET_CATEGORY_NAME, twl$year)
#                                       
#                                        1982 1983 1984 1985
#   ROCKFISH, GROUP BOCACCIO/CHILIPEPPER    0   18    0    0
#   ROCKFISH, GROUP BOLINA (BROWN)         34  414  325  161
#   ROCKFISH, GROUP RED                     1    5    0    0
#   ROCKFISH, GROUP ROSEFISH                0    5    0    0
#   ROCKFISH, NOM. BOCACCIO               112   95    0    0
#   ROCKFISH, NOM. BROWN                    1    5    0    0
#   ROCKFISH, NOM. COPPER                   0    4    2    2
#   ROCKFISH, NOM. COWCOD                   8    5    0    0
#   ROCKFISH, UNSPECIFIED                 952  212   91  244

aggregate(VESSEL_ID ~ LANDING_MONTH + MARKET_CATEGORY_NAME, twl, function(x) length(unique(x)))
# Catches from trawl occurring across all months
ggplot(twl, aes(x = LANDING_MONTH, y = catch)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "pacfin_catch", "plots", "twl_catch_by_area_port.png"), 
      width = 13, height = 10, units = 'in')

aggregate(VESSEL_ID ~ year + PACFIN_GROUP_PORT_CODE, twl, function(x) length(unique(x)))
ggplot(twl, aes(x = year, y = catch, fill = PACFIN_GROUP_PORT_CODE)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "pacfin_catch", "plots", "twl_catch_by_year_port.png"), 
      width = 13, height = 10, units = 'in')


ggplot(twl, aes(x = year, y = catch, fill = MARKET_CATEGORY_NAME)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "pacfin_catch", "plots", "CONFIDENTIAL_twl_catch_by_area_market_cat.png"), 
      width = 13, height = 10, units = 'in')

ggplot(pacfin, aes(x = year, y = catch, fill = gear)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "cond")) +
	scale_fill_viridis_d()

ggsave(filename = file.path(dir, "pacfin_catch", "plots", "catch_by_area_cond_gear.png"), 
      width = 13, height = 10, units = 'in')

sample_by_year_cond <- aggregate(catch ~ year + area + cond, pacfin, sum, drop = FALSE)
sample_by_year_cond[is.na(sample_by_year_cond)] = 0
write.csv(sample_by_year_cond, file = file.path(dir, "pacfin_catch", "landings_by_cond_area.csv"))
total_by_year <- aggregate(catch ~ year + area, pacfin, sum, drop = FALSE)
total_by_year[is.na(total_by_year)] = 0

total_dead_area <-  aggregate(catch ~ year + area, sample_by_year_cond[sample_by_year_cond$cond == "dead",], sum, drop = FALSE)
prop <- data.frame(
	year = total_dead_area[total_dead_area$year > 2000,'year'], 
	area = total_dead_area[total_dead_area$year > 2000, 'area'], 
	prop_dead = total_dead_area[total_dead_area$year > 2000, 'catch'] / total_by_year[total_by_year$year > 2000, 'catch'])

ggplot(prop, aes(y = prop_dead, x = year)) + 
    geom_bar(position="stack", stat="identity") + 
    xlab("Year") + ylab("Proportion of Catch from Dead Fish") +
    facet_wrap(facets = "area") 
ggsave(filename = file.path(dir, "pacfin_catch", "plots", "prop_catch_dead_fish_by_area.png"),
	width = 14, height = 7)


# Are there specific ports that see a higher proportion of live fish landed?
aggregate(VESSEL_NAME~PACFIN_GROUP_PORT_CODE + year, pacfin, length)
# No confidential issues when plotted by port group but cannot show the
# landings by port and year due to < 3 vessels

ggplot(pacfin, aes(x = PACFIN_GROUP_PORT_CODE, y = catch, fill = cond)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	xlab("Port Group") + ylab("Landings") +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "pacfin_catch", "plots", "catch_by_cond_area.png"), 
      width = 13, height = 10, units = 'in')

ggplot(pacfin, aes(y = catch, x = year, fill = PACFIN_GROUP_PORT_CODE)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "cond")) + 
	xlab("Year") + ylab("Landings") 
ggsave(filename = file.path(dir, "pacfin_catch", "plots", "CONFIDENTIAL_catch_by_cond_port_year.png"), 
      width = 13, height = 10, units = 'in')

# Find where the pot caught live fish are being landed
ind = which(pacfin$gear == "pot" & pacfin$cond == "live")
aggregate(VESSEL_ID~PACFIN_GROUP_PORT_CODE + area, pacfin[ind,], function(x) length(unique(x)))
# CONFIDENTIAL VIEW - 
ggplot(pacfin[ind,], aes(y = catch, x = year, fill = PACFIN_GROUP_PORT_CODE)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) + 
	xlab("Port") + ylab("Pot Live Landings") 
# MRA (morro bay area) and MNA (monterey bay) ports are the
# primary ports in recent years peaking ~ 0.2 mt / yr (441 lbs.)


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
# There area 17 records with no catch recorded
ind = which(is.na(mrfss$catch_mt))
mrfss[ind, ]
# There is information in the TOT_CAT column which I think might
# be numbers of fish? Could explore ways to calculate catch weight
# from this information

plot(mrfss$catch_mt, mrfss$TOT_CAT)
plot(mrfss$WGT_AB1 / mrfss$TOT_CAT)
quantile(mrfss$WGT_AB1 / mrfss$TOT_CAT, na.rm = TRUE)
#       0%      25%      50%      75%     100% 
# 0.050000 0.565000 0.835000 1.177778 3.400000

table(mrfss$SOURCE_AREA_NAME) 
#   INLAND OCEAN (<= 3 MI)  OCEAN (> 3 MI)         UNKNOWN 
# 1     62             425             300              28 

# I think the inland records should be removed similar to how the
# crfss data was pulled from RecFIN. Going to retain the catch
# records from unknown areas for now.
aggregate(mrfss$catch_mt~SOURCE_AREA_NAME, mrfss, sum)
# SOURCE_AREA_NAME mrfss$catch_mt
#           INLAND       18.80436
#  OCEAN (<= 3 MI)     2122.32071
#   OCEAN (> 3 MI)     1192.01276
#          UNKNOWN       27.69313

mrfss <- mrfss[!mrfss$SOURCE_AREA_NAME %in% c("INLAND", ""), ]

ggplot(mrfss, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "WAVE")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Catch (mt)")

ggplot(mrfss, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Catch (mt)")

ggsave(filename = file.path(dir, "rec_catch", "plots", "mrfss_catch_mt_by_area_mode.png"), 
      width = 13, height = 10, units = 'in')

ggplot(mrfss, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "SOURCE_AREA_NAME")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Catch (mt)")
ggsave(filename = file.path(dir, "rec_catch", "plots", "mrfss_catch_mt_by_area_source_area_name.png"), 
      width = 13, height = 10, units = 'in')