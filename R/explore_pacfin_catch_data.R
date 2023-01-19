##############################################################################################################
#
# 	Data explorations of commercial landings data for
#	  the 2023 copper rockfish assessment 
#			  by Chantel Wetzel 
#					
##############################################################################################################

dir = "C:/Assessments/2023/copper_rockfish_2023/data/pacfin_catch"
library(ggplot2)

load(file.path(dir, "PacFIN.COPP.CompFT.07.Nov.2022.RData"))
com_hist_early <- read.csv(file.path(dir, "ca_hist_commercial_1916_1968_ej.csv"))
com_hist_late <- read.csv(file.path(dir, "ca_hist_commercial_1969_1980_ej.csv"))
hist_ratio <- read.csv(file.path(dir, "historical_landings_tables_2020_revised_Avila_1953.csv"))

# Filter the PacFIN catches to only CA
pacfin <- catch.pacfin[catch.pacfin$AGENCY_CODE == "C", ]

# ==========================================================================================
# Commercial Data Issues:
# The historical 1916 -1968 commercial catches are also not 
# split correctly north and south of point conception. 
# The historical analysis using Avila data provides ratios
# of how to shift catches between areas. 
# ===========================================================================================

# ===============================================================================
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
ggsave(filename = file.path(dir, "plots", "catch_by_area_cond.png"), 
      width = 13, height = 10, units = 'in')

catch_cond <- aggregate(catch~year + area + cond, pacfin, sum, drop = FALSE)
percent_dead_n <- 
	catch_cond[catch_cond$area == "north" & catch_cond$cond == "dead", "catch"] / 
	(catch_cond[catch_cond$area == "north" & catch_cond$cond == "dead", "catch"] + catch_cond[catch_cond$area == "north" & catch_cond$cond == "live", "catch"])
percent_dead_s <- 
	catch_cond[catch_cond$area == "south" & catch_cond$cond == "dead", "catch"] / 
	(catch_cond[catch_cond$area == "south" & catch_cond$cond == "dead", "catch"] + catch_cond[catch_cond$area == "south" & catch_cond$cond == "live", "catch"])

png(filename = file.path(dir, "plots", "percent_dead_landings.png"), width = 7, height = 7, units = 'inches')
plot(sort(unique(catch_cond$year)), percent_dead_n, type = 'b',
	ylab = "Proportion of Fish Landed Dead ", xlab = "Year", ylim = c(0,1))
lines(sort(unique(catch_cond$year)), percent_dead_s, lty = 2)
points(sort(unique(catch_cond$year)), percent_dead_s, pch = 17)
legend("topleft", bty = 'n', legend = c("North", "South"), 
	lty = 1:2, pch = c(1, 17))
dev.off()

out <- cbind(sort(unique(catch_cond$year)), percent_dead_n, percent_dead_s)
colnames(out) <- c("year", "percent_dead_north", "percent_dead_south") 
write.csv(out, file = file.path(dir, "percent_landed_dead.csv"))

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
ggsave(filename = file.path(dir, "plots", "twl_catch_by_area_port.png"), 
      width = 13, height = 10, units = 'in')

aggregate(VESSEL_ID ~ year + PACFIN_GROUP_PORT_CODE, twl, function(x) length(unique(x)))
ggplot(twl, aes(x = year, y = catch, fill = PACFIN_GROUP_PORT_CODE)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "twl_catch_by_year_port.png"), 
      width = 13, height = 10, units = 'in')


ggplot(twl, aes(x = year, y = catch, fill = MARKET_CATEGORY_NAME)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "CONFIDENTIAL_twl_catch_by_area_market_cat.png"), 
      width = 13, height = 10, units = 'in')


conf_check <- aggregate(VESSEL_ID ~ year + area + cond + gear, pacfin, function(x) length(unique(x)))
min(conf_check$VESSEL_ID) # 1 so the following fig is confidential by CDFW rules

ggplot(pacfin, aes(x = year, y = catch, fill = gear)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "cond")) +
	scale_fill_viridis_d()

ggsave(filename = file.path(dir, "plots", "CONFIDENTIAL_catch_by_area_cond_gear.png"), 
      width = 13, height = 10, units = 'in')

# There are issues across gear types (pot, twl, tws, net, msc) the
# have only one vessel by area and gear
conf_check <- aggregate(VESSEL_ID ~ area + gear, pacfin, function(x) length(unique(x)))
min(conf_check$VESSEL_ID) 
# To get around this attempt to block by time period and some
# gear types
pacfin$gear_group <- pacfin$gear
pacfin$gear_group[pacfin$gear %in% c('msc', 'net', 'tls', 'tws')] <- "msc_net_tls_tws"
conf_check <- aggregate(VESSEL_ID ~ area + gear_group, pacfin, function(x) length(unique(x)))
min(conf_check$VESSEL_ID)
pacfin$year_block <- NA
pacfin$year_block[pacfin$year %in% 1980:1989] <- "1980s"
pacfin$year_block[pacfin$year %in% 1990:1999] <- "1990s"
pacfin$year_block[pacfin$year %in% 2000:2009] <- "2000s"
pacfin$year_block[pacfin$year %in% 2010:2022] <- "2010+"
conf_check <- aggregate(VESSEL_ID ~ year_block + area + gear_group , pacfin, function(x) length(unique(x)))
min(conf_check$VESSEL_ID)

ggplot(pacfin, aes(x = year_block, y = catch, fill = gear_group)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.x = element_text(size = 14)) +
	xlab("Year Blocks") + ylab("Landings (mt)") +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "landings_by_area_year_block_gear_group.png"), 
      width = 13, height = 10, units = 'in')


sample_by_year_cond <- aggregate(catch ~ year + area + cond, pacfin, sum, drop = FALSE)
sample_by_year_cond[is.na(sample_by_year_cond)] = 0
write.csv(sample_by_year_cond, file = file.path(dir, "landings_by_cond_area.csv"))
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
ggsave(filename = file.path(dir, "plots", "prop_catch_dead_fish_by_area.png"),
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
ggsave(filename = file.path(dir, "plots", "catch_by_cond_area.png"), 
      width = 13, height = 10, units = 'in')

ggplot(pacfin, aes(y = catch, x = year, fill = PACFIN_GROUP_PORT_CODE)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "cond")) + 
	xlab("Year") + ylab("Landings") 
ggsave(filename = file.path(dir, "plots", "CONFIDENTIAL_catch_by_cond_port_year.png"), 
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


