##############################################################################################################
#
# 	Data explorations of recreational landings data for
#	  the 2023 copper rockfish assessment 
#			  by Chantel Wetzel 
#					
##############################################################################################################

dir <- here::here("data", "rec_catch")
library(ggplot2)

load(file.path(dir, "mrfss_catch_filtered.rdata"))
load(file.path(dir, "crfss_catch_filtered_march_2023.rdata"))
# Objects are called mrfss and crfs

# Read the historical landings
rec_hist <- read.csv(file.path(dir, "ca_hist_recreational_1928_1980_ej.csv"))

# Recreational Data Issues:
# California - MRFSS years were 1980 - 1989 split wrong, 
# need to move catches from south to north of pt conception
# 1980 should be replaced with historical reconstruction
# Missing 1990-1992 catches - need to linear interpolate
# 1993 - 2004 CRFSS years which are split north and south correctly at pt. concep

# ===============================================================================
# Switch to looking at CRFSS era catches
# Determine the area and mode
# ===============================================================================

crfs$catch_mt <- crfs$TOTAL_MORTALITY_MT
crfs$catch_num <- crfs$TOTAL_MORTALITY_NUM

crfs$ports <- sapply(strsplit(crfs$RECFIN_PORT_NAME, '\\s*[()]'), '[',1)

# Confirm the area assignment
table(crfs$RECFIN_SUBREGION_NAME, crfs$area)

# Look for NAs
find <- which(is.na(crfs$catch_mt))
length(find)

aggregate(catch_mt ~ area + mode, crfs, function(x) round(sum(x),1) )
#  area      mode catch_mt
# north      cpfv    370.4
# south      cpfv    664.7
# north   private    484.7
# south   private    158.6
# north shoreside      5.0
# south shoreside      0.4

ggplot(crfs, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Landings (mt)")

ggsave(filename = file.path(dir, "plots", "crfs_landings_mt_by_area_mode.png"), 
      width = 13, height = 10, units = 'in')

ggplot(crfs, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "RECFIN_MONTH"), ncol = 6) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Landings (mt)")

ggsave(filename = file.path(dir, "plots", "crfs_landings_mt_by_area_mode_month.png"), 
      width = 13, height = 10, units = 'in')

ggplot(crfs, aes(x = year, y = catch_num / 1000, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Landings (1,000s of fish)")

ggsave(filename = file.path(dir, "plots", "crfs_landings_num_by_area_mode.png"), 
      width = 13, height = 10, units = 'in')

# Look at the RecFIN water area records by area
table(crfs$RECFIN_WATER_AREA_NAME, crfs$area)
#                  north south
#  OCEAN <= 3 MILES  1490  1478
#  OCEAN > 3 MILES    610   864

ggplot(crfs, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "RECFIN_WATER_AREA_NAME")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Landings (mt)")

ggsave(filename = file.path(dir, "plots", "crfs_landings_mt_by_area_water_area.png"), 
      width = 13, height = 10, units = 'in')

ggplot(crfs, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "RECFIN_MONTH")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Landings (mt)")

ggsave(filename = file.path(dir, "plots", "crfs_landings_mt_by_month_area.png"), 
      width = 13, height = 10, units = 'in')

ggplot(crfs[crfs$year >2015, ], aes(x = RECFIN_MONTH, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "year"), ncol = 3) +
	scale_fill_viridis_d() +
	xlab("Month") + ylab("Landings (mt)")
ggsave(filename = file.path(dir, "plots", "crfs_landings_mt_by_year_area_mong.png"), 
      width = 13, height = 10, units = 'in')

aggregate(catch_mt ~ year + area + RECFIN_MONTH, crfs[crfs$year > 2019, ], sum)

# ===============================================================================
# Switch to looking at MRFSS era catches
# Determine the area and mode
# ===============================================================================

# Convert from kg to mt
mrfss$orig_catch_mt <- 0.001 * mrfss$WGT_AB1 
mrfss$catch_mt <- mrfss$orig_catch_mt
aggregate(orig_catch_mt ~ year + area, mrfss, sum)

# There area 26 records with no catch recorded
ind <- which(is.na(mrfss$catch_mt))
# The ESTWGT is also listed as EST. WGT OF A CATCH (KGS)
mrfss[ind, c("WGT_AB1", "ESTWGT")]

# There is information in the LANDING column that is the 
# numbers of fish EST. A + B1 CATCH IN NUMBERS. Also, the 
# TOT_CAT column contains EST. TOTAL CATCH IN NUMBERS.
mrfss[ind, c('LANDING', 'TOT_CAT')]
plot(mrfss$LANDING, mrfss$TOT_CAT)

table(mrfss[ind, 'year'], mrfss[ind, 'area'])

mrfss$ave_wgt_kg <- mrfss$WGT_AB1 / mrfss$TOT_CAT
mrfss$ave_wgt_mt <- 0.001 * mrfss$ave_wgt_kg
ave_wgt <- aggregate(ave_wgt_mt ~ year + area, mrfss, mean)
ggplot(ave_wgt, aes(x = year, y = ave_wgt_mt)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area"))
ggsave(filename = file.path(dir, "plots", "mrfss_ave_fish_weight.png"), 
      width = 13, height = 10, units = 'in')

# Fill in using the TOT_CAT column and average fish weight
# by area and year
ind <- which(is.na(mrfss$catch_mt))
for (i in ind){
	y <- mrfss[i, 'year']
	a <- mrfss[i, 'area']
	key <- which(ave_wgt$year == y & ave_wgt$area == a)
	mrfss$catch_mt[i] <- 
		mrfss$TOT_CAT[i] * ave_wgt[key, 'ave_wgt_mt']
}
zero <- which(mrfss$catch_mt == 0)
mrfss[zero, ]

table(mrfss$SOURCE_AREA_NAME) 
#    INLAND OCEAN (<= 3 MI)  OCEAN (> 3 MI)  UNKNOWN 
# 1       54             406             274       22
# I think the inland records should be removed similar to how the
# crfs data was pulled from RecFIN. Going to retain the catch
# records from unknown areas for now.
aggregate(mrfss$catch_mt ~ SOURCE_AREA_NAME, mrfss, sum)
# SOURCE_AREA_NAME mrfss$catch_mt
#                        0.000000
#           INLAND      21.759181
#  OCEAN (<= 3 MI)    2082.442101
#   OCEAN (> 3 MI)     880.774803
#          UNKNOWN       8.158615

# Remove the inland and unknow area records
mrfss <- mrfss[!mrfss$SOURCE_AREA_NAME %in% c("INLAND", ""), ]

df <- aggregate(catch_mt ~ year + area + mode + WAVE, mrfss, sum, drop = FALSE)
df[is.na(df)] <- 0

# Move landings in the south to the north for 1980 - 1989: ratio of 0.317 based on Albin et al. 1993 
# See Allocate_MRFSS_1980-1989.xls to see how calculated.
# The calculation is based on numbers of fish so may want to consider
# how to use this if we are using landings weight
factor <- 0.317
n <- which(df$year %in% c(1980:1989) & df$area == "north")
s <- which(df$year %in% c(1980:1989) & df$area == "south")
df$catch_mt[n] <- df[n, "catch_mt"] + df[s, "catch_mt"] * factor 
df$catch_mt[s] <- df[s, "catch_mt"] - df[s, "catch_mt"] * factor

ggplot(df, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area", "WAVE"), ncol = 6) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Landings (mt)")
ggsave(filename = file.path(dir, "plots", "mrfss_landings_mt_by_area_wave_mode.png"), 
      width = 14, height = 7, units = 'in')

ggplot(df, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) +
	scale_fill_viridis_d() +
	xlab("Year") + ylab("Landings (mt)")
ggsave(filename = file.path(dir, "plots", "mrfss_landings_mt_by_area_mode.png"), 
      width = 13, height = 10, units = 'in')

ind = which(crfs$area == "south", crfs$mode == "cpfv")
tmp = aggregate(catch_mt ~year + ports, crfs[ind, ], sum)

plot(tmp$year[tmp$ports == "SOUTH"], tmp$catch_mt[tmp$ports == "SOUTH"], type = 'b', col = 'red', ylim = c(0, 100))
lines(tmp$year[tmp$ports == "CHANNEL"], tmp$catch_mt[tmp$ports == "CHANNEL"], lty =  2, col = 'blue')
