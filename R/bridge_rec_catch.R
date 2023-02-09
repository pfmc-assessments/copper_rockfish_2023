##############################################################################################################
#
# 	Purpose: Process CA Catches for model bridging
#
#			  by Chantel Wetzel 
#
##############################################################################################################
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/PacFIN.Utilities")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021")
library(HandyCode)
library(dplyr)
options(stringsAsFactors = FALSE)

dir = "E:/Assessments/2021/copper_rockfish_2021/data/catches"
save_dir = "C:/Assessments/2023/copper_rockfish_2023/data/bridging"

#-----------------------------------------------------------------------------------
# Load the Recreational Data
#-----------------------------------------------------------------------------------

# California - has both mt and fish numbers (but the fish numbers have decimals)
ca_rec = read.csv(paste0(dir, "/_ca/ca_rec_catch_all_2005-2019_crfs.csv"))
ca_rec = ca_rec[ca_rec$Species.Name == "COPPER ROCKFISH", ]

# California - MRFSS years were 1980 - 1989 split wrong (need to move catches from south to north)
# 1980 should be replaced with historical reconstruction
# Missing 1990-1992 catches - need to linear interpolate
# 1993 - 2004 CRFS years which are split north and south correctly at pt. concep
ca_rec_early = read.csv(paste0(dir, "/_ca/mrfss_catch_estimates_1980_2004_final.csv"))
ca_rec_early = ca_rec_early[ca_rec_early$AGENCY_CODE == 6 & ca_rec_early$SPECIES_NAME == "Copper Rockfish", ]
ca_rec_early = ca_rec_early[ca_rec_early$YEAR_ != 1980, ]
# The column A+B1 should represent retained and discard mortality 

#-----------------------------------------------------------------------------------
# Evaluate the recreational data first
#-----------------------------------------------------------------------------------

# California - RecFIN years - These data area split north and south at pt. conception already
ca_rec$areas = recfin_areas(data = ca_rec, 
			  area_grouping = list(c("CHANNEL", "SOUTH"), c("BAY AREA", "WINE", "CENTRAL", "REDWOOD")), 
			  area_names  = c("south", "north"), 
			  column_name = "RecFIN.Port.Name")

ca_rec$mode <- "private"
ca_rec$mode[ca_rec$RecFIN.Mode.Name == "PARTY/CHARTER BOATS"] <- "cpfv"

# California - MRFSS years 
# The 1981-1989 "south" includes san louis obisbo and those catches need to be moved to the north area.
ca_rec_early$areas = recfin_areas(data = ca_rec_early, 
			  area_grouping = list("Northern California", "Southern California"), 
			  area_names  = c("north", "south"), 
			  column_name = "RECFIN_SUB_REGION_NAME")
ca_rec_early$year <- ca_rec_early$YEAR_
ca_rec_early$mode <- "private"
ca_rec_early$mode[ca_rec_early$RECFIN_MODE_NAME == "Party/Charter Boats"] <- "cpfv"


# Cut down the data frame to a manageble size of key items
tmp = aggregate(WGT_AB1..mt. ~ YEAR_ + areas + mode, data = ca_rec_early, drop = FALSE, FUN = sum)
colnames(tmp) = c("year", "areas", "mode", "orig_catch_mt")
# Move catches in the south to the north for 1980 - 1989: ratio of 0.317 based on Albin et al. 1993 
# See Allocate_MRFSS_1980-1989.xls to see how calculated:
n = which(tmp$year %in% c(1980:1989) & tmp$areas == "north")
s = which(tmp$year %in% c(1980:1989) & tmp$areas == "south")
tmp$catch_mt = NA
tmp$catch_mt[n] = tmp[n, "orig_catch_mt"] + tmp[s, "orig_catch_mt"] * 0.317
tmp$catch_mt[s] = tmp[s, "orig_catch_mt"] - tmp[s, "orig_catch_mt"] * 0.317
tmp$catch_mt[is.na(tmp$catch_mt)] = tmp$orig_catch_mt[is.na(tmp$catch_mt)]
# Need to linear interpolate the missing years 1990-1992
rn = sum(tmp[which(tmp$year %in% c(1993) & tmp$area == "north"), "catch_mt"] - 
	tmp[which(tmp$year %in% c(1989) & tmp$area == "north"), "catch_mt"], na.rm = TRUE) / 4
rs = sum(tmp[which(tmp$year %in% c(1993) & tmp$area == "south"), "catch_mt"] - 
	tmp[which(tmp$year %in% c(1989) & tmp$area == "south"), "catch_mt"]) / 4

add = NULL
for (a in c("south", "north")){
	x = ifelse(a == "south", rs, rn)
	z = sum(tmp[which(tmp$year == 1989 & tmp$area == a), "catch_mt"])
	step = 1
	for (y in 1990:1992){
		new = c(y, a, "cpfv", 0, z + step*x)
		add = rbind(add, new)
		step = step + 1
	}
	
}
add = as.data.frame(add)
colnames(add) = c('year', 'areas', 'mode', 'orig_catch_mt', 'catch_mt'); rownames(add) = NULL
tmp_early = rbind(tmp, as.data.frame(add))
tmp_early = tmp_early[order(tmp_early$year),]

tmp_late = aggregate(Total.Mortality.MT~RecFIN.Year + areas + mode, data = ca_rec, drop = FALSE, FUN = sum)
colnames(tmp_late) = c('year', 'areas', 'mode', 'catch_mt')

# Create a single data frame with all the california rec catches
ca_rec_all = merge(tmp_late, tmp_early, by = c("year",  "areas", "mode", "catch_mt"), all = TRUE)

write.csv(ca_rec_all, file = file.path(save_dir, "rec_catches.csv"))