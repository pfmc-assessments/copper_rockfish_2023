##################################################################################################
#
#	PacFIN Data Exploration for Copper Rockfish 2023
# 					
#		Written by Chantel Wetzel
#
##################################################################################################

library(PacFIN.Utilities)
library(ggplot2)

dir <- "C:/Assessments/2023/copper_rockfish_2023/data/pacfin_bds"
setwd(dir)
dir.create(file.path(dir, "plots"))

load("PacFIN.COPP.bds.07.Nov.2022.RData")

# Clean the data using PacFIN Utilities to see 
# what records get removed
data <- cleanPacFIN(
	Pdata = bds.pacfin[bds.pacfin$AGENCY_CODE == "C",], 
	CLEAN = TRUE,
	verbose = TRUE)

# N SAMPLE_TYPEs changed from M to S for special samples from OR: 0
# N not in keep_sample_type (SAMPLE_TYPE): 0
# N with SAMPLE_TYPE of NA: 0
# N not in keep_sample_method (SAMPLE_METHOD): 0
# N with SAMPLE_NO of NA: 0
# N without length: 276
# N without Age: 7580
# N without length and Age: 7580
# N sample weights not available for OR: 0
# N records: 7580
# N remaining if CLEAN: 7580
# N removed if CLEAN: 0

#  276 fish without lengths or ages
no_lengths <- which(is.na(data$lengthcm))

data <- data[-no_lengths, ]

# Parse out areas north and south of point conception
south_areas <- c("DNA","HNM","LGB","NWB","OBV","OLA","OSD","OXN","SB","SD","SP","TRM","VEN","WLM")
north_areas <- c("ALB","ALM","ARE","AVL", "BCR","BDG","BKL","BOL","BRG","CRS","CRZ","ERK",
			 "FLN","MNT","MOS","MRO","OAK","OCA","OCM","OCN","ODN","OHB","OMD","OSF","OSL","OSM","OWC","PRN","RCH","RYS","SF","SLT","TML","TRN")

sum(!data$PCID %in% c(south_areas, north_areas))
# 0 all records included in the above areas

data$area <- "south"
ind <- which(data$PCID %in% north_areas)
data$area[ind] <- "north"

data$cond <- "dead"
data$cond[data$COND == "A"] <- "alive"
data$count <- 1
#        alive dead
#  north  1099 3517
#  south   553 2135

length_by_cond <- aggregate(length~cond + area, data, quantile)
sd_by_cond <- aggregate(length~cond + area, data, sd)
#  cond  area length.0% length.25% length.50% length.75% length.100%
# alive north     200.0      303.0      351.0      425.5       573.0
#  dead north     185.0      341.0      397.0      454.0       616.0
# alive south     247.0      327.0      352.0      380.0       490.0
#  dead south     174.0      335.0      375.0      410.0       544.0
median_by_cond_yr <- aggregate(length~year + area + cond, data, median)
sd_by_cond_yr <- aggregate(length~year + area + cond, data, sd)
n_by_cond_yr <- aggregate(length~year + area + cond, data, length)

# The vast majority of length data comes from HKL gear
#      north south
#  HKL  4268  2585
#  NET    32    24
#  POT   139     4
#  TLS    15     0
#  TWL   162    39
#  TWS     0    36
ggplot(data, aes(x = year, y = lengthcm, group = year)) +
	geom_boxplot() + 
	xlab("Year") + ylab("Length (cm)") + 
    facet_wrap(facets = c("area", "geargroup"), ncol = 5) 
ggsave(filename = file.path(dir, "plots", "length_boxplot_by_gear_area.png"),
	width = 10, height = 7)


write.csv(cbind(median_by_cond_yr, sd_by_cond_yr[, 'length'], n_by_cond_yr[, 'length']),
	file = file.path(dir, "median_sd_by_cond_area_year.csv"))

ggplot(data, aes(y = lengthcm, x = year, group = year)) +
	geom_boxplot() + 
	facet_wrap(facets = c("area", "cond")) + 
	xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "length_boxplot_by_cond_area_year.png"),
	width = 10, height = 7)

ggplot(data, aes(fill = cond, y = count, x = year)) + 
    geom_bar(position="stack", stat="identity") + 
    xlab("Year") + ylab("Number of Length Samples") +
    facet_wrap(facets = "area")
ggsave(filename = file.path(dir, "plots", "length_samples_by_cond_area_year.png"),
	width = 10, height = 7)

ggplot(data, aes(fill = cond, y = lengthcm)) +
	geom_boxplot() + 
	xlab("Landed Condition") + ylab("Length (cm)") + 
    facet_wrap(facets = "area") 
ggsave(filename = file.path(dir, "plots", "length_boxplot_by_cond_area.png"),
	width = 10, height = 7)

ggplot(data, aes(lengthcm, fill = cond, color = cond)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    facet_grid(area~.) 
ggsave(filename = file.path(dir, "plots", "length_by_cond_area.png"),
	width = 10, height = 7)

ggplot(data[data$year > 2011, ], aes(lengthcm, fill = cond, color = cond)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    facet_wrap(facets = c("area", "year"))
ggsave(filename = file.path(dir, "plots", "length_by_cond_area_year.png"),
	width = 10, height = 10)

ggplot(data, aes(fill = cond, y = count, x = PCID)) + 
    geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Year") + ylab("Number of Length Samples") +
    facet_wrap(facets = "area") + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "length_by_cond_area_port.png"),
	width = 14, height = 7)

ggplot(data, aes(fill = geargroup, y = count, x = PCID)) + 
    geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Year") + ylab("Number of Length Samples") +
    facet_wrap(facets = "area") + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "length_n_by_gear_area_port.png"),
	width = 14, height = 7)


ggplot(data, aes(y = lengthcm, x = year, group = year)) + 
	geom_boxplot() + 
    xlab("Year") + ylab("Length (cm)") +
    facet_wrap(facets = c("area", "cond")) + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "length_dist_by_year_area_cond.png"),
	width = 14, height = 7)

ggplot(data[data$area == 'north',], aes(y = lengthcm, x = PCID, group = PCID)) + 
	geom_boxplot() + 
    xlab("Port") + ylab("Length (cm)") +
    facet_wrap(facets = c("geargroup")) + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "north_length_dist_by_port_gear.png"),
	width = 14, height = 7)

ggplot(data[data$area == 'south',], aes(y = lengthcm, x = PCID, group = PCID)) + 
	geom_boxplot() + 
    xlab("Port") + ylab("Length (cm)") +
    facet_wrap(facets = c("geargroup")) + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "south_length_dist_by_port_gear.png"),
	width = 14, height = 7)


prop_sample <- aggregate(lengthcm~area+PCID+cond, data, length)
prop_sample$prop <- prop_sample[,"lengthcm"] / sum(prop_sample[,"lengthcm"])
# sum(prop_sample[prop_sample$area == "north", "prop"])
# south 0.368 and north 0.632

ggplot(prop_sample, aes(fill = cond, y = prop, x = PCID)) + 
    geom_bar(position="stack", stat="identity") + 
    xlab("Year") + ylab("Proportion of Statewide Samples") +
    facet_wrap(facets = "area") + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "prop_samples_by_cond_area_port.png"),
	width = 14, height = 7)

ggplot(prop_sample, aes(fill = cond, y = prop, x = PCID)) + 
    geom_bar(position="stack", stat="identity") + 
    xlab("Year") + ylab("Proportion of Statewide Samples") +
    facet_wrap(facets = "area") + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "prop_samples_by_cond_area_port.png"),
	width = 14, height = 7)

sample_by_year_cond <- aggregate(lengthcm ~ year + area + cond, data, length, drop = FALSE)
sample_by_year_cond[is.na(sample_by_year_cond)] = 0
total_by_year <- aggregate(lengthcm ~ year + area, data, length, drop = FALSE)
total_by_year[is.na(total_by_year)] = 0

total_dead_area <-  aggregate(lengthcm ~ year + area, sample_by_year_cond[sample_by_year_cond$cond == "dead",], sum, drop = FALSE)
prop <- data.frame(
	year = total_dead_area$year, 
	area = total_dead_area$area, 
	prop_dead = total_dead_area[, 'lengthcm'] / total_by_year[, 'lengthcm'])

ggplot(prop[prop$year > 2000,], aes(y = prop_dead, x = year)) + 
    geom_bar(position="stack", stat="identity") + 
    xlab("Year") + ylab("Proportion of Samples from Dead Fish") +
    facet_wrap(facets = "area") 
ggsave(filename = file.path(dir, "plots", "prop_samples_dead_fish_by_area.png"),
	width = 14, height = 7)

# Look at samples collected by gear group
ggplot(data, aes(x = geargroup, y = count, fill = cond, color = cond)) + 
	geom_bar(position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Number of Samples by Gear Type") +
    facet_wrap(facets = c("area")) 
ggsave(filename = file.path(dir, "plots", "samples_by_gear_cond_area.png"),
	width = 7, height = 7)


ggplot(data[data$area == 'north',], aes(x = year, y = count, fill = geargroup, color = geargroup)) + 
	geom_bar(position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Number of Samples by Year and Port") +
    facet_wrap(facets = c("PCID")) 
ggsave(filename = file.path(dir, "plots", "north_samples_by_gear_port_year.png"),
	width = 7, height = 7)

ggplot(data[data$area == 'south',], aes(x = year, y = count, fill = geargroup, color = geargroup)) + 
	geom_bar(position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Number of Samples by Year and Port") +
    facet_wrap(facets = c("PCID")) 
ggsave(filename = file.path(dir, "plots", "south_samples_by_gear_port_year.png"),
	width = 7, height = 7)

ggplot(data[data$year > 2001 & data$area == "north", ], 
		aes(x = geargroup, y = count, fill = cond)) + 
	geom_bar(position="stack", stat="identity") + 
	scale_fill_viridis_d() + 
    xlab("Length (cm)") + ylab("Number of Samples by Gear Type") +
    facet_wrap(facets = c("year")) + 
    ggtitle("North")
ggsave(filename = file.path(dir, "plots", "north_samples_by_gear_year_cond_area.png"),
	width = 7, height = 7)

ggplot(data[data$year > 2001 & data$area == "south", ], 
		aes(x = geargroup, y = count, fill = cond)) + 
	geom_bar(position="stack", stat="identity") + 
	scale_fill_viridis_d() + 
    xlab("Length (cm)") + ylab("Number of Samples by Gear Type") +
    facet_wrap(facets = c("year")) + 
    ggtitle("South")
ggsave(filename = file.path(dir, "plots", "south_samples_by_gear_year_cond_area.png"),
	width = 7, height = 7)

table(data$area, data$geargroup)
# TLS = troll gear and TWS = shrimp trawls
#         HKL  NET  POT  TLS  TWL  TWS
#  north 4268   32  139   15  162    0
#  south 2585   24    4    0   39   36

aggregate(lengthcm ~ geargroup + area, data, quantile)
# geargroup  area lengthcm.0% lengthcm.25% lengthcm.50% lengthcm.75% lengthcm.100%
#       HKL north       18.00        32.00        39.00        44.00         61.00
#       NET north       29.00        33.75        37.00        40.00         46.00
#       POT north       23.00        29.00        32.00        34.00         44.00
#       TLS north       29.00        39.00        43.00        49.50         53.00
#       TWL north       29.00        39.00        43.50        47.00         55.00
#       HKL south       19.00        33.00        36.00        40.00         54.00
#       NET south       30.00        43.00        46.50        48.50         53.00
#       POT south       33.00        36.00        37.50        39.50         44.00
#       TWL south       17.00        27.00        34.00        40.00         49.00
#       TWS south       22.00        28.75        33.00        43.00         49.00

ggplot(data, aes(lengthcm, fill = geargroup, color = geargroup)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    facet_grid(area~.)
ggsave(filename = file.path(dir, "plots", "length_by_gear_area.png"),
	width = 7, height = 7)

ggplot(data[data$geargroup == "HKL",] , aes(lengthcm, fill = area)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") 
ggsave(filename = file.path(dir, "plots", "length_by_hkl_area.png"),
	width = 7, height = 7)

ggplot(data , aes(y = lengthcm, fill = geargroup)) + 
	geom_boxplot() + 
    xlab("Gear Type") + ylab("Length (cm)")  + 
    facet_wrap('area')
ggsave(filename = file.path(dir, "plots", "boxplot_length_by_hkl_area.png"),
	width = 7, height = 7)

ggplot(data , aes(x = area, fill = geargroup)) + 
	geom_bar(aes(y = count), position="stack", stat="identity") + 
    xlab("Area") + ylab("Total Number of Samples")  + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "total_samples_by_gear_area.png"),
	width = 7, height = 7)

ggplot(data, aes(x = year, fill = geargroup)) + 
	geom_bar(aes(y = count), position="stack", stat="identity") + 
    xlab("Area") + ylab("Total Number of Samples")  + 
    scale_fill_viridis_d() + 
    facet_wrap("area")
ggsave(filename = file.path(dir, "plots", "samples_by_year_gear_area.png"),
	width = 7, height = 7)

# Samples by sex and area
# table(data$area, data$SEX)     
#          F    M    U
# north  136  132 4348
# south    2    7 2679

# Samples by month and area
# table(data$area, data$SAMPLE_MONTH)      
#         1   2   3   4   5   6   7   8   9  10  11  12
# north 191 230 130 159 313 578 644 567 580 667 459  98
# south 294 180  93 140 198 212 397 238 244 437 139 116


