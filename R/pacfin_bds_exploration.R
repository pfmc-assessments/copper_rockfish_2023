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
data <- cleanPacFIN(Pdata = bds.pacfin[bds.pacfin$AGENCY_CODE == "C",], 
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
#  cond  area length.0% length.25% length.50% length.75% length.100%
# alive north     200.0      303.0      351.0      425.5       573.0
#  dead north     185.0      341.0      397.0      454.0       616.0
# alive south     247.0      327.0      352.0      380.0       490.0
#  dead south     174.0      335.0      375.0      410.0       544.0

ggplot(data, aes(fill = cond, y = count, x = year)) + 
    geom_bar(position="stack", stat="identity") + 
    xlab("Year") + ylab("Number of Length Samples") +
    facet_wrap(facets = "area")
ggsave(filename = file.path(dir, "plots", "length_samples_by_cond_area_year.png"),
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
    facet_wrap(facets = "area")
ggsave(filename = file.path(dir, "plots", "length_by_cond_area_port.png"),
	width = 10, height = 7)

prop_sample <- aggregate(lengthcm~area+PCID+cond, data, length)
prop_sample$prop <- prop_sample[,"lengthcm"] / sum(prop_sample[,"lengthcm"])
# sum(prop_sample[prop_sample$area == "north", "prop"])
# south 0.368 and north 0.632

ggplot(prop_sample, aes(fill = cond, y = prop, x = PCID)) + 
    geom_bar(position="stack", stat="identity") + 
    xlab("Year") + ylab("Proportion of Statewide Samples") +
    facet_wrap(facets = "area")
ggsave(filename = file.path(dir, "plots", "prop_samples_by_cond_area_port.png"),
	width = 14, height = 7)

# Look at samples collected by gear group
ggplot(data, aes(x = geargroup, y = count, fill = cond, color = cond)) + 
	geom_bar(position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Number of Samples by Gear Type") +
    facet_wrap(facets = c("area"))
ggsave(filename = file.path(dir, "plots", "samples_by_gear_cond_area.png"),
	width = 7, height = 7)

table(data$area, data$geargroup)
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


