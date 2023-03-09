##################################################################################################
#
#	Recreation BDS Data Exploration for Copper Rockfish 2023
# 					
#			Written by Chantel Wetzel
#
##################################################################################################

library(ggplot2)

dir <- "C:/Assessments/2023/copper_rockfish_2023/data/rec_bds"
setwd(dir)
dir.create(file.path(dir, "plots"))

load("crfss_bds_filtered.rdata")
load("mrfss_bds_filtered.rdata")

crfs <- crfss_bds
mrfs <- mrfss_bds

ggplot(crfs, aes(y = lengthcm, x = year, group = year)) +
	geom_boxplot() + 
	facet_wrap(facets = c("area", "IS_RETAINED")) + 
	xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "crfs_length_boxplot_by_retention_area_year.png"),
	width = 10, height = 7)

# Remove the released fish since they are distinctly different
# The released fish are all from CPFV model with 52 in the north and 187 in south
crfs <- crfs[crfs$IS_RETAINED == "RETAINED", ]
#           north south
#  RELEASED    52   187
#  RETAINED 33131 27344

ggplot(crfs, aes(y = lengthcm, x = year, group = year)) +
	geom_boxplot() + 
	facet_wrap(facets = c("area", "mode")) + 
	xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "crfs_length_boxplot_by_mode_area_year.png"),
	width = 10, height = 7)

# There are only a handful of shoreside lengths that are
# just noise, let's remove them (north = 42, south = 18 N)
crfs <- crfs[crfs$mode != "shoreside", ]
#            north south
#  cpfv       9162 21119
#  private   23927  6207
#  shoreside    42    18

ggplot(crfs, aes(lengthcm, fill = mode, color = mode)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    facet_grid(area~.) 
ggsave(filename = file.path(dir, "plots", "crfs_length_dist_by_mode_area.png"),
	width = 10, height = 7)

ggplot(crfs, aes(lengthcm, fill = mode, color = mode)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    facet_wrap(facets = c("area", "year"))

# =======================================================
# Let's look at MRFS now ================================
# =======================================================
# LNGTH is a standardized fork length across all species according
# to E.J.
mrfs$count <- 1 

use_tlen = which(mrfs$LNGTH %% 1 != 0)
table(mrfs[use_tlen, 'year'])
# 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1997 1998 
#  828  529  669  514  562  713  673  207  283  255  490   60 
use_lngth = which(mrfs$T_LEN %% 1 != 0)
table(mrfs[use_lngth, 'year'])
# 1980 1988 1994 1996 1997 1998 1999 2000 2001 2002 2003 
#  124    1  398  387   75  265  677  314  188  248  497

# table(mrfs$mode, mrfs$area)           
#             north south
#   cpfv       2809  2416
#   private    3098  1294
#   shoreside   106     2
# Let's remove the shoreside length because they are noise

mrfs <- mrfs[mrfs$mode != "shoreside", ]

find <- which(mrfs$lengthcm > 70)
mrfs <- mrfs[-find, ]

ggplot(mrfs, aes(y = lengthcm, x = year, group = year)) +
	geom_boxplot() + 
	facet_wrap(facets = c("area", "mode")) + 
	xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "mrfs_length_boxplot_by_mode_area_year.png"),
	width = 10, height = 7)

ggplot(mrfs, aes(lengthcm, fill = mode, color = mode)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    facet_grid(area~.) 
ggsave(filename = file.path(dir, "plots", "mrfs_length_dist_by_mode_area.png"),
	width = 10, height = 7)

ggplot(mrfs, aes(y = count, x = year))  + 
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Year") + ylab("Number of Length Samples") +
    facet_wrap(facets = c("area", "mode")) + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "mrfs_length_samples_by_area_year.png"),
	width = 10, height = 7)

# ==========================================================
# Create a single data frame to look at changes
# across time
# ==========================================================

data <- data.frame(
	year = c(mrfs$year, crfs$year),
	lengthcm = c(mrfs$lengthcm, crfs$lengthcm),
	area = c(mrfs$area, crfs$area),
	mode = c(mrfs$mode, crfs$mode),
	program = c(rep('mrfs', dim(mrfs)[1]), rep('crfs', dim(crfs)[1]))
)

ggplot(data, aes(y = lengthcm, x = year, group = year)) +
	geom_boxplot() + 
	facet_wrap(facets = c("area", "mode")) + 
	xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "all_length_boxplot_by_mode_area_year.png"),
	width = 10, height = 7)


means_by_year <- aggregate(lengthcm ~ year + area + mode, data, mean)

png(file.path(dir, "plots", "mean_length_by_mode_area.png"),
	height = 10, width = 13, units = 'in', res = 300)
ind = which(means_by_year$area == "south" & means_by_year$mode == "private")
plot(means_by_year[ind, 'year'], means_by_year[ind, 'lengthcm'], 
	type = 'p', pch = 1, col = 'red', ylim = c(20, 45), ylab = "Length (cm)",
	xlab = "Year")
ind = which(means_by_year$area == "south" & means_by_year$mode == "cpfv")
points(means_by_year[ind, 'year'], means_by_year[ind, 'lengthcm'], pch = 2, col = 'blue')
ind = which(means_by_year$area == "north" & means_by_year$mode == "private")
points(means_by_year[ind, 'year'], means_by_year[ind, 'lengthcm'], pch = 16, col = 'red')
ind = which(means_by_year$area == "north" & means_by_year$mode == "cpfv")
points(means_by_year[ind, 'year'], means_by_year[ind, 'lengthcm'], pch = 17, col = 'blue')
abline(h = 34.3, col = 'grey10', lty = 3, lwd = 2)
abline(h = 34.8, col = 'grey10', lty = 3, lwd = 2)
legend('bottomright', bty = 'n', col = c('red', 'blue'), pch = c(1, 2, 16, 17),
	legend = c('South Pt. Conception - Private', 'South Pt. Conception - CPFV',
	'North Pt. Conception - Private', 'North Pt. Conception - CPFV'))
legend('bottomleft', bty = 'n', lwd = 2, lty = 3, col = 'grey10', legend = "Length at 50% Maturity")
dev.off()

means <- aggregate(lengthcm ~ program + mode, data[data$area == "south",] , mean)

ggplot(data[data$area == "south", ], aes(lengthcm, fill = program, color = program)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
	geom_vline(data = means, aes(xintercept = lengthcm, colour = program), lwd = 2) +
    xlab("Length (cm)") + ylab("Density") +
    facet_wrap(c("mode"))
ggsave(filename = file.path(dir, "plots", "all_south_length_dist_by_mode.png"),
	width = 10, height = 5)

means <- aggregate(lengthcm ~ program + mode, data[data$area == "north",] , mean)

ggplot(data[data$area == "north", ], aes(lengthcm, fill = program, color = program)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
	geom_vline(data = means, aes(xintercept = lengthcm, colour = program), lwd = 2) +
    xlab("Length (cm)") + ylab("Density") +
    facet_wrap(c("mode"))
ggsave(filename = file.path(dir, "plots", "all_north_length_dist_by_mode.png"),
	width = 10, height = 5)


means <- aggregate(lengthcm ~ area + mode, data, mean)
# program  area    mode lengthcm
#    crfs north    cpfv 37.36482
#    mrfs north    cpfv 37.40638
#    crfs south    cpfv 32.15094
#    mrfs south    cpfv 33.86601
#    crfs north private 36.38861
#    mrfs north private 35.81970
#    crfs south private 32.78672
#    mrfs south private 33.18530

ggplot(data, aes(lengthcm, fill = program, color = program)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
	geom_vline(data = means, aes(xintercept = lengthcm, colour = area), lwd = 2) +
    xlab("Length (cm)") + ylab("Density") +
    scale_fill_viridis_d() +
    facet_wrap(c("area", "mode"))
ggsave(filename = file.path(dir, "plots", "all_length_dist_by_mode_area_program.png"),
	width = 10, height = 5)

ggplot(data, aes(lengthcm, fill = mode, color = mode)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
	#geom_vline(data = means, aes(xintercept = lengthcm, colour = area), lwd = 2) +
    xlab("Length (cm)") + ylab("Density") +
    scale_fill_viridis_d() +
    facet_grid(area~.) 
ggsave(filename = file.path(dir, "plots", "all_length_dist_by_mode_area.png"),
	width = 10, height = 5)


ggplot(data, aes(lengthcm, fill = area, color = area)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
	geom_vline(data = means, aes(xintercept = lengthcm, colour = area), lwd = 2) +
    xlab("Length (cm)") + ylab("Density") +
    facet_wrap(c("mode")) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "all_length_dist_by_mode_area_w_mean.png"),
	width = 10, height = 5)

data$count = 1
ggplot(data, aes(y = count, x = year, fill = mode))  + 
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Year") + ylab("Number of Length Samples") +
    facet_grid(area~.)  + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "rec_length_samples_by_area_year.png"),
	width = 10, height = 7)