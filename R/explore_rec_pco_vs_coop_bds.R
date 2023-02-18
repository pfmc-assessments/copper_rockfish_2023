##################################################################################################
#
#	 Recreational Cooperative Sample Comparison by Trip Duration with
#                CPFV Data from Similar Trip Duration
# 					
#			Written by Melissa Monk & Chantel Wetzel
#
##################################################################################################

library(here)
library(ggplot2)
library(tidyr)
#Chantel dir
#dir <- "C:/Assessments/2023/copper_rockfish_2023/data/rec_bds"
#Melissa dir
#dir <- "S:/copper_rockfish_2023/data/rec_bds"
dir <- file.path(here(), "data", "rec_bds")
setwd(dir)
dir.create(file.path(dir, "plots"), showWarnings = FALSE)

load("rec_pco2015_2019_bds.rdata")
load("rec_coop_bds.rdata")
coop$TripType <- as.factor(coop$TripType)

#Limit the pco bds to years 2018- and remove half day trips
pco_bds <- pco_bds %>%
  dplyr::filter(year > 2017)  %>%
  dplyr::filter(DurationType != "Twilight")

ggplot(pco_bds, aes(lengthcm, fill = dataset, color = dataset)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  geom_density(data = coop, alpha = 0.4, lwd = 0.8, adjust = 0.5)  +
    xlab("Length (cm)") + ylab("Density") +
    facet_grid(area+DurationType~.) 
ggsave(filename = file.path(dir, "plots", "pco_bds_coop_length_dist_by_duration_area.png"),
	width = 10, height = 7)

ggplot(subset(pco_bds, area=="south"), 
       aes(lengthcm, fill = DurationType, color = DurationType)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") 
ggsave(filename = file.path(dir, "plots", "pco_bds__south_length_dist_by_duration.png"),
       width = 10, height = 7)

#Coop lengths by vessel
ggplot(subset(coop, area=="south"), aes(lengthcm, fill = Vessel, color = Vessel)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_grid(TripType~.)

ggplot(coop, aes(lengthcm, fill = TripType, color = TripType)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_grid(area~.)


means_pco_bds <- aggregate(lengthcm ~ area + TripType, pco_bds, mean )
means_coop <- aggregate(lengthcm ~ area + TripType, coop, mean)
means_pco_bds
means_coop

pco_bds_sample_size <- pco_bds %>%
  dplyr::group_by(TripType, area) %>%
  dplyr::tally()
pco_bds_sample_size

# TripType  area      n
# <chr>     <chr> <int>
# MultiDay  south   258
# SingleDay north  1282
# SingleDay south  2177

coop_sample_size <- coop %>%
  dplyr::group_by(Vessel) %>%
  dplyr::tally()
coop_sample_size

# Vessel         n
# <chr>      <int>
# Amigo         53
# Coral Sea    156
# Legacy       105
# Mirage       150
# Salty Lady     7
# Sea Wolf      99
# Stardust     125

# ==========================================================
# Create a single data frame to look at changes
# across time
# ==========================================================
load("mrfss_bds_filtered.rdata")
mrfs <- mrfss_bds

data <- data.frame(
	year = c(mrfs$year, pco_bds$year),
	lengthcm = c(mrfs$lengthcm, pco_bds$lengthcm),
	area = c(mrfs$area, pco_bds$area),
	mode = c(mrfs$mode, pco_bds$mode),
	program = c(rep('mrfs', dim(mrfs)[1]), rep('pco_bds', dim(pco_bds)[1]))
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
#    pco_bds north    cpfv 37.36482
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