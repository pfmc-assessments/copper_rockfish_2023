###################################################################################
#
#       Copper rockfish 2023
#        NWFSC HKL survey 
#    	  data exploration 
#
#############################################################################################

library(ggplot2)
library(dplyr)
library(HandyCode)

dir <- "C:/Assessments/2023/copper_rockfish_2023/data/nwfsc_hkl"

hkl_all <- read.csv(file.path(dir, "hookandline_2004_2021_draft_data.csv"))
hkl_all$lat <- hkl_all$drop_latitude_degrees
hkl_all$lon <- hkl_all$drop_longitude_degrees

hkl_all$area <- ifelse(hkl_all$site_number >= 500, "CCA", "Outside CCA")
hkl_all$count <- 0
ind <- which(hkl_all$common_name == "Copper Rockfish")
hkl_all[ind, 'count'] <- 1
hkl_all$count_bocaccio <- hkl_all$count_vermilion <- 0
hkl_all[hkl_all$common_name == "Bocaccio", 'count_bocaccio'] <- 1
hkl_all[hkl_all$common_name == "Vermilion Rockfish", 'count_bocaccio'] <- 1
hkl_all$fathom_bin <- plyr::round_any(hkl_all$drop_depth_fathoms, 5, floor)

hkl_all_site <- hkl_all %>%
	group_by(site_number) %>%
	summarise(
		site_lat = mean(drop_latitude_degrees),
		site_lon = -1*mean(drop_longitude_degrees),
		site_depth = mean(drop_depth_fathoms),
		site_area = unique(area),
		total_count = sum(count),
		total_bocaccio = sum(count_bocaccio),
		total_vermilion = sum(count_vermilion)
	)

hkl_species <- hkl_all[hkl_all$common_name %in% c(
	"Copper Rockfish", "Bocaccio", "Vermilion Rockfish"), ]

# Filter down to only copper obervations
hkl <- hkl_all[ind, ]
hkl$length_bin <- plyr::round_any(hkl$length_cm, 2, floor)


sub_hkl_all_site <- hkl_all_site[hkl_all_site$total_count > 0, ]
colors <- viridis::viridis(10)[c(1,5,9)]
ggplot() +
	geom_jitter() + 
	geom_point(data = hkl_all_site, aes(x = site_lon, y = site_lat, col = site_area), size = 5, pch = 1) + 
	geom_point(data = sub_hkl_all_site, aes(x = site_lon, y = site_lat, size = total_count), 
		color = colors[2]) + 
	scale_size_continuous("Count", breaks = c(1, 10, seq(25, 125, 25))) +
	scale_fill_manual("Site", values = c('CCA'  = colors[1], 'Outside CCA' = colors[3])) +
    scale_color_manual("Site", values = c('CCA' = colors[1], 'Outside CCA' = colors[3])) +
	theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 14),
      	legend.text = element_text(size = 14)) +
	xlab("Longitude") + ylab("Latitude") 
ggsave(filename = file.path(dir, "plots", "hkl_copper_by_site_count.png"),
	width = 10, height = 8)

pngfun(wd = file.path(dir, "plots"), file = "hkl_site_observations.png", w = 7, h = 7, pt = 12)
colors <- viridis::viridis(3, alpha = c(0.05, 0.05, 1))
jitter = runif(nrow(hkl_all), 0.01, 0.1)
plot(-1*hkl_all$lon + rev(jitter), hkl_all$lat + jitter, type = 'p', col = colors[1], 
	xlab = "Longitude", ylab = "Latitude")
find = which(hkl_all$area == "CCA")
points(-1*hkl_all$lon[find] + rev(jitter[find]), hkl_all$lat[find] + jitter[find], col = colors[2])
find = which(hkl_all$common_name == "Copper Rockfish")
points(-1*hkl_all$lon[find] + rev(jitter[find]), hkl_all$lat[find] + jitter[find], pch = 16, col = colors[3])
colors <- viridis::viridis(3, alpha = 1)
legend('topright', bty = 'n', pch = c(16, 16, 16), col = colors, pt.cex = 2, 
	legend = c("Outside CCA Sites", "Inside CCA Sites", "Sites with Copper Rockfish Observed"))
dev.off()


aggregate(fathom_bin~area, hkl_all, quantile)
#        area fathom_bin.0% fathom_bin.25% fathom_bin.50% fathom_bin.75% fathom_bin.100%
#         CCA            24             48             60             84             128
# Outside CCA            18             44             54             72             140


ggplot(hkl, aes(x = drop_depth_fathoms, y = length_cm)) +
	geom_jitter() + 
	geom_point(aes(col = sex), size = 2) +
	scale_colour_viridis_d() + 
	xlim(20, 70) + ylim(10, 55) + 
	 theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 14),
      	legend.text = element_text(size = 14)) +
	xlab("Depth (fathoms)") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "hkl_length_sex_depth.png"),
	width = 10, height = 8)

ggplot(hkl, aes(y = count, x = year, fill = sex))  + 
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Year") + ylab("Total Observations") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "hkl_observations_by_year_sex.png"),
	width = 10, height = 8)


ggplot(hkl[!is.na(hkl$otolith_number), ], aes(y = count, x = length_bin, fill = sex))  + 
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Number of Length Samples with Otoliths") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "hkl_length_samples_w_otoliths_by_area.png"),
	width = 10, height = 8)

table(hkl$area)
# CCA Outside CCA 
#  94        1057
ggplot(hkl, aes(length_cm, fill = area, color = area)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "hkl_density_by_area.png"),
	width = 10, height = 8)

ggplot(hkl, aes(y = count, x = length_bin, fill = area))  +  
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Total Observations") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(filename = file.path(dir, "plots", "hkl_observations_by_length_area.png"),
	width = 10, height = 8)

ggplot(hkl, aes(x = length_bin, fill = area))  + 
	geom_bar(position = 'fill') + 
    xlab("Length (cm)") + ylab("Total Observations") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(filename = file.path(dir, "plots", "hkl_catch_by_hook_position.png"),
	width = 10, height = 8)

ggplot(hkl, aes(x = length_bin, fill = area))  + 
	geom_bar(position = 'fill') + 
    xlab("Length (cm)") + ylab("Total Observations") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d(begin = 0, end = 0.5)


ggplot(hkl, aes(x = hook_number, fill = drop_number))  + 
	geom_bar(position="stack") + 
    xlab("Hook Position") + ylab("Numbers Caught") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(filename = file.path(dir, "plots", "hkl_catch_by_hook_position.png"),
	width = 10, height = 8)

ggplot(hkl, aes(x = drop_number, fill = area))  + 
	geom_bar(position="stack") + 
    xlab("Drop Number") + ylab("Numbers Caught") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(filename = file.path(dir, "plots", "hkl_catch_by_drop_number.png"),
	width = 10, height = 8)


hkl_all$lat_round <- round(hkl_all$lat,1)
hkl_all$lon_round <- round(hkl_all$lon,1)
lon_range <- c(min(hkl_all$lon), max(hkl_all$lon))
lat_range <- c(min(hkl_all$lat), max(hkl_all$lat))
ggplot2::ggplot(hkl_all, aes(lon_round, lat_round, fill = count)) +
		geom_raster() +
		coord_fixed() +
		nwfscSurvey::draw_theme() +
      	nwfscSurvey::draw_land() +
      	nwfscSurvey::draw_USEEZ(lon_range, lat_range) 