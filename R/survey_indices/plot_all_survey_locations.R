
#===============================================================================
# Visualize the sampling locations by the ROV, CCFRP, and NWFSC HKL survey
#==============================================================================

library(dplyr)
library(here)
library(ggplot2)
library(nwfscSurvey)

#setwd to the north or the south
dir <- file.path(here(),"data") 


#===============================================================================
# Load in the CCFRP data
#===============================================================================

load(file.path(dir,"survey_indices","ccfrp", "north", "Filtered_data_CCFRP.RData"))
dat$lat <- dat[, "startLat"]
dat$lon <- dat[, "startLong"]
dat$designation <- dat$site.x
dat$designation[dat$designation == "REF"] <- "Reference"
ccfrp_north <- dat


load(file.path(dir,"survey_indices","ccfrp", "south", "Filtered_data_CCFRP.RData"))
dat$lat <- dat[, "startLat"]
dat$lon <- dat[, "startLong"]
dat$designation <- dat$site.x
dat$designation[dat$designation == "REF"] <- "Reference"
ccfrp_south <- dat


#===============================================================================
# Load in NWFSC HKL data
#===============================================================================

load(file.path(dir, "nwfsc_hkl", "filtered_species_data_nwfsc_hkl.rdata"))
hkl <- species_data
hkl$lat <- hkl$drop_latitude_degrees            
hkl$lon <- -1 * hkl$drop_longitude_degrees
hkl$designation <- "Reference"
hkl$designation[hkl$cca == 1] <- "MPA"

site <- expand.grid(
  year = unique(hkl$year),
  site_number = unique(hkl$site_number))

## join in location info for all sites
locs <- dplyr::group_by(hkl, site_number) %>%
  dplyr::summarise(
    lat = lat[1],
    lon = lon[1])

grid <- dplyr::left_join(site, locs) %>%
  dplyr::filter(!is.na(lat + lon))

grid$designation <- "Reference"
find <- as.numeric(as.character(grid$site_number)) 
ind <- which(find > 500)
grid$designation[ind] <- "MPA"

#===============================================================================
# Load in CDFW ROV
#===============================================================================

load(file.path(dir, "survey_indices", "rov", "rov_south_data_used_for_index_creation.rdata"))
# rov_south
rov_south$designation <- droplevels(rov_south$designation)
load(file.path(dir, "survey_indices", "rov", "rov_north_data_used_for_index_creation.rdata"))
# rov_north

#===============================================================================
# Plot all the sampling locations
#===============================================================================

# Create one single df in order to have ggplot automatically add legend
all_data <- data.frame(
  lat = c(ccfrp_south$lat, rov_south$lat, grid$lat),
  lon = c(ccfrp_south$lon, rov_south$lon, grid$lon),
  designation = c(ccfrp_south$designation, as.character(rov_south$designation), grid$designation),
  source = c(rep("CCFRP", dim(ccfrp_south)[1]), rep("ROV", dim(rov_south)[1]), rep("NWFSC HKL", dim(grid)[1]))
)


all_north <- data.frame(
  lat = c(ccfrp_north$lat, rov_north$lat),
  lon = c(ccfrp_north$lon, rov_north$lon),
  designation = c(ccfrp_north$designation, as.character(rov_north$designation)),
  source = c(rep("CCFRP", dim(ccfrp_north)[1]), rep("ROV", dim(rov_north)[1]))
)




# South ========================================================================
lon_range <- c(min(rov_south$lon, grid$lon, ccfrp_south$lon), 
               max(rov_south$lon, grid$lon, ccfrp_south$lon))
lat_range <- c(32.2, 34.6)

color <- viridis::viridis(3)
names(color) <- levels(all_data$source)

ggplot() +
  #scale_size_continuous("Count", breaks = c(1, 5, 10, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) +
  geom_point(data = all_data, aes(x = lon, y = lat, color = source, shape = source), 
             alpha = 1, size = 2) + 
  geom_point(data = all_data[all_data$source == "CCFRP", ], aes(x = lon, y = lat), color = color[1], size = 2) +
  scale_colour_manual(name = "source",values = color)
ggsave(file = file.path(dir, "survey_indices", "plots", "south_survey_locations.png"),
       width = 10, height = 8)

ggplot() +
  #scale_size_continuous("Count", breaks = c(1, 5, 10, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) +
  geom_point(data = all_data, aes(x = lon, y = lat, color = designation, shape = designation), 
             alpha = 1, size = 2) + 
  scale_colour_manual(name = "designation",values = color)
ggsave(file = file.path(dir, "survey_indices", "plots", "south_survey_locations_designation.png"),
       width = 10, height = 8)


# North ========================================================================
lon_range <- c(min(rov_north$lon, ccfrp_north$lon, na.rm = TRUE), 
               -120.2) #max(rov_north$lon, ccfrp_north$lon, na.rm = TRUE))
lat_range <- c(34.6, 42.0)#min(rov_south$lat, grid$lat, ccfrp_south$lat), 
# max(rov_south$lat, grid$lat, ccfrp_south$lat))


ggplot() +
  #scale_size_continuous("Count", breaks = c(1, 5, 10, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) +
  geom_point(data = all_north, aes(x = lon, y = lat, color = source, shape = source), 
             alpha = 1, size = 3) + 
  #geom_point(data = all_data[all_north$source == "CCFRP", ], aes(x = lon, y = lat), color = color[1], size = 2) +
  scale_colour_manual(name = "source",values = color)
ggsave(file = file.path(dir, "survey_indices", "plots", "north_survey_locations.png"),
       width = 8, height = 12)

ggplot() +
  #scale_size_continuous("Count", breaks = c(1, 5, 10, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) +
  geom_point(data = all_north, aes(x = lon, y = lat, color = designation, shape = designation), 
             alpha = 1, size = 3) + 
  scale_colour_manual(name = "designation",values = color)
ggsave(file = file.path(dir, "survey_indices", "plots", "north_survey_locations_designation.png"),
       width = 8, height = 12)

#===============================================================================
# Create df with the year, location, number and source
#===============================================================================

# Create one single df in order to have ggplot automatically add legend
n_south <- data.frame(
  year = c(ccfrp_south$year, rov_south$year, hkl$year),
  lat = c(ccfrp_south$lat, rov_south$lat, hkl$lat),
  lon = c(ccfrp_south$lon, rov_south$lon, hkl$lon),
  n = c(ccfrp_south$Target, rov_south$n, hkl$number_caught),
  designation = c(ccfrp_south$designation, as.character(rov_south$designation), hkl$designation),
  source = c(rep("CCFRP", dim(ccfrp_south)[1]), rep("ROV", dim(rov_south)[1]), rep("NWFSC HKL", dim(hkl)[1]))
)


n_north <- data.frame(
  year = c(ccfrp_north$year, rov_north$year),
  lat = c(ccfrp_north$lat, rov_north$lat),
  lon = c(ccfrp_north$lon, rov_north$lon),
  n = c(ccfrp_north$Target, rov_north$n),
  designation = c(ccfrp_north$designation, as.character(rov_north$designation)),
  source = c(rep("CCFRP", dim(ccfrp_north)[1]), rep("ROV", dim(rov_north)[1]))
)

hkl_site <- hkl %>%
  group_by(year, site_number, designation) %>%
  summarise(
    site_lat = mean(lat),
    site_lon = mean(lon),
    n = sum(number_caught)
  )
hkl_site$year <- as.numeric(as.character(hkl_site$year))
hkl_site$source <- "NWFSC HKL"

ccfrp_site <- ccfrp_south %>%
  group_by(year, area, designation) %>%
  reframe(
    site_lat = mean(lat),
    site_lon = mean(lon),
    n = sum(Target)
  )  
ccfrp_site$source <- "CCFRP"

rov_site <- rov_south %>%
  group_by(year, mpa_group, designation) %>%
  reframe(
    site_lat = mean(lat),
    site_lon = mean(lon),
    n = sum(n)
  )
rov_site$source <- "ROV"

all <- rbind(hkl_site[, c("year", "designation", "site_lat", "site_lon", "n", "source")],
             ccfrp_site[, c("year", "designation", "site_lat", "site_lon", "n", "source")],
             rov_site[, c("year", "designation", "site_lat", "site_lon", "n", "source")])

lon_range <- c(min(rov_south$lon, grid$lon, ccfrp_south$lon), 
               max(rov_south$lon, grid$lon, ccfrp_south$lon))
lat_range <- c(32.2, 34.6)

colors <- viridis::viridis(3)
names(colors) <- levels(all_data$source)

ggplot() +
  #geom_point(data = all_data, aes(x = lon, y = lat), alpha = 1, size = 2, color = 'grey') +
  geom_point(data = all, aes(x = site_lon, y = site_lat, size = n), color = colors[1], alpha = 1) +
  scale_size_binned("Count", breaks = c(1, 10, 25, 50,  100, 200, 300)) +
  #scale_colour_manual(name = "Source", values = colors) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) +
  facet_wrap("year", ncol = 4, nrow = 5)
ggsave(file = file.path(dir, "survey_indices", "plots", "south_count_by_year.png"),
       width = 12, height = 15)

ggplot() +
  geom_point(data = all_data, aes(x = lon, y = lat), alpha = 1, size = 2, color = 'grey') +
  geom_point(data = all, aes(x = site_lon, y = site_lat, size = n, fill = source, color = source),  alpha = 0.5) +
  scale_size_binned("Count", breaks = c(1, 10, 25, 50,  100, 300)) +
  #scale_colour_manual(name = "Source", values = colors) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) 
ggsave(file = file.path(dir, "survey_indices", "plots", "south_count_by_survey.png"),
       width = 12, height = 12)

ggplot() +
  #geom_point(data = all_data, aes(x = lon, y = lat), alpha = 1, size = 2, color = 'grey') +
  geom_point(data = all, aes(x = site_lon, y = site_lat, size = n, color = designation),  alpha = 1) +
  scale_size_binned("Count", breaks = c(1, 10, 25, 50,  100, 300)) +
  scale_colour_manual(name = "Source", values = colors) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(lon_range), c(lat_range)) 
ggsave(file = file.path(dir, "survey_indices", "plots", "south_count_by_designation.png"),
       width = 12, height = 12)
  