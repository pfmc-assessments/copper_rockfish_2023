#############################################################
#       Explore CDFW ROV Data 10m Segments
#           for Copper Rockfish 
#             Chantel Wetzel
#############################################################

library(here)
library(ggplot2)
library(nwfscSurvey)
library(dplyr)

dir <- file.path(here(), "data", "survey_indices", "rov")
rov_orig <- read.csv(file.path(dir, "rov_data_10m_2023.csv"))
# This new file removes records with no usable area fished calculations
rov <- read.csv(file.path(dir, "ROVData10mMaster2023-3_1_23.csv"))
# Transect level data provided by John Budrick
rov_south <- read.csv(file.path(dir, "TransectNorth.csv"))
rov_north <- read.csv(file.path(dir, "TransectSouth.csv"))
rov_orig <- as.data.frame(rov_orig)
rov <- as.data.frame(rov)

rov$ave_depth_bin <- plyr::round_any(as.numeric(rov$Avg_Depth), 2, floor)
rov$ave_lat_bin <- plyr::round_any(as.numeric(rov$Avg_Lat), 0.1, floor)
rov$ave_lon_bin <- plyr::round_any(as.numeric(rov$Avg_Lon), 0.1, floor)
rov$Copper.Rockfish[is.na(rov$Copper.Rockfish)] <- 0
rov$area <- 'north'
rov$area[rov$Avg_Lat < 34.5] <- 'south'

pos_rov <- rov[rov$Copper.Rockfish > 0, ]

quantile(pos_rov$ave_depth_bin, na.rm = TRUE)

transects_by_year <- aggregate(LineID~SurveyYear+LongTerm_Region, rov, function(x) sum(length(x)))

depth_ranges <- aggregate(ave_depth_bin~SurveyYear + area, rov, quantile)
quantile(rov$ave_depth_bin, na.rm = TRUE)
depth_ranges_w_copper <- aggregate(ave_depth_bin~SurveyYear + area, pos_rov, quantile)
quantile(pos_rov$Avg_Depth, na.rm = TRUE)

# Look at the observations of copper across depths
ggplot(pos_rov, aes(y = Copper.Rockfish, x = ave_depth_bin))  +  
  geom_histogram(aes(y = Copper.Rockfish), position="stack", stat="identity") + 
  xlab("Depth (fm)") + ylab("Total Observations") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) +
  facet_wrap(facets = c("SurveyYear")) + 
  scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "rov_copper_by_depth_count.png"),
       width = 10, height = 8)

pos_by_lat <- aggregate(Copper.Rockfish~ave_lat_bin, pos_rov, sum)
pos_by_year <- aggregate(Copper.Rockfish~SurveyYear + LongTerm_Region, rov, sum)
pos_by_region <- aggregate(Copper.Rockfish~SuperYear + LongTerm_Region, rov, sum)
pos_by_area <- aggregate(Copper.Rockfish~SuperYear + area, rov, sum)

# Look at the maximum time a area has been protected by area
table(rov$SurveyYear, rov$Years_since_imp)
# There is something strange going on with this column since the maximum
# years an area has been protected does not align with the latest year.
table(rov$Years_since_imp[rov$SurveyYear == 2014], rov$area[rov$SurveyYear == 2014])
table(rov$Years_since_imp[rov$SurveyYear == 2021], rov$area[rov$SurveyYear == 2021])

find = which(rov$Designation == "MPA" & rov$SurveyYear == 2014)
table(rov$MPAGroup[find], rov$Years_since_imp[find])
find = which(rov$Designation == "MPA" & rov$SurveyYear == 2021)
table(rov$MPAGroup[find], rov$Years_since_imp[find])

rov_transect <- rov %>%
  group_by(SurveyYear, SuperYear, LineID, Designation, MPAGroup, area) %>% 
  summarise(n = sum(Copper.Rockfish),
            lat = mean(Avg_Lat), 
            lon = mean(Avg_Lon),
            depth = mean(as.numeric(Avg_Depth), na.rm = TRUE),
            usable_dist = sum(Usable_XYdist),
            usable_area = sum(Usable_Area_Fish),
            prop_hard = sum(Propn_Hard),
            prop_mixed = sum(Propn_Mixed),
            prop_soft = sum(Propn_Soft)
   )

unique_transects <- table(rov_transect$LineID)
find <- which(unique_transects > 1)
duplicate <- names(unique_transects[find])
# All the duplicate seem to come from
View(rov_transect[rov_transect$LineID %in% duplicate, ])

# The range here for each of this is very large. I think we may
# want to cut off the outliers on both sides
quantile(rov_transect$usable_dist)
quantile(rov_transect$usable_area)
plot(rov_transect$usable_area, rov_transect$usable_dist)

bad_area <- which(rov_transect$usable_area > quantile(rov_transect$usable_area, 0.95) |
            rov_transect$usable_area < quantile(rov_transect$usable_area, 0.05)) 
bad_dist <- which(rov_transect$usable_dist < quantile(rov_transect$usable_dist, 0.05) | 
                  rov_transect$usable_dist > quantile(rov_transect$usable_dist, 0.90))
rov_sub <- rov_transect[-c(bad_area), ]
# 182 records removed based on area
plot(rov_sub$usable_dist, rov_sub$usable_area)

# Cut out sites at depths we would not expect to see copper rockfish
min <- min(rov_sub[rov_sub$n > 0, "depth"])
max <- max(rov_sub[rov_sub$n > 0, 'depth'])
quantile(rov_sub$depth)
remove <- rov_sub$depth < 20 | rov_sub$depth > 100
# 14 transects removed
rov_sub <- rov_sub[-remove, ]

# Check for location that were visited in either super year period
super_year_samples <- table(rov_sub$MPAGroup, rov_sub$SuperYear)

# Remove the N Farrallon Islands since this only occurs in the 2020 super year
remove <- which(rov_sub$MPAGroup == "N Farallon Islands")
rov_sub <- rov_sub[-remove, ]

# Remove records that area MPA/Outide (39 recordsfrom Swami)
rov_sub <- rov_sub[rov_sub$Designation != "MPA/Outside", ]

obs_by_des_loc <- aggregate(LineID ~ SuperYear + area + MPAGroup + Designation, rov_sub, length)
# south: reference Anacapa Island only visited in one super year period
# north: reference Piedras Blancas only visited in one super year period
remove_s <- which(rov_sub$area == "south" & rov_sub$Designation == "Reference" & rov_sub$MPAGroup == "Anacapa Island")
remove_n <- which(rov_sub$area == "north" & rov_sub$Designation == "Reference" & rov_sub$MPAGroup == "Piedras Blancas")
rov_sub <- rov_sub[-c(remove_s, remove_n), ]

obs_by_des_loc <- aggregate(LineID ~ SuperYear + area + MPAGroup + Designation, rov_sub, length)
write.csv(obs_by_des_loc, file = file.path(dir, "forSS", "transects_by_mpa_designation_super_year.csv"), row.names = FALSE)

copper_obs <- aggregate(n ~ SuperYear + area + Designation + MPAGroup, rov_sub, sum)
write.csv(copper_obs, file = file.path(dir, "forSS", "copper_obs_designation_mpa_group_super_year.csv"),  row.names = FALSE)

rov_sub_pos <- rov_sub[rov_sub$n > 0, ] 

quantile(rov_sub_pos$prop_hard, na.rm = TRUE)
quantile(rov_sub_pos$prop_mixed, na.rm = TRUE)
quantile(rov_sub_pos$prop_soft, na.rm = TRUE)

ggplot(rov_sub_pos, aes(x = prop_hard, y = n, color = 'red', alpha = 0.4)) +
  geom_jitter() + 
  geom_point(aes(x = prop_mixed, y = n, color = 'green', alpha = 0.4)) +
  geom_point(aes(x = prop_soft, y = n, color = 'blue', alpha = 0.4)) 

table(rov_sub$Designation)

south <- rov_sub[rov_sub$area == 'south', ]
south_samples <- aggregate(n ~ Designation + MPAGroup + SuperYear, south, sum)
south_transects <- aggregate(LineID ~  Designation + MPAGroup + SuperYear, south, length)

out <- cbind(south_transects, south_samples)
colnames(out) <- c("Designation", "MPA Group", "Super Year", "Transects", "Observations")
write.csv(out, file = file.path(dir, "forSS", "south_samples_transects.csv"), row.names = FALSE)

north <- rov_sub[rov_sub$area == 'north', ]
north_samples <- aggregate(n ~ Designation + MPAGroup + SuperYear, north, sum)
north_transects <- aggregate(LineID ~  Designation + MPAGroup + SuperYear, north, length)

out <- cbind(north_transects, north_samples)
colnames(out) <- c("Designation", "MPA Group", "Super Year", "Transects", "Observations")
write.csv(out, file = file.path(dir, "forSS", "north_samples_transects.csv"), row.names = FALSE)


#Collapse across the number of transects to get a sum total seen in a location for mapping
rov_transect_collapsed <- rov_sub %>%
  group_by(SuperYear,Designation, MPAGroup, area) %>% 
  summarise(n = sum(n),
            lat = mean(lat), 
            lon = mean(lon)
  )


south <- rov_transect_collapsed[rov_transect_collapsed$area == "south", ]
ggplot() +
  scale_size_continuous("Count", breaks = c(1, 10, 25, 50, seq(100, 500, 100))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(min(south$lon), max(south$lon)), c(min(south$lat), max(south$lat))) +
  geom_point(data = south, aes(x = lon, y = lat, size = n, color = Designation)) + 
  facet_grid(SuperYear~.) +
  scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "rov_transect_collapsed_copper_south_protection_count.png"),
       width = 10, height = 8)


north <- rov_transect_collapsed[rov_transect_collapsed$area == 'north', ]
ggplot() +
  scale_size_continuous("Count", breaks = c(1, 5, 10, 25, 50, 75, 100)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(min(north$lon), max(north$lon)), c(min(north$lat), max(north$lat))) +
  geom_point(data = north, aes(x = lon, y = lat, size = n, color = Designation)) + 
  facet_wrap("SuperYear") +
  scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "rov_transect_collapsed_copper_north_protection_count.png"),
       width = 14, height = 8)


rov_sub$rate <- rov_sub$n / rov_sub$usable_area
sd(rov_sub$rate)

#Collapse across the number of transects to get a sum total seen in a location for mapping
copper_ob_rate <- rov_sub[rov_sub$Designation != "MPA/Outside", ] %>%
  group_by(SuperYear, area, Designation) %>% 
  summarise(mean = mean(rate),
            sd = sd(rate)
  )

ggplot(copper_ob_rate, 
       aes(x = SuperYear, y = mean, ymin = mean - sd, ymax = mean + sd, color = Designation)) +
  geom_line() + 
  geom_point() + 
  geom_pointrange(aes(ymin = mean - sd, ymax = mean + sd,
        linetype = Designation)) +
  facet_grid(area~.) +
  xlab("Super Year") + ylab("CPUE") +
  scale_fill_viridis_c()
ggsave(filename = file.path(dir, "plots", "rov_cpue_mean_sd_designation.png"),
       width = 8, height = 8)


copper_ob_rate$protection <- c(0.2, 0.8, 0.42, 0.58, 0.2, 0.80, 0.33, 0.67)
copper_ob_rate$index <- copper_ob_rate$mean * copper_ob_rate$protection
#copper_ob_rate$index_se <- copper_ob_rate$se^2 * copper_ob_rate$protection^2

index <- aggregate(index~SuperYear + area, copper_ob_rate, sum)
#cv <- aggregate(index_se~SuperYear + area, copper_ob_rate, sum)

index <- cbind(index, cv$index_se)
colnames(index) <- c("SuperYear", "area", "est")#, "se")
index$hi <- stats::qlnorm(0.975, meanlog = log(index$est), sdlog = index$se)
index$lo <- stats::qlnorm(0.025, meanlog = log(index$est), sdlog = index$se)

ggplot(index, 
       aes(x = SuperYear, y = est,  color = area)) +
  geom_line() + 
  geom_point() + 
  #geom_pointrange(aes(ymin = lo, ymax = hi,linetype = area)) +
  xlab("Super Year") + ylab("CPUE") +
  scale_fill_viridis_c()
ggsave(filename = file.path(dir, "plots", "rov_cpue_weighted.png"),
       width = 8, height = 8)

#===========================================================================
# Run sdmTMB
#===========================================================================
library(sdmTMB)

save(rov_sub, file = file.path(dir, "rov_data_for_glm.rdata"))
data <- rov_sub

habitat <- data %>%
  group_by(Line)

# Convert the latitude and longitude to WGS projections  
data_trans <- data
coordinates(data_trans) <- c("lon", "lat")
proj4string(data_trans) <- CRS("+proj=longlat +datum=WGS84")
newproj <- paste("+proj=utm +zone=10 ellps=WGS84 +datum=WGS84")
data_trans <- spTransform(data_trans, CRS(newproj))
data_trans <- as.data.frame(data_trans)
data$X <- data_trans$lon / 1000 # convert to km
data$Y <- data_trans$lat / 1000 # convert to km

year_site <- expand.grid(
  SuperYear = unique(data$SuperYear),
  MPAGroup = unique(data$MPAGroup),
  depth = data$depth,
  prop_hard = data$prop_hard,
  prop_soft = data$prop_soft,
  prop_mixed = data$prop_mixed)

## join in location info for all sites
locs <- dplyr::group_by(data, MPAGroup) %>%
  dplyr::summarise(
    Y = Y[1],
    X = X[1])

grid <- dplyr::left_join(year_site, locs) %>%
  dplyr::filter(!is.na(X + Y))
#grid$effort <- 'fill'

# Split data by area and designation and run 4 separate
# indices. The GLM would include n ~ year + location
# with a offset of usable area

# South MPA ==============================================================
use <- data$area == "south" & data$Designation == "MPA"
locs <- data[use, ] %>%
  dplyr::group_by(MPAGroup, depth) %>%
  dplyr::summarise(
    Y = Y[1],
    X = X[1])
grid <- dplyr::left_join(year_site, locs) %>%
  dplyr::filter(!is.na(X + Y))

fit_sp <- sdmTMB(
  n ~ 0 + as.factor(SuperYear) + as.factor(MPAGroup),# + poly(log(depth), 2),
  data = data[use, ],
  offset = log(data$usable_area[use]),
  time = "SuperYear",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")#,
  #silent = TRUE,
  #do_index = TRUE,
  #predict_args = list(newdata = grid, re_form_iid = NA),
  #index_args = list(area = 1),
  #control = sdmTMBcontrol(newton_loops = 1)
)



index_sp <- get_index(fit_sp, bias_correct = TRUE)

# South Reference Areas =======================================
use <- data$area == "south" & data$Designation == "Reference"
locs <- data[use, ] %>%
  dplyr::group_by(MPAGroup, depth) %>%
  dplyr::summarise(
    Y = Y[1],
    X = X[1])

grid <- dplyr::left_join(year_site, locs) %>%
  dplyr::filter(!is.na(X + Y))

fit_sr <- sdmTMB(
  n ~ 0 + as.factor(SuperYear) + as.factor(MPAGroup),# + poly(log(depth),2),
  data = data[use, ],
  offset = log(data$usable_area[use]),
  time = "SuperYear",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  silent = TRUE,
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),
  index_args = list(area = 1),
  control = sdmTMBcontrol(newton_loops = 1)
)

index_sr <- get_index(fit_sr, bias_correct = TRUE)

tidy(fit_sr, effects = "ran_pars")

nd <- data.frame(depth = seq(20, 100, length.out = 300))
p <- predict(
  fit_sr, 
  newdata = nd, 
  se_fit = TRUE
)
p$lwr <- plogis(p$est - 1.96 * p$est_se)
p$upr <- plogis(p$est + 1.96 * p$est_se)





