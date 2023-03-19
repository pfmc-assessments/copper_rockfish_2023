###################################################################################
#
#       Copper rockfish 2023
#        NWFSC HKL survey 
#    	  data exploration 
#
#############################################################################################

library(here)
library(ggplot2)
library(dplyr)
library(HandyCode)
library(nwfscSurvey)

dir <- file.path(here(), "data", "nwfsc_hkl")

hkl_all <- read.csv(file.path(dir, "H&LSurveyDataThru2022_DWarehouse version_03042023.csv"))
hkl_all$lat <- hkl_all$drop_latitude_degrees
hkl_all$lon <- hkl_all$drop_longitude_degrees

# This is also findable via the cowcod_conservation_area_indicator column
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

# Filter down to only copper observations
hkl <- hkl_all[ind, ]
hkl$length_bin <- plyr::round_any(hkl$length_cm, 1, floor)

table(hkl$year, hkl$include_fish)
hkl <- hkl[hkl$include_fish == 1, ]

sub_hkl_all_site <- hkl_all_site[hkl_all_site$total_count > 0, ]
colors <- viridis::viridis(10)[c(1,5,9)]

hkl$Length_cm <- hkl$length_cm
hkl$Sex <- hkl$sex

nwfscSurvey::PlotSexRatio.fn(
	dir = dir, 
	dat = hkl)

ggplot() +
	geom_jitter() + 
	geom_point(data = hkl_all_site, aes(x = site_lon, y = site_lat, col = site_area), size = 5, pch = 1) + 
	geom_point(data = sub_hkl_all_site, aes(x = site_lon, y = site_lat, size = total_count), 
		color = colors[2]) + 
	scale_size_continuous("Count", breaks = c(1, 10, seq(25, 125, 25))) +
	scale_fill_manual("Site", values = c('CCA'  = colors[1], 'Outside CCA' = colors[3])) +
    scale_color_manual("Site", values = c('CCA' = colors[1], 'Outside CCA' = colors[3])) +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(min(hkl_all_site$site_lon), max(hkl_all_site$site_lon)), c(min(hkl_all_site$site_lat), max(hkl_all_site$site_lat))) +
	theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 14),
      	legend.text = element_text(size = 14)) +
	xlab("Longitude") + ylab("Latitude") 
ggsave(filename = file.path(dir, "plots", "hkl_copper_by_site_count_all_years.png"),
	width = 10, height = 8)


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
# 101        1097
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

ggplot(hkl[hkl$sex != "U",], aes(y = count, x = length_bin, fill = sex))  +  
  geom_histogram(aes(y = count), position="stack", stat="identity") + 
  xlab("Length (cm)") + ylab("Total Observations") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) +
  facet_grid(area~.) + 
  scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(filename = file.path(dir, "plots", "hkl_observations_by_length_sex_area.png"),
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

aggregate(length_cm~area, hkl[hkl$count > 0, ], quantile)
aggregate(fathom_bin~area, hkl[hkl$count > 0, ], quantile)

ggplot(hkl, aes(y = count, x = fathom_bin, fill = as.factor(hook_number)))  +  
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Depth (fm)") + ylab("Total Observations") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "hkl_observations_by_hook_number_depth.png"),
	width = 10, height = 8)


ggplot(hkl, aes(y = count, x = swell_height_m, fill = area))  +  
  geom_histogram(aes(y = count), position="stack", stat="identity") + 
  xlab("Swell Height (m)") + ylab("Total Observations") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) +
  scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "hkl_observations_by_swell_height.png"),
       width = 10, height = 8)

ggplot(hkl, aes(y = count, x = swell_height_m, fill = hook_number))  +  
  geom_histogram(aes(y = count), position="stack", stat="identity") + 
  xlab("Swell Height (m)") + ylab("Total Observations") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) +
  scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "hkl_observations_by_swell_height_and_hook.png"),
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

on_bottom_time_seconds

ggplot(hkl, aes(y = count, x = on_bottom_time_seconds, fill = as.factor(hook_number)))  + 
	geom_bar(aes(y = count), position="stack", stat="identity") + 
    xlab("Time on Bottom (seconds)") + ylab("Numbers Caught") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()

ggplot(hkl, aes(y = count, x = length_bin, fill = as.factor(hook_number)))  + 
	geom_bar(aes(y = count), position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Numbers Caught") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "hkl_length_by_hook_number.png"),
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

find = which(hkl_all$hook_number %in% 4:5 & hkl_all$common_name == "Copper Rockfish")
grab = hkl_all[find,'set_id']
sub = hkl_all[hkl_all$set_id %in% grab, ]

ggplot(sub, aes(x = fathom_bin))  +  
	geom_bar(aes(fill = common_name), position="fill") + 
    xlab("Depth (fm)") + ylab("Total Observations") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()

ggplot(hkl, aes(x = length_cm, y = weight_kg)) +
	geom_jitter() + 
	geom_point(aes(col = as.factor(include_fish)), size = 2) +
	scale_colour_viridis_d() + 
	xlim(0, 60) + ylim(0, 3) + 
	xlab("Length (cm)") + ylab("Weight (kg)") 

ggplot(hkl, aes(y = count, x = length_bin, fill = as.factor(include_fish)))  + 
	geom_bar(aes(y = count), position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Numbers Caught") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()

table(hkl$sex, hkl$include_fish)

ggplot(hkl, aes(y = count, x = sex, fill = as.factor(include_fish)))  + 
	geom_bar(aes(y = count), position="stack", stat="identity") + 
    xlab("Sex") + ylab("Numbers Caught") +
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()

#==============================================================================
# look at what else is on the line when copper is on hook 5
#==========================================================================

find <- which(hkl_all$hook_number == 5 & hkl_all$common_name == "Copper Rockfish")
hkl_all$unq_id <- paste0(hkl_all[, "set_id"], "_", hkl_all[, "drop_number"], "_", hkl_all[, "angler_number"] )

data <- hkl_all[hkl_all$unq_id %in% hkl_all[find, "unq_id"], ]
table(data$common_name)

occur <- table(data$common_name)
keep <- names(occur[occur > 1])
occur[keep]

ggplot(data[data$common_name %in% keep, ], aes(x = hook_number, fill = common_name))  + 
  geom_bar(position="stack") + 
  xlab("Hook Position") + ylab("Numbers Caught") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) 
ggsave(filename = file.path(dir, "plots", "hkl_composition_with_copper_on_top_hook.png"),
       width = 10, height = 8)


# Look at all instances when copper is observed
find <- which(hkl_all$common_name == "Copper Rockfish")
unq_set_id <- hkl_all[find, "unq_id"]

data <- hkl_all %>%
  filter(unq_id %in% unq_set_id) 

occur <- table(data$common_name)
keep <- names(occur[occur > 20])
occur[keep]

ggplot(data[data$common_name %in% keep, ], aes(x = hook_number, fill = common_name))  + 
  geom_bar(position="stack") + 
  xlab("Hook Position") + ylab("Numbers Caught") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) 
ggsave(filename = file.path(dir, "plots", "hkl_composition_with_copper_on_any_hook.png"),
       width = 10, height = 8)

sub_species <- which(hkl_all$common_name %in% c("Copper Rockfish", "Bocaccio", "Vermilion Rockfish"))
data <- hkl_all[sub_species, ]

ggplot(data, aes(x = year, fill = common_name))  + 
  geom_bar(position="fill") + 
  xlab("Year") + ylab("Numbers Caught") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) 
ggsave(filename = file.path(dir, "plots", "hkl_prop_year_with_copper_bocaccio_vermilion.png"),
       width = 10, height = 8)

####Look at copper hook locations by lines where copper observed 
# mhm 3/6/23
#get lines that encountered copper
copper_lines <- hkl %>%
  dplyr::select(set_id, drop_number, angler_number) %>%
  unique()

#join to all data
lines_with_copper <- left_join(copper_lines, hkl_all, 
                               by = c("set_id", "drop_number", "angler_number"),
                               keep = FALSE)

#make sure join gets you the correct number of drops
dat_check <- lines_with_copper %>%
  dplyr::select(set_id, drop_number, angler_number) %>%
  unique()

#look at lines by species position
#could turn into color coded matrix
hook_table <- lines_with_copper %>%
  mutate(hook_number = as.factor(hook_number),
         common_name = ifelse(common_name == '', '0', common_name)) %>%
  dplyr::select(set_id, drop_number, angler_number, common_name, hook_number) %>% 
  group_by(set_id, drop_number, angler_number) %>%
  pivot_wider(names_from = hook_number, values_from = common_name)
View(hook_table)

#how often is copper observed only on hooks 4 or 5
copp_4_5 <- hook_table %>%
  filter(`4` == "Copper Rockfish" | `5` =="Copper Rockfish") %>%
  filter(`1` != "Copper Rockfish",
         `2` != "Copper Rockfish",
         `3` != "Copper Rockfish" )
#only 66 total lines


raw.cpue <- hkl_all %>%
  group_by(year) %>%
  summarize(
    sites = length(unique(site_number)),
    n = sum(count),
    effort = length(unique(angler_number)) * length(unique(hook_number)) * length(unique(drop_number)),
    cpue = n / effort,
    avg_cpue = mean(cpue)) 
  
  
ggplot(raw.cpue) +
  geom_line(aes(x = year, y = avg_cpue)) + 
  geom_point(aes(x = year, y = avg_cpue), size = 2) +
  ylim(c(0, 2))
ggsave(file = file.path(dir, "plots", 'raw_cpue_nwfsc_hkl.png'), width = 7, height = 7)
