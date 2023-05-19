#############################################################
#       Model the CDFW ROV Data at the Transect Level
#                 for Copper Rockfish 
#                   Chantel Wetzel
#############################################################

library(here)
library(ggplot2)
library(dplyr)
library(sdmTMB)
library(HandyCode)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
library(tidybayes)
library(gridExtra)
library(fitdistrplus)

# Load in some helper functions for processing and plotting the data
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}
all <- list.files(file.path(user_dir, "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(user_dir, "R", "sdmTMB", all[a]))}

dir <- file.path(here(), "data", "survey_indices", "rov")
dir <- "//nwcfile/FRAM/Assessments/CurrentAssessments/copper_rockfish_2023/data/survey_indices/rov"
# Transect level data provided by John Budrick (corrected data provided on 5/19/2023)
rov_south <- read.csv(file.path(dir, "TransectDataSouth.csv"))
rov_north <- read.csv(file.path(dir, "TransectDataNorth.csv"))

# Give the columns more usable names
rov_south <- as_tibble(rov_south)
rov_south <- rov_south %>%
  rename(
    super_year = FirstOfSuperYear,
    year = FirstOfSurveyYear,
    designation = FirstOfDesignation,
    site = FirstOfSite, 
    mpa_group = FirstOfMPAGroup,
    lat = AvgOfAvg_Lat,
    lon = AvgOfAvg_Lon,
    #effort = AvgOfEffort,
    depth = AvgOfAvg_Depth,
    prop_hard = AvgOfPropn_Hard, #PropnHardTransect,
    prop_mixed = AvgOfPropn_Mixed, #PropnMixedTransect,
    prop_soft = AvgOfPropn_Soft, #PropnSoftTransect,
    usable_area = SumOfUsable_Area_Fish, 
    distance = SumOfUsable_XYdist,
    protection = FirstOfProtection,
    #imp_year = AvgOfYears_since_imp,
    #port_dist = AvgOfportdistanceM,
    n = SumOfCopper_Rockfish) %>%
  #filter(designation != "MPA/Outside") %>%
  mutate(
    super_year = as.factor(super_year),
    designation = as.factor(designation),
    protection = as.factor(protection), 
    mpa_group = as.factor(mpa_group),
    depth_scaled = (depth - mean(depth)) / sd(depth),
    depth_scaled_2 = depth_scaled^2,
    prop_soft_scaled = prop_soft - mean(prop_soft),
    prop_hard_scaled = prop_hard - mean(prop_hard),
    prop_mixed_scaled = prop_mixed - mean(prop_mixed)
  )

rov_south$n[is.na(rov_south$n)] <- 0

rov_north<- as_tibble(rov_north)
rov_north <- rov_north %>%
  rename(
    super_year = FirstOfSuperYear,
    year = FirstOfSurveyYear,
    designation = FirstOfDesignation,
    site = FirstOfSite, 
    mpa_group = FirstOfMPAGroup,
    lat = AvgOfAvg_Lat,
    lon = AvgOfAvg_Lon,
    #effort = AvgOfEffort,
    depth = AvgOfAvg_Depth,
    prop_hard = AvgOfPropn_Hard, #PropnHardTransect,
    prop_mixed = AvgOfPropn_Mixed, #PropnMixedTransect,
    prop_soft = AvgOfPropn_Soft, #PropnSoftTransect,
    usable_area = SumOfUsable_Area_Fish, 
    distance = SumOfUsable_XYdist,
    protection = FirstOfProtection,
    #imp_year = AvgOfYears_since_imp,
    #port_dist = AvgOfportdistanceM,
    n = SumOfCopper_Rockfish) %>%
  mutate(
    super_year = as.factor(super_year),
    designation = as.factor(designation),
    protection = as.factor(protection), 
    mpa_group = as.factor(mpa_group),
    depth_scaled = (depth - mean(depth)) / sd(depth),
    depth_scaled_2 = depth_scaled^2,
    prop_soft_scaled = prop_soft - mean(prop_soft),
    prop_hard_scaled = prop_hard - mean(prop_hard),
    prop_mixed_scaled = prop_mixed - mean(prop_mixed)
  )
rov_north$n[is.na(rov_north$n)] <- 0


#==========================================================================
# Clean the data
#==========================================================================

# Remove extreme areas for each area
bad_area <- which(rov_south$usable_area > quantile(rov_south$usable_area, 0.98) |
                  rov_south$usable_area < quantile(rov_south$usable_area, 0.02)) 
filtered_data_south <- c("Records with usable area outside the 96th quantile", length(bad_area))
rov_south <- rov_south[-bad_area, ]

bad_area <- which(rov_north$usable_area > quantile(rov_north$usable_area, 0.98) |
                  rov_north$usable_area < quantile(rov_north$usable_area, 0.02)) 
filtered_data_north <- c("Records with usable area outside the 96th quantile", length(bad_area))
rov_north <- rov_north[-bad_area, ]

# Cut out sites at depths we would not expect to see copper rockfish
min <- min(rov_south[rov_south$n > 0, "depth"], rov_north[rov_north$n > 0, "depth"])
max <- max(rov_south[rov_south$n > 0, 'depth'], rov_north[rov_north$n > 0, "depth"])

remove <- which(rov_south$depth < min | rov_south$depth > max)
rov_south <- rov_south[-remove, ]
filtered_data_south <- rbind(filtered_data_south,
  c(paste0("Records with depths outside ", round(min, 1), " - ", round(max, 1), " m"), length(remove)))

remove <- which(rov_north$depth < min | rov_north$depth > max)
rov_north <- rov_north[-remove, ]
filtered_data_north <- rbind(filtered_data_north,
  c(paste0("Records with depths outside ", round(min, 1), " - ", round(max, 1), " m"), length(remove)))

# Remove records that area MPA/Outide 
filtered_data_south <- rbind(filtered_data_south,
  c("Transects that were both inside and outside MPA area", sum(rov_south$designation == "MPA/Outside")))

rov_south <- rov_south[rov_south$designation != "MPA/Outside", ]

obs_by_des_loc <- aggregate(FirstOfLineID ~ super_year + mpa_group + designation, rov_south, length)

remove_s <- which(rov_south$designation == "Reference" & rov_south$mpa_group == "Anacapa Island")
filtered_data_south <- rbind(filtered_data_south,
  c("Reference or MPA locations without sampling for at least three years", length(remove_s)))


obs_by_des_loc <- aggregate(FirstOfLineID ~ super_year + mpa_group + designation, rov_north, length)
remove_n <- c(which(rov_north$designation == "Reference" & rov_north$mpa_group %in% c("Piedras Blancas")),
              which(rov_north$mpa_group %in% c("N Farallon Islands")))
filtered_data_north <- rbind(filtered_data_north,
 c("Reference or MPA locations without sampling for both super years", length(remove_n)))

rov_south <- rov_south[-remove_s, ]
rov_north <- rov_north[-remove_n, ]

filtered_data_north <- rbind(filtered_data_north,
 c("Retained records", nrow(rov_north)))
filtered_data_south <- rbind(filtered_data_south,
  c("Retained records", nrow(rov_south)))

colnames(filtered_data_north) <- colnames(filtered_data_south) <- c("Removal reason", "Number")
write.csv(filtered_data_north, file = file.path(dir, "forSS", "rov_removed_records_north_05192023.csv"), row.names = FALSE)
write.csv(filtered_data_south, file = file.path(dir, "forSS", "rov_removed_records_south_05192023.csv"), row.names = FALSE)

save(rov_south, file = file.path(dir, "rov_south_data_used_for_index_creation_05192023.rdata"))
save(rov_north, file = file.path(dir, "rov_north_data_used_for_index_creation_05192023.rdata"))

rov_south$designation <- droplevels(rov_south$designation)
#=================================================================================
# Write out tables of remaining data
#==================================================================================

copper_obs <- aggregate(n ~ super_year + mpa_group + designation, rov_south, sum)
transects <- aggregate(FirstOfLineID ~  super_year + mpa_group + designation, rov_south, length)
both <- cbind(transects, copper_obs$n)
colnames(both) <- c("Super Year", "Area", "Designation", "Transects",  "Observations")
write.csv(both, file = file.path(dir, "forSS", "south_copper_obs_designation_mpa_group_super_year_05192023.csv"),  row.names = FALSE)

copper_obs <- aggregate(n ~ super_year + mpa_group + designation, rov_north, sum)
transects <- aggregate(FirstOfLineID ~  super_year + mpa_group + designation, rov_north, length)
both <- cbind(transects, copper_obs$n)
colnames(both) <- c("Super Year", "Area",  "Designation", "Transects", "Observations")
write.csv(both, file = file.path(dir, "forSS", "north_copper_obs_designation_mpa_group_super_year_05192023.csv"),  row.names = FALSE)

south_samples <- rov_south %>%
  group_by(year, mpa_group, designation) %>%
  reframe(
    transect = length(unique(FirstOfLineID)),
    n = sum(n)
  )
colnames(south_samples) <- c("Year", "Location", "Designation", "Transect", "Obs.")
write.csv(south_samples, file = file.path(dir, "forSS", "south_obs_designation_mpa_group_year_05192023.csv"),  row.names = FALSE)

transects_by_area <- rov_south %>%
  group_by(mpa_group, year) %>%
  reframe(
    n_mpa = sum(designation == "MPA"),
    n_ref = sum(designation == "Reference")
  )
colnames(transects_by_area) <- c("Location", "Year", "MPA", "Reference")
write.csv(transects_by_area, file = file.path(dir, "forSS", "south_obs_mpa_group_year_05192023.csv"),  row.names = FALSE)

transects_by_area <- rov_north %>%
  group_by(mpa_group, year) %>%
  reframe(
    n_mpa = sum(designation == "MPA"),
    n_ref = sum(designation == "Reference")
  )
colnames(transects_by_area) <- c("Location", "Year", "MPA", "Reference")
write.csv(transects_by_area, file = file.path(dir, "forSS", "north_obs_mpa_group_year_05192023.csv"),  row.names = FALSE)


#=================================================================================
# Create some visualization of the data
#==================================================================================

# South ========================================================================
ggplot(aes(x = n),
       data = rov_south) + geom_bar() + theme_bw() + 
  facet_wrap(c("designation")) +
  xlab("Numbers Observed by Transect") + ylab("Count")
ggsave(file = file.path(dir, "plots", "observations_south_05192023.png"), width =10, height = 7)


raw.cpue <- rov_south %>%
  mutate(cpue = n / usable_area) %>%
  group_by(super_year, designation) %>%
  summarize(avg_cpue = mean(cpue))

ggplot(aes(y = avg_cpue, x = super_year, colour = designation),
       data = raw.cpue) + geom_point(size = 3) + theme_bw() +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.006)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "south_raw_cpue_05192023.png"), width = 7, height = 7)

raw.cpue <- rov_south %>%
  mutate(cpue = n / usable_area) %>%
  group_by(year, designation) %>%
  summarize(avg_cpue = mean(cpue))

ggplot(aes(y = avg_cpue, x = year, colour = designation),
       data = raw.cpue) + geom_point(size = 3) + theme_bw() +
  geom_line(aes(x = year, y = avg_cpue, colour = designation)) +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.0075)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "south_raw_cpue_by_year_05192023.png"), width = 7, height = 7)


raw.cpue <- rov_south %>%
  mutate(cpue = n / usable_area) %>%
  group_by(super_year, mpa_group, designation) %>%
  summarize(avg_cpue = round(mean(cpue), 4), sd_cpue = sd(cpue))

ggplot(data = raw.cpue, aes(y = avg_cpue, x = super_year, colour = mpa_group)) + 
  geom_point(size = 3) + theme_bw() + 
  geom_line(aes(x = super_year, y = avg_cpue, colour = mpa_group, group = mpa_group)) +
  facet_wrap('designation') +
  #geom_errorbar(aes( ymin = avg_cpue - sd_cpue, ymax = avg_cpue + sd_cpue)) + 
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.015)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "south_raw_cpue_by_mpa_group_05192023.png"), width = 7, height = 7)

raw.cpue <- rov_south %>%
  mutate(cpue = n / usable_area) %>%
  group_by(year, mpa_group, designation) %>%
  summarize(avg_cpue = round(mean(cpue), 4), sd_cpue = sd(cpue))

ggplot(data = raw.cpue, aes(y = avg_cpue, x = year, colour = mpa_group)) + 
  geom_point(size = 3) + theme_bw() + 
  geom_line(aes(x = year, y = avg_cpue, colour = mpa_group)) +
  facet_wrap('designation') +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.0155)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "south_raw_cpue_by_mpa_group_by_year_05192023.png"), width = 7, height = 7)


raw.cpue <- rov_south %>%
  filter(mpa_group %in% c("Anacapa Island", "Carrington Point", "South La Jolla", "Point Conception", "Swami's")) %>%
  mutate(cpue = n / usable_area) %>%
  group_by(super_year, mpa_group, designation) %>%
  summarize(avg_cpue = round(mean(cpue), 4), sd_cpue = sd(cpue))

ggplot(data = raw.cpue, aes(y = avg_cpue, x = super_year, colour = mpa_group)) + 
  geom_point(size = 3, aes(shape = mpa_group)) + theme_bw() + facet_grid(designation~.) + 
  #geom_line(aes(x = super_year, y = avg_cpue, colour = mpa_group)) +
  #geom_errorbar(aes( ymin = avg_cpue - sd_cpue, ymax = avg_cpue + sd_cpue)) + 
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.008)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "south_raw_cpue_by_ccfrp_site_comparison.png"), width = 7, height = 7)

n_by_site <- rov_south %>%
  filter(mpa_group %in% c("Anacapa Island", "Carrington Point", "South La Jolla", "Point Conception", "Swami's")) %>%
  group_by(super_year, mpa_group, designation) %>%
  reframe(
    n_site = sum(n)
  )
write.csv(n_by_site, row.names = FALSE, 
          file = file.path(dir, "forSS", "south_observations_for_ccfrp_site.csv"))

# North ========================================================================
ggplot(aes(x = n),
       data = rov_north) + geom_bar() + theme_bw() + facet_grid(designation~.) +
  xlab("Numbers Observed by Transect") + ylab("Count")
ggsave(file = file.path(dir, "plots", "observations_north_05192023.png"), width = 7, height = 7)

raw.cpue <- rov_north %>%
  mutate(cpue = n / usable_area) %>%
  group_by(super_year, designation) %>%
  summarize(avg_cpue = mean(cpue))

ggplot(aes(y = avg_cpue, x = super_year, colour = designation),
       data = raw.cpue) + geom_point(size = 3) + theme_bw() +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.006)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "north_raw_cpue_05192023.png"), width = 7, height = 7)

raw.cpue <- rov_north %>%
  mutate(cpue = n / usable_area) %>%
  group_by(year, designation) %>%
  summarize(avg_cpue = mean(cpue))

ggplot(aes(y = avg_cpue, x = year, colour = designation),
       data = raw.cpue) + geom_point(size = 3) + theme_bw() +
  geom_line(aes(x = year, y = avg_cpue, colour = designation)) +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.006)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "north_raw_cpue_by_year_05192023.png"), width = 7, height = 7)

raw.cpue <- rov_north %>%
  mutate(cpue = n / usable_area) %>%
  group_by(super_year, mpa_group, designation) %>%
  summarize(avg_cpue = round(mean(cpue), 4), sd_cpue = sd(cpue))

ggplot(data = raw.cpue, aes(y = avg_cpue, x = super_year, colour = mpa_group)) + 
  geom_point(size = 3) + theme_bw() + facet_grid(designation~.) + 
  geom_line(aes(x = super_year, y = avg_cpue, colour = mpa_group, group = mpa_group)) +
  #geom_errorbar(aes( ymin = avg_cpue - sd_cpue, ymax = avg_cpue + sd_cpue)) + 
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.005)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "north_raw_cpue_by_mpa_group_05192023.png"), width = 7, height = 7)

raw.cpue <- rov_north %>%
  mutate(cpue = n / usable_area) %>%
  group_by(year, mpa_group, designation) %>%
  summarize(avg_cpue = round(mean(cpue), 4), sd_cpue = sd(cpue))

ggplot(data = raw.cpue, aes(y = avg_cpue, x = year, colour = mpa_group)) + 
  geom_point(size = 3) + theme_bw() + facet_grid(designation~.) + 
  geom_line(aes(x = year, y = avg_cpue, colour = mpa_group, group = mpa_group)) +
  #geom_errorbar(aes( ymin = avg_cpue - sd_cpue, ymax = avg_cpue + sd_cpue)) + 
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.005)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "north_raw_cpue_by_mpa_group_by_year_05192023.png"), width = 7, height = 7)


raw.cpue <- rov_north %>%
  filter(mpa_group %in% c("Ano Nuevo", "Bodega Bay", "Piedras Blancas", "Point Buchon", "Point Lobos", "Ten Mile")) %>%
  mutate(cpue = n / usable_area) %>%
  group_by(super_year, mpa_group, designation) %>%
  summarize(avg_cpue = round(mean(cpue), 4), sd_cpue = sd(cpue))

ggplot(data = raw.cpue, aes(y = avg_cpue, x = super_year, colour = mpa_group)) + 
  geom_point(size = 3, aes(shape = mpa_group)) + theme_bw() + facet_grid(designation~.) + 
  #geom_line(aes(x = super_year, y = avg_cpue, colour = mpa_group)) +
  #geom_errorbar(aes( ymin = avg_cpue - sd_cpue, ymax = avg_cpue + sd_cpue)) + 
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, 0.006)) + 
  scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "north_raw_cpue_by_ccfrp_site_comparison.png"), width = 7, height = 7)

n_by_site <- rov_north %>%
  filter(mpa_group %in% c("Ano Nuevo", "Bodega Bay", "Piedras Blancas", "Point Buchon", "Point Lobos", "Ten Mile")) %>%
  group_by(super_year, mpa_group, designation) %>%
  reframe(
    n_site = sum(n)
  )
write.csv(n_by_site, row.names = FALSE, 
          file = file.path(dir, "forSS", "north_observations_for_ccfrp_site.csv"))


#=================================================================================
# Create Predictions Grids for Each Area
#==================================================================================

# Create the grid
# Year and Sites
grid <- expand.grid(
  super_year = unique(rov_south$super_year),
  #site = unique(rov_south$mpa_group),
  designation = unique(rov_south$designation))

## join in location info for all sites
locs <- rov_south %>% 
  dplyr::group_by(super_year, designation) %>%
  dplyr::summarise(
    lat = lat[1],
    lon = lon[1],
    prop_soft_scaled = prop_soft_scaled[1],
    depth_scaled = depth_scaled[1],
    depth_scaled_2 = depth_scaled_2[1])

grid <- dplyr::left_join(grid, locs) %>%
  dplyr::filter(!is.na(lat + lon))
grid$year <- grid$super_year

n_open <- round(0.73 * 100, 0)
n_mpa  <- round(0.27 * 100, 0)

grid_south <- NULL
for (a in 1:n_open){
  grid_south <- rbind(grid_south, grid[grid$designation == "Reference", ])
}
for(a in 1:n_mpa){
  grid_south <- rbind(grid_south, grid[grid$designation == "MPA", ])
}


# North grid ===========================

grid <- expand.grid(
  super_year = unique(rov_north$super_year),
  #site = unique(rov_south$mpa_group),
  designation = unique(rov_north$designation))

## join in location info for all sites
locs <- rov_north %>% 
  dplyr::group_by(super_year, designation) %>%
  dplyr::summarise(
    lat = lat[1],
    lon = lon[1],
    depth_scaled = depth_scaled[1],
    depth_scaled_2 = depth_scaled_2[1],
    prop_soft_scaled = prop_soft_scaled[1])

grid <- dplyr::left_join(grid, locs) %>%
  dplyr::filter(!is.na(lat + lon))
grid$year <- grid$super_year

grid_north <- NULL
for (a in 1:40){
  grid_north <- rbind(grid_north, grid[grid$designation == "Reference", ])
}
for(a in 1:10){
  grid_north <- rbind(grid_north, grid[grid$designation == "MPA", ])
}


#===============================================================================
# Negative-Binomial GLM model selection
#===============================================================================

# South ========================================================================
data = rov_south

# Create data set to use in estimating the indices
covars <- c("super_year", "poly(depth_scaled, 2)", "designation", "prop_hard_scaled", 
  "prop_soft_scaled", "prop_mixed_scaled", "super_year:designation",
  "offset(log(usable_area))")

model.full <- MASS::glm.nb(as.formula(
  paste("n", 
        paste(0, "+", paste(covars, collapse = " + ")), 
        sep = " ~ ")),
  data = data,
  na.action = "na.fail")

model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(log(usable_area))", "super_year", "designation"))


#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- MuMIn::get.models(model.suite,subset = delta == 0)
best.formula <- best.model$ `8`$call$formula

save(model.suite, 
     file = file.path(dir, "nbglm_model_selection_south.rdata"))
save(Model_selection, file = file.path(dir, "model_formula_south.rdata"))

#format table for the document
out <- Model_selection[, -ncol(Model_selection)]
out[, 9:11] <- round(out[ , 9:11],1)
out[, 12] <- round(out[, 12], 2)
out[, c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] <- round(out[ ,c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] ,2)

out[is.na(out)] <- "N.A."
colnames(out) <- c("Designation", "Depth Polynomial", "Prop. Hard", "Prop. Mixed", "Prop. Soft", "Super Year", "Designation:Super_year",
  "offset-log(usable area)", "DF", "log-likelihood", "AICc", "Delta")
write.csv(out, file = file.path(dir, "forSS", "south_model_selection.csv"), row.names = FALSE)


# North ==========================================================================

data = rov_north

model.full <- MASS::glm.nb(as.formula(
  paste("n", 
        paste(0, "+", paste(covars, collapse = " + ")), 
        sep = " ~ ")),
  data = data,
  na.action = "na.fail")

model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(log(usable_area))", "super_year", "designation"))


#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- MuMIn::get.models(model.suite,subset = delta == 0)
best.formula <- best.model$ `8`$call$formula

save(model.suite, 
     file = file.path(dir, "nbglm_model_selection_north.rdata"))
save(Model_selection, file = file.path(dir, "model_formula_north.rdata"))

#format table for the document
out <- Model_selection[, -ncol(Model_selection)]
out[, 9:11] <- round(out[ , 9:11], 1)
out[, 12] <- round(out[, 12], 2)
out[, c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] <- round(out[ ,c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] ,2)

out[is.na(out)] <- "N.A."
colnames(out) <- c("Designation", "Depth Polynomial", "Prop. Hard", "Prop. Mixed", "Prop. Soft", "Super Year", "Designation:Super_year",
                   "offset-log(usable area)", "DF", "log-likelihood", "AICc", "Delta")
write.csv(out, file = file.path(dir, "forSS", "north_model_selection.csv"), row.names = FALSE)


#============================================================================
# South of Point Conception
# Run a single model that use interaction term for designation
# super year, designation, poly(depth, 2), prop_soft, year*designation
#============================================================================

# name <- "glm_negbin_south_designation_depth_soft_super_year"
# dir.create(file.path(dir, name), showWarnings = FALSE)
# 
# data <- rov_south
# data$year <- data$super_year
# data$log_usable_area <- log(data$usable_area)
# 
# south_model <- sdmTMB(
#   n ~ poly(depth_scaled, 2) + prop_soft_scaled + as.factor(year)*as.factor(designation), 
#   data = data,
#   offset = log(data$usable_area),
#   time = "year",
#   spatial="off",
#   spatiotemporal = "off",
#   family = nbinom2(link = "log")
# )
# 
# index <- calc_index(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   grid = grid_south)
# 
# do_diagnostics(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   plot_resids = FALSE)
# 


 name <- "delta_gamma_south_designation_depth_soft_73_27_05092023"
 dir.create(file.path(dir, name), showWarnings = FALSE)
 
 data <- rov_south
 data$year <- data$super_year
 data$log_usable_area <- log(data$usable_area)
 
 south_model <- sdmTMB(
   n ~ poly(depth_scaled, 2) + prop_soft_scaled + as.factor(year)*as.factor(designation), 
   data = data,
   offset = log(data$usable_area),
   time = "year",
   spatial="off",
   spatiotemporal = "off",
   family = delta_gamma()
 )
 
 index <- calc_index(
   dir = file.path(dir, name), 
   fit = south_model,
   grid = grid_south)
 
 do_diagnostics(
   dir = file.path(dir, name), 
   fit = south_model,
   plot_resids = TRUE)
 
 name <- "delta_lognormal_south_designation_depth_soft_73_27_05092023"
 dir.create(file.path(dir, name), showWarnings = FALSE)
 
 data <- rov_south
 data$year <- data$super_year
 data$log_usable_area <- log(data$usable_area)
 
 south_model <- sdmTMB(
   n ~ poly(depth_scaled, 2) + prop_soft_scaled + as.factor(year)*as.factor(designation), 
   data = data,
   offset = log(data$usable_area),
   time = "year",
   spatial="off",
   spatiotemporal = "off",
   family = delta_lognormal()
 )
 
 index <- calc_index(
   dir = file.path(dir, name), 
   fit = south_model,
   grid = grid_south)
 
 do_diagnostics(
   dir = file.path(dir, name), 
   fit = south_model,
   plot_resids = TRUE)

# alternative approach - add area
# total_area_km2 <- 200 # dummy number, units should be in whatever your area units are
# mpa_fraction <- 0.08
# grid$area <- c(rep(mpa_fraction,2), rep(1-mpa_fraction,2)) * total_area_km2
# pred <- predict(south_model, newdata = grid, return_tmb_object = TRUE)
# index <- get_index(pred, area = grid$area, bias_correct = TRUE)

# South STAN Model Run
# start.time <- Sys.time()
# # use STAN to see how well 'best model' fits the data
# Dnbin<- stan_glm.nb(n ~ as.factor(year) + as.factor(designation) + poly(depth_scaled,2) + prop_soft_scaled + as.factor(year):as.factor(designation),
#                     data = data,
#                     offset = log(data$usable_area),
#                     prior_intercept = normal(location = 0, scale = 10),
#                     prior = normal(location = 0, scale = 10),
#                     prior_aux = cauchy(0, 5),
#                     chains = 4,
#                     iter = 5000
# ) # iterations per chain
# Sys.time() - start.time
# save(Dnbin, file = file.pprath(dir, name, "rstan_glm_output.rdata"))
# 
# ## pp_check
# prop_zero <- function(y) mean(y == 0)
# 
# # figure of proportion zero - does good job
# figure_Dnbin_prop_zero <- rstanarm::pp_check(Dnbin, plotfun = "stat", stat = "prop_zero", binwidth = 0.01)
# pngfun(wd = file.path(dir, name), file = "proportion_zero_south.png")
# figure_Dnbin_prop_zero
# dev.off()
# 
# #plot of predictions with the mpa designation
# ptest4 <- ggeffects::ggpredict(mod2,c("year", "designation"))
# pngfun(wd = file.path(dir, name), file = "south_designation_predictions_from_nb_glm.png")
# plot(ptest4)
# dev.off()
# 
# #Now get the weighted index
# allcoefs <- as.matrix(Dnbin)
# allmodmat <- model.matrix(Dnbin)
# colnames(allcoefs)
# 
# # Order should be intercept, year, designation, year:designation terms
# coefind <- which(colnames(allcoefs) %in% c("(Intercept)", "as.factor(year)2020", "as.factor(designation)Reference", "as.factor(year)2020:as.factor(designation)Reference"))
# coefmat <- allcoefs[, coefind]
# modmat <- unique.matrix(allmodmat[, coefind])
# modmat <- modmat[c('1','56', '2','55'),]
# vals <- data[c(1, 56, 2, 55),c("year", "designation")]
# # vals (rows of modmat) need to be sorted by year, then designation, for weighting to work later
# 
# predmat <- modmat %*% t(coefmat)
# predmatbt <- exp(predmat)
# vals$pmed <- apply(predmatbt, 1, median)
# vals$pmedl <- apply(predmatbt, 1, quantile, 0.025)
# vals$pmedu <- apply(predmatbt, 1, quantile, 0.975)
# 
# # the values are not exactly the same as plot(ptest4), but the pattern is.
# # probably because ggpredict is conditioning on other vars e.g. mean depth
# ggplot2::ggplot(vals, aes(x = year,y = pmed, color = factor(designation))) +
#   geom_point(position = position_dodge(0.5)) +
#   geom_errorbar(aes(ymin = pmedl, ymax = pmedu),position = position_dodge(0.5), width=0.2) +
#   theme_bw() + ylab("Prediciton") + xlab("Year")
# ggsave(file = file.path(dir, name, "south_rstan_prediction.png"), width = 7, height = 7)
# 
# phi <- 0.92 #weighting to allocate to MPA
# # The following won't work if there are more than 2 sites
# nyears <- nrow(modmat) / 2
# wmat <- matrix(c(1 - phi, phi, rep(0, nyears*2)), byrow = T, nrow = nyears, ncol = nyears*2)
# # the warning is fine, it produces the right matrix
# 
# wpredmat <- wmat %*% predmatbt
# wpmed <- apply(wpredmat, 1, median)
# wpmedl <- apply(wpredmat, 1, quantile, 0.025)
# wpmedu <- apply(wpredmat, 1, quantile, 0.975)
# cvpred <- apply(wpredmat, 1, sd) / wpmed
# wres <- cbind.data.frame(year = unique(vals$year), wpmed, wpmedl, wpmedu, cvpred)
# 
# save(wres, file = file.path(dir, name, "south_rstan_weighted_index.rdata"))
# 
# ggplot(data = wres, aes(x=year, y=wpmed)) +
#   geom_point(position = position_dodge(0.5)) +
#   geom_errorbar(aes(ymin=wpmedl, ymax=wpmedu),position = position_dodge(0.5), width=0.2) +
#   theme_bw() + #ggtitle(paste("Predicted probibility, phi = ",phi)) +
#   xlab("Year") + ylab("Relative Index") +
#   ylim(c(0, 0.003))
# ggsave(file = file.path(dir, "plots", "south_weighted_stan_index.png"), width = 7, height = 7)


#=======================================================================
# North 
#=======================================================================

name <- "glm_negbin_north_designation_depth_80_20_05192023"
dir.create(file.path(dir, name), showWarnings = FALSE)

data <- rov_north
data$year <- data$super_year
data$log_usable_area <- log(data$usable_area)

north_model <- sdmTMB(
  n ~ poly(depth_scaled, 2) + as.factor(year)*as.factor(designation), 
  data = data,
  offset = log(data$usable_area),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(dir, name), 
  fit = north_model,
  grid = grid_north)

do_diagnostics(
  dir = file.path(dir, name), 
  fit = north_model,
  plot_resids = FALSE)

# start.time <- Sys.time()
# # use STAN to see how well 'best model' fits the data
# Dnbin<- stan_glm.nb(n ~ as.factor(year) + as.factor(designation) + poly(depth_scaled,2) + as.factor(year):as.factor(designation),
#                     data = data,
#                     offset = log(data$usable_area),
#                     prior_intercept = normal(location = 0, scale = 10),
#                     prior = normal(location = 0, scale = 10),
#                     prior_aux = cauchy(0, 5),
#                     chains = 4,
#                     iter = 5000
# ) # iterations per chain
# Sys.time() - start.time
# 
# ## pp_check
# prop_zero <- function(y) mean(y == 0)
# 
# # figure of proportion zero - does good job
# figure_Dnbin_prop_zero <- pp_check(Dnbin, plotfun = "stat", stat = "prop_zero", binwidth = 0.01)
# pngfun(wd = file.path(dir, name), file = "proportion_zero.png")
# figure_Dnbin_prop_zero
# dev.off()
# 
# #plot of predictions with the mpa designation
# ptest4 <- ggeffects::ggpredict(mod2,c("year", "designation"))
# pngfun(wd = file.path(dir, name), file = "north_predictions_from_nb_glm.png")
# plot(ptest4)
# dev.off()
# 
# #Now get the weighted index
# allcoefs <- as.matrix(Dnbin)
# allmodmat <- model.matrix(Dnbin)
# colnames(allcoefs)
# 
# # can probably automate generation of below, but order should be intercept, year, site, year:site terms
# coefind <- which(colnames(allcoefs) %in% c("(Intercept)", "as.factor(year)2020", "as.factor(designation)Reference", "as.factor(year)2020:as.factor(designation)Reference"))
# coefmat <-  allcoefs[, coefind]
# modmat <-  unique.matrix(allmodmat[, coefind])
# modmat <-  modmat[c('3','24','1','23'),]
# vals <-  data[c(3, 24, 1, 23),c("year", "designation")]
# #vals (rows of modmat) need to be sorted by year, then site, for weighting to work later
# 
# predmat <-  modmat %*%t (coefmat)
# predmatbt <-  exp(predmat)
# vals$pmed <-  apply(predmatbt, 1, median)
# vals$pmedl <-  apply(predmatbt, 1, quantile, 0.025)
# vals$pmedu <-  apply(predmatbt, 1, quantile, 0.975)
# 
# #the values are not exactly the same as plot(ptest4), but the pattern is.
# #probably because ggpredict is conditioning on other vars e.g. mean depth
# ggplot2::ggplot(vals, aes(x = year,y = pmed, color = factor(designation))) +
#   geom_point(position = position_dodge(0.5)) +
#   geom_errorbar(aes(ymin = pmedl, ymax = pmedu),position = position_dodge(0.5), width=0.2) +
#   theme_bw() + ylab("Prediciton") + xlab("Year") + ylim(c(0, 0.0025))
# ggsave(file = file.path(dir, name, "north_rstan_prediction.png"), width = 7, height = 7)
# 
# phi <-  0.80 #weighting to allocate to MPA
# #the following won't work if there are more than 2 sites
# nyears <-  nrow(modmat) / 2
# wmat <-  matrix(c(1-phi, phi, rep(0, nyears*2)), byrow = T, nrow = nyears, ncol = nyears*2)
# #the warning is fine, it produces the right matrix
# 
# wpredmat <-  wmat %*% predmatbt
# wpmed <-  apply(wpredmat, 1, median)
# wpmedl <- apply(wpredmat, 1, quantile, 0.025)
# wpmedu <- apply(wpredmat, 1, quantile, 0.975)
# wpmedcv<- apply(wpredmat, 1, sd) / wpmed
# wres <-  cbind.data.frame(year = unique(vals$year), wpmed, wpmedl, wpmedu, wpmedcv)
# 
# save(wres, file = file.path(dir, name, "north_rstan_weighted_index.rdata"))
# 
# ggplot(data = wres, aes(x=year, y=wpmed)) +
#   geom_point(position = position_dodge(0.5)) +
#   geom_errorbar(aes(ymin=wpmedl, ymax=wpmedu),position = position_dodge(0.5), width=0.2) +
#   theme_bw() + #ggtitle(paste("Predicted probibility, phi = ",phi)) +
#   xlab("Year") + ylab("Relative Index") + ylim(c(0, 0.0015))
# ggsave(file = file.path(dir, name, "north_weighted_stan_index.png"), width = 7, height = 7)



# name <- "delta_lognormal_north_designation_depth"
# dir.create(file.path(dir, name), showWarnings = FALSE)
# 
# data <- rov_north
# data$year <- data$super_year
# data$log_usable_area <- log(data$usable_area)
# 
# north_model <- sdmTMB(
#   n ~ poly(depth_scaled, 2) +  as.factor(year)*as.factor(designation), 
#   data = data,
#   offset = log(data$usable_area),
#   time = "year",
#   spatial="off",
#   spatiotemporal = "off",
#   family = delta_lognormal()
# )
# 
# index <- calc_index(
#   dir = file.path(dir, name), 
#   fit = north_model,
#   grid = grid_north)
# 
# do_diagnostics(
#   dir = file.path(dir, name), 
#   fit = north_model,
#   plot_resids = FALSE)
# 
# # north STAN Model Run
# mod2 <- MASS::glm.nb(n ~ as.factor(year) + poly(depth_scaled,2) +
#                      as.factor(designation) + as.factor(year):as.factor(designation) +
#                      offset(log_usable_area),
#                      data = data)
# 
#=================================================================================
# Analyze the data on a yearly basis
#
# Create Predictions Grids for Each Area
#==================================================================================


#===============================================================================
# Negative-Binomial GLM model selection
#===============================================================================

# South ========================================================================
data = rov_south

# Create data set to use in estimating the indices
covars <- c("year", "poly(depth_scaled, 2)", "designation", "prop_hard_scaled", 
            "prop_soft_scaled", "prop_mixed_scaled", "year:designation",
            "offset(log(usable_area))")

model.full <- MASS::loglm(as.formula(
  paste("n", 
        paste(0, "+", paste(covars, collapse = " + ")), 
        sep = " ~ ")),
  data = data,
  na.action = "na.fail")

model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(log(usable_area))", "year", "designation"))


#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- MuMIn::get.models(model.suite,subset = delta == 0)
best.formula <- best.model$ `8`$call$formula

save(model.suite, 
     file = file.path(dir, "nbglm_model_selection_south.rdata"))
save(Model_selection, file = file.path(dir, "model_formula_south.rdata"))

#format table for the document
out <- Model_selection[, -ncol(Model_selection)]
out[, 9:11] <- round(out[ , 9:11],1)
out[, 12] <- round(out[, 12], 2)
out[, c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] <- round(out[ ,c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] ,2)

out[is.na(out)] <- "N.A."
colnames(out) <- c("Designation", "Depth Polynomial", "Prop. Hard", "Prop. Mixed", "Prop. Soft", "Super Year", "Designation:Super_year",
                   "offset-log(usable area)", "DF", "log-likelihood", "AICc", "Delta")
write.csv(out, file = file.path(dir, "forSS", "south_model_selection.csv"), row.names = FALSE)

# Delta log-normal model selection ===============================================================
data_pos = rov_south[rov_south$n > 0, ]
data_present = rov_south
data_present$pa <- 0
data_present$pa[data_present$n > 0] <- 1

b1 <- as.formula(paste("pa", paste(covars, collapse=" + "), sep=" ~ "))
b2 <- as.formula(paste("pa", paste(covars[c(1, 2, 3, 5, 7, 8)], collapse=" + "), sep=" ~ "))
b3 <- as.formula(paste("pa", paste(covars[c(1, 3, 5, 7, 8)], collapse=" + "), sep=" ~ "))
b4 <- as.formula(paste("pa", paste(covars[c(1, 2, 7, 8)], collapse=" + "), sep=" ~ "))

bin.mod1 <- glm(b1, data=data_present,family=binomial)
summary(bin.mod1)
anova(bin.mod1, test="Chi")
bin.mod2 <- glm(b2, data=data_present,family=binomial)
summary(bin.mod2)
anova(bin.mod2, test="Chi")
bin.mod3 <- glm(b3, data=data_present,family=binomial)
summary(bin.mod3)
anova(bin.mod3, test="Chi")
bin.mod4 <- glm(b4, data=data_present,family=binomial)
summary(bin.mod4)
anova(bin.mod4, test="Chi")
binAIC <- AIC(bin.mod1, bin.mod2, bin.mod3, bin.mod4)

logn.full <- tryCatch(glm(as.formula(paste("n", 
                                           paste(covars[c(1:length(covars))], collapse=" + "), sep=" ~ ")), 
                          data = data_pos, 
                          family = gaussian),
                      error=function(e) NA)

summary(logn.full)
anova(logn.full, test = "Chisq")

gamma.full <- tryCatch(glm(as.formula(paste("n", 
                                            paste(covars[c(1:length(covars))], collapse=" + "), sep=" ~ ")), 
                           data = data_pos, 
                           family = Gamma(link ="log")),
                       error=function(e) NA)
summary(gamma.full)
anova(gamma.full, test = "Chisq")

logn_or_gamma_aic <- c(logn.full$aic, gamma.full$aic)
logn_or_gamma_aic
pos.mod.dist <- ifelse(logn_or_gamma_aic[1]<logn_or_gamma_aic[2],
                       "Lognormal", "Gamma")

g1 <- as.formula(paste("n", paste(covars, collapse=" + "), sep=" ~ "))
g2 <- as.formula(paste("n", paste(covars[c(1, 2, 3, 5, 7, 8)], collapse=" + "), sep=" ~ "))
g3 <- as.formula(paste("n", paste(covars[c(1, 3, 5, 7, 8)], collapse=" + "), sep=" ~ "))
g4 <- as.formula(paste("n", paste(covars[c(1, 2, 7, 8)], collapse=" + "), sep=" ~ "))

gamma.mod1 <- glm(g1, data = data_pos, family = Gamma(link ="log"))
anova(gamma.mod1, test = "Chisq")
gamma.mod2 <- glm(g2, data = data_pos, family = Gamma(link ="log"))
anova(gamma.mod2, test <- "Chisq")
gamma.mod3 <- glm(g3, data = data_pos, family = Gamma(link ="log"))
anova(gamma.mod3, test = "Chisq")
gamma.mod4 <- glm(g4, data = data_pos, family = Gamma(link ="log"))
summary(gamma.mod4)
anova(gamma.mod4, test = "Chisq")
posAIC <- AIC(gamma.mod1, gamma.mod2, gamma.mod3, gamma.mod4)

Models <- as.data.frame(rbind(b1, b2, b3, b4))
Model_selection <- as.data.frame(cbind(Models$V3,
                                       round(binAIC$AIC,4),
                                       round(posAIC$AIC,4)))
colnames(Model_selection) <- c('Model', 'Binomial AIC', 'Gamma AIC')


# North ==========================================================================

data = rov_north

model.full <- MASS::glm.nb(as.formula(
  paste("n", 
        paste(0, "+", paste(covars, collapse = " + ")), 
        sep = " ~ ")),
  data = data,
  na.action = "na.fail")

model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(log(usable_area))", "super_year", "designation"))


#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- MuMIn::get.models(model.suite,subset = delta == 0)
best.formula <- best.model$ `8`$call$formula

save(model.suite, 
     file = file.path(dir, "nbglm_model_selection_north.rdata"))
save(Model_selection, file = file.path(dir, "model_formula_north.rdata"))

#format table for the document
out <- Model_selection[, -ncol(Model_selection)]
out[, 9:11] <- round(out[ , 9:11], 1)
out[, 12] <- round(out[, 12], 2)
out[, c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] <- round(out[ ,c('prop_hard_scaled', 'prop_mixed_scaled', 'prop_soft_scaled')] ,2)

out[is.na(out)] <- "N.A."
colnames(out) <- c("Designation", "Depth Polynomial", "Prop. Hard", "Prop. Mixed", "Prop. Soft", "Super Year", "Designation:Super_year",
                   "offset-log(usable area)", "DF", "log-likelihood", "AICc", "Delta")
write.csv(out, file = file.path(dir, "forSS", "north_model_selection.csv"), row.names = FALSE)

#====================================================================================================
# Create the grid
# Year and Sites
#====================================================================================================
grid <- expand.grid(
  year = unique(rov_south$year),
  #site = unique(rov_south$mpa_group),
  designation = unique(rov_south$designation))

## join in location info for all sites
locs <- rov_south %>% 
  dplyr::group_by(year, designation) %>%
  dplyr::summarise(
    lat = lat[1],
    lon = lon[1],
    depth_scaled = depth_scaled[1],
    depth_scaled_2 = depth_scaled_2[1],
    prop_soft_scaled = prop_soft_scaled[1])

grid <- dplyr::left_join(grid, locs) %>%
  dplyr::filter(!is.na(lat + lon))

grid$mpa_group_year <- 1

n_open <- round(0.73 * 100, 0)
n_mpa  <- round(0.27 * 100, 0)

grid_south <- NULL
for (a in 1:n_open){
  grid_south <- rbind(grid_south, grid[grid$designation == "Reference", ])
}
for(a in 1:n_mpa){
  grid_south <- rbind(grid_south, grid[grid$designation == "MPA", ])
}


# North grid ===========================

grid <- expand.grid(
  super_year = unique(rov_north$year),
  #site = unique(rov_south$mpa_group),
  designation = unique(rov_north$designation))

## join in location info for all sites
locs <- rov_north %>% 
  dplyr::group_by(year, designation) %>%
  dplyr::summarise(
    lat = lat[1],
    lon = lon[1],
    depth_scaled = depth_scaled[1],
    depth_scaled_2 = depth_scaled_2[1],
    prop_soft_scaled = prop_soft_scaled[1])

grid <- dplyr::left_join(grid, locs) %>%
  dplyr::filter(!is.na(lat + lon))
grid$mpa_group_year <- 1

grid_north <- NULL
for (a in 1:8){
  grid_north <- rbind(grid_north, grid[grid$designation == "Reference", ])
}
for(a in 1:2){
  grid_north <- rbind(grid_north, grid[grid$designation == "MPA", ])
}

#=================================================================================
# South Model - Neg. Binomial Model
#==================================================================================

# name <- "glm_negbin_south_designation_year"
# dir.create(file.path(dir, name), showWarnings = FALSE)
# 
# data <- rov_south
# data$mpa_group_year <- as.factor(paste0(data$mpa_group, "_", data$year))
# data$mpa_group_year <- as.factor(as.numeric(data$mpa_group_year))
# 
# south_model <- sdmTMB(
#   n ~ 0 + as.factor(year) + as.factor(year)*as.factor(designation), # + (1|mpa_group_year), 
#   data = data,
#   offset = log(data$usable_area),
#   time = "year",
#   spatial="off",
#   spatiotemporal = "off",
#   family = nbinom2(link = "log"),
#   control = sdmTMBcontrol(newton_loops = 1)
# )

# index <- calc_index(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   grid = grid_south)
# 
# do_diagnostics(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   plot_resids = FALSE)
# 
# name <- "glm_negbin_south_designation_depth_year_soft"
# dir.create(file.path(dir, name), showWarnings = FALSE)

# data <- rov_south
# data$mpa_group_year <- as.factor(paste0(data$mpa_group, "_", data$year))
# data$mpa_group_year <- as.factor(as.numeric(data$mpa_group_year))
# 
# south_model <- sdmTMB(
#   n ~ as.factor(year) + poly(depth_scaled, 2) + prop_soft_scaled +  as.factor(year)*as.factor(designation) + (1|mpa_group_year), 
#   data = data,
#   offset = log(data$usable_area),
#   time = "year",
#   spatial="off",
#   spatiotemporal = "off",
#   family = nbinom2(link = "log")
# )
# 
# index <- calc_index(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   grid = grid_south)
# 
# do_diagnostics(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   plot_resids = FALSE)
# 
# 
# # Check the proportion zero for the negative binomial model
# start.time <- Sys.time()
# # use STAN to see how well 'best model' fits the data
# Dnbin<- stan_glmer.nb(n ~ as.factor(year) + poly(depth_scaled, 2) + prop_soft_scaled +  as.factor(year)*as.factor(designation) + (1|mpa_group_year),
#                   data = data,
#                   offset = log(data$usable_area),
#                   prior_intercept = normal(location = 0, scale = 10),
#                   prior = normal(location = 0, scale = 10),
#                   prior_aux = cauchy(0, 5),
#                   chains = 4,
#                   iter = 5000
# ) # iterations per chain
# Sys.time() - start.time
# 
# ## pp_check
# prop_zero <- function(y) mean(y == 0)
# 
# # figure of proportion zero - does good job
# figure_Dnbin_prop_zero <- pp_check(Dnbin, plotfun = "stat", stat = "prop_zero", binwidth = 0.01)
# pngfun(wd = file.path(dir, name), file = "proportion_zero.png")
# figure_Dnbin_prop_zero
# dev.off()

#=================================================================================
# South Model - Delta Lognormal
#==================================================================================

# name <- "delta_lognormal_south_designation_depth_year_soft_73_27"
# dir.create(file.path(dir, name), showWarnings = FALSE)
# 
# data <- rov_south
# data$mpa_group_year <- as.factor(paste0(data$mpa_group, "_", data$year))
# data$mpa_group_year <- as.factor(as.numeric(data$mpa_group_year))
# 
# south_model <- sdmTMB(
#   n ~ as.factor(year) + poly(depth_scaled, 2) + prop_soft_scaled +  as.factor(year)*as.factor(designation) + (1|mpa_group_year), 
#   data = data,
#   offset = log(data$usable_area),
#   time = "year",
#   spatial="off",
#   spatiotemporal = "off",
#   family = delta_lognormal()
# )
# 
# index <- calc_index(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   grid = grid_south)
# 
# do_diagnostics(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   plot_resids = FALSE)
# 
# 
# name <- "delta_lognormal_south_designation_depth_year_soft_no_re_73_27"
# dir.create(file.path(dir, name), showWarnings = FALSE)
# 
# data <- rov_south
# 
# south_model <- sdmTMB(
#   n ~ as.factor(year) + poly(depth_scaled, 2) + prop_soft_scaled +  as.factor(year)*as.factor(designation), 
#   data = data,
#   offset = log(data$usable_area),
#   time = "year",
#   spatial="off",
#   spatiotemporal = "off",
#   family = delta_lognormal()
# )
# 
# index <- calc_index(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   grid = grid_south)
# 
# do_diagnostics(
#   dir = file.path(dir, name), 
#   fit = south_model,
#   plot_resids = FALSE)
# 
 name <- "delta_gamma_south_designation_depth_year_soft_73_27_05192023"
 dir.create(file.path(dir, name), showWarnings = FALSE)
 
 data <- rov_south
 data$mpa_group_year <- as.factor(paste0(data$mpa_group, "_", data$year))
 data$mpa_group_year <- as.factor(as.numeric(data$mpa_group_year))
 
 south_model <- sdmTMB(
   n ~ as.factor(year) + poly(depth_scaled, 2) + prop_soft_scaled +  as.factor(year)*as.factor(designation) + (1|mpa_group_year), 
   data = data,
   offset = log(data$usable_area),
   time = "year",
   spatial="off",
   spatiotemporal = "off",
   family = delta_gamma()
 )
 
 index <- calc_index(
   dir = file.path(dir, name), 
   fit = south_model,
   grid = grid_south)
 
 do_diagnostics(
   dir = file.path(dir, name), 
   fit = south_model,
   plot_resids = TRUE)

name <- "delta_gamma_south_designation_depth_year_73_27_05192023"
 dir.create(file.path(dir, name), showWarnings = FALSE)
 
 data <- rov_south
 data$mpa_group_year <- as.factor(paste0(data$mpa_group, "_", data$year))
 data$mpa_group_year <- as.factor(as.numeric(data$mpa_group_year))
 
 south_model <- sdmTMB(
   n ~ as.factor(year) + poly(depth_scaled, 2) +  as.factor(year)*as.factor(designation) + (1|mpa_group_year), 
   data = data,
   offset = log(data$usable_area),
   time = "year",
   spatial="off",
   spatiotemporal = "off",
   family = delta_gamma()
 )
 
 index <- calc_index(
   dir = file.path(dir, name), 
   fit = south_model,
   grid = grid_south)
 
 do_diagnostics(
   dir = file.path(dir, name), 
   fit = south_model,
   plot_resids = TRUE)

#=================================================================================
# North Model
#==================================================================================

name <- "delta_lognormal_north_designation_depth_year"
dir.create(file.path(dir, name), showWarnings = FALSE)

data <- rov_north
data$mpa_group_year <- as.factor(paste0(data$mpa_group, "_", data$year))
data$mpa_group_year <- as.factor(as.numeric(data$mpa_group_year))
data$log_usable_area <- log(data$usable_area)

north_model <- sdmTMB(
  n ~ as.factor(year) + poly(depth_scaled, 2) +  as.factor(year)*as.factor(designation) + (1|mpa_group_year), 
  data = data,
  offset = log(data$usable_area),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 1)
)


index <- calc_index(
  dir = file.path(dir, name), 
  fit = north_model,
  grid = grid_north)

do_diagnostics(
  dir = file.path(dir, name), 
  fit = north_model,
  plot_resids = FALSE)

# Example test
# mod2 <- lme4::glmer.nb(n ~ as.factor(year) + poly(depth_scaled,2) + prop_soft_scaled + 
#                         as.factor(designation) + as.factor(year):as.factor(designation) +
#                         (1|mpa_group_year) +
#                         offset(log_usable_area),
#                       data = data)