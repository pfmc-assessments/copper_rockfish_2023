



library(dplyr)
library(sdmTMB)
library(sf)
library(sp)
library(tidyr)

species <- "Copper Rockfish"
dir <- "C:/Assessments/2023/copper_rockfish_2023/data/nwfsc_hkl"

d <- read.csv(file.path(dir, "hookandline_2004_2021_draft_data.csv"))
d$ave_lat = d$ave_long = NA 
for (aa in unique(d$site_number)) {
  find = which(d$site_number == aa) 
  d$ave_long[find] = -1 * mean(d[find, "drop_longitude_degrees"])
  d$ave_lat[find] = mean(d[find, "drop_latitude_degrees"])
}
#d <- sdmTMB::add_utm_columns(d, c("ave_long", "ave_lat")) # UTM 10
#d = cbind(d, project_coordinates(X = d$ave_long, Y = d$ave_lat))
d_trans = d
coordinates(d_trans) <- c("ave_long", "ave_lat")
proj4string(d_trans) <- CRS("+proj=longlat +datum=WGS84")
newproj <- paste("+proj=utm +zone=10 ellps=WGS84 +datum=WGS84")
d_trans <- spTransform(d_trans, CRS(newproj))
d_trans <- as.data.frame(d_trans)
d$X <- d_trans$ave_long / 1000 # convert to km
d$Y <- d_trans$ave_lat / 1000 # convert to km

git_dir <- "C:/Users/Chantel.Wetzel/Documents/GitHub/hook-line"
source(file.path(git_dir, "R", "format_hkl_data.R"))
source(file.path(git_dir, "R", "match.f.R"))
source(file.path(git_dir, "R", "refactor.R"))
source(file.path(git_dir, "R", "plot_betas.R"))
source(file.path(git_dir, "R", "plot_index.R"))

wd <- file.path(dir, "index")

species_data <- format_hkl_data(
  common_name = species, 
  data = d)

# Create data set to use for negative-binomial
# Does not include wave height, moon phase, or angler location on the vessel(angler scaled)
subdata <- species_data %>%
  group_by(common_name, year, site_number, crew_scaled, 
    angler_scaled, drop_scaled, hook_scaled) %>% 
  summarise(n = sum(number_caught),
            weight = sum(weight_kg, na.rm = TRUE),
            X = X[1], 
            Y = Y[1])

if (length(is.na(subdata$crew_scaled)) > 0) {
  subdata[is.na(subdata$crew_scaled),'crew_scaled'] = 999 } 
subdata <- as.data.frame(subdata)
subdata$crew_scaled <- as.factor(subdata$crew_scaled)
subdata$effort <- subdata$drop_scaled * subdata$hook_scaled * subdata$angler_scaled

# Create the grid
# Year and Sites
year_site <- expand.grid(year = unique(subdata$year),
  site_number = unique(subdata$site_number))

## join in location info for all sites
locs <- dplyr::group_by(subdata, site_number) %>%
  dplyr::summarise(
    Y = Y[1],
    X = X[1])

grid <- dplyr::left_join(year_site, locs) %>%
  dplyr::filter(!is.na(X + Y))
grid$hook_scaled <- 1
grid$drop_scaled <- 1
grid$crew_scaled <- 0
grid$angler_scaled <- 1
# Effort is set equal to 75 because that is the total
# number of hooks * drops per site
grid$effort <- 75 

#make_grid = mkpredgrid2d(pnts.x = grid$X, pnts.y = grid$Y, par.x = 75)
#uniform_grid = make_grid$xygrid

# Fit negative binomial model to these data
mesh <- make_mesh(subdata, xy_cols = c("X", "Y"), cutoff = 10)

indices <- metrics <- run_time <- NULL

save(subdata, mesh, grid,
  file = file.path(wd, "data_mesh.Rdata"))

# Negative-Binomial GLM with only main effects: year, site ###########################################################
tictoc::tic()

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) ,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  mesh = mesh,
  family = nbinom2(link = "log"),
  silent = TRUE,
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),
  index_args = list(area = 1),
  control = sdmTMBcontrol(newton_loops = 1)
)

name <- "glm_negbin_main_year_site"
index <- get_index(fit, bias_correct = TRUE)
index$model <- name
indices <- rbind(indices, index)
save(indices, file = file.path(wd, "all_indices.Rdata"))
time = tictoc::toc()
run_time  = cbind(run_time, time$toc - time$tic)

loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

plot_indices(data = index, main_name = species, 
  type = name, save_loc = wd, ymax = NULL)

save(fit, index, file = file.path(wd, paste0(name, ".Rdata")))

# Negative-Binomial GLM with only main effects: year, site, hook ###########################################################
tictoc::tic()

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(hook_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  mesh = mesh,
  family = nbinom2(link = "log"),
  silent = TRUE,
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),
  index_args = list(area = 1),
  control = sdmTMBcontrol(newton_loops = 1)
)

name <- "glm_negbin_main_year_site_hook"
index <- get_index(fit, bias_correct = TRUE)
index$model <- name
indices <- rbind(indices, index)
save(indices, file = file.path(wd, "all_indices.Rdata"))
time = tictoc::toc()
run_time  = cbind(run_time, time$toc - time$tic)

loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

plot_indices(data = index, main_name = species, 
  type = name, save_loc = wd, ymax = NULL)

save(fit, index, file = file.path(wd, paste0(name, ".Rdata")))


# Negative-Binomial GLM with only main effects: year, site, hook, drop ###########################################################
tictoc::tic()

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(hook_scaled) + as.factor(drop_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  mesh = mesh,
  family = nbinom2(link = "log"),
  silent = TRUE,
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),
  index_args = list(area = 1),
  control = sdmTMBcontrol(newton_loops = 1)
)

name <- "glm_negbin_main_year_site_hook_drop"
index <- get_index(fit, bias_correct = TRUE)
index$model <- name
indices <- rbind(indices, index)
save(indices, file = file.path(wd, "all_indices.Rdata"))
time = tictoc::toc()
run_time  = cbind(run_time, time$toc - time$tic)

loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

plot_indices(data = index, main_name = species, 
  type = name, save_loc = wd, ymax = NULL)

save(fit, index, file = file.path(wd, paste0(name, ".Rdata")))

# Negative-Binomial GLM with Random Effects ###########################################################
rm(fit, index)

tictoc::tic()
fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(hook_scaled) + as.factor(drop_scaled) + (1|crew_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  mesh = mesh,
  family = nbinom2(link = "log"),
  silent = TRUE,
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),
  index_args = list(area = 1),
  control = sdmTMBcontrol(newton_loops = 1)
)

name <- "glm_negbin_main_year_site_hook_drop_and_re"
index <- get_index(fit, bias_correct = TRUE)
index$model <- name
indices <- rbind(indices, index)
save(indices, file = file.path(wd, "all_indices.Rdata"))
time = tictoc::toc()
run_time  = cbind(run_time, time$toc - time$tic)

loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

plot_indices(data = index, main_name = species, 
  type = name, save_loc = wd, ymax = NULL)

save(fit, index, file = file.path(wd, paste0(name, ".Rdata")))

# Delta GlM with RE ###########################################################
rm(fit, index)

tictoc::tic()
fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(hook_scaled) + as.factor(drop_scaled) + (1|crew_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  mesh = mesh,
  family = delta_truncated_nbinom2(),
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),
  index_args = list(area = 1),
  control = sdmTMBcontrol(newton_loops = 1)
)

name <- "delta_negbin_main_year_site_hook_drop_and_re"
index <- get_index(fit, bias_correct = TRUE)
index$model = name
indices <- rbind(indices, cbind(index))
time = tictoc::toc()
run_time  = cbind(run_time, time$toc - time$tic)

loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

plot_indices(data = index, main_name = species, 
  type = name, save_loc = wd, ymax = NULL)

save(fit, index, file = file.path(wd, paste0(name, ".Rdata")))
save(indices, file = file.path(wd, "all_indices.Rdata"))  
save(metrics, file = file.path(wd, "metrics.Rdata"))
save(run_time, file = file.path(wd, "run_time.Rdata"))


