############################################################################################
#   Estimate index for the NWFSC hkl survey
#           using sdmTMB
#          November, 2022
#           Chantel Wetzel
############################################################################################

library(here)
library(dplyr)
library(sdmTMB)
library(sf)
library(sp)
library(tidyr)
library(tmbstan)

data_dir <- file.path(here(), "data", "nwfsc_hkl")
index_dir <- file.path(here(), "data", "survey_indices", "nwfsc_hkl")

# Load in some helper functions for processing and plotting the data
all <- list.files(file.path(here(), "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(here(), "R", "sdmTMB", all[a]))}

species <- "Copper Rockfish"
d <- read.csv(file.path(data_dir, "H&LSurveyDataThru2022_DWarehouse version_03042023.csv"))

# d$ave_lat <- d$ave_long <- NA 
# for (aa in unique(d$site_number)) {
#   find <-  which(d$site_number == aa) 
#   d$ave_long[find] <-  -1 * mean(d[find, "drop_longitude_degrees"])
#   d$ave_lat[find] <-  mean(d[find, "drop_latitude_degrees"])
# }

# Convert the latitude and longitude to WGS projections  
# d_trans <- d
# coordinates(d_trans) <- c("ave_long", "ave_lat")
# #proj4string(d_trans) <- CRS("+proj=longlat +datum=WGS84")
# newproj <- paste("+proj=utm +zone=10 ellps=WGS84 +datum=WGS84")
# d_trans <- spTransform(d_trans, CRS(newproj))
# d_trans <- as.data.frame(d_trans)
# d$X <- d_trans$ave_long / 1000 # convert to km
# d$Y <- d_trans$ave_lat / 1000 # convert to km

# Pull out data for copper rockfish
species_data <- format_hkl_data(
  common_name = species, 
  data = d)

# Does not include wave height, moon phase, or angler location on the vessel (angler scaled)
subdata <- species_data %>%
  group_by(common_name, year, site_number, drop_scaled, cca) %>% 
  summarise(n = sum(number_caught),
            swell = median(swell_height_m),
            depth = median(drop_depth_meters),
            wave = median(wave_height_m),
            vermillion = sum(vermilion),
            bocaccio = sum(bocaccio),
            lat = mean(drop_latitude_degrees),
            lon = mean(drop_longitude_degrees)) 

#if (length(is.na(subdata$crew_scaled)) > 0) {
#  subdata[is.na(subdata$crew_scaled),'crew_scaled'] = 999 } 
subdata <- as.data.frame(subdata)
subdata$effort <- 15 # (3 lines * 5 hooks) subdata$drop_scaled * subdata$hook_scaled * subdata$angler_scaled
#subdata$crew_scaled <- as.factor(subdata$crew_scaled)
subdata$drop_scaled <- as.factor(subdata$drop_scaled)
#subdata$hook_scaled <- as.factor(subdata$hook_scaled)
#subdata$swell <- as.factor(subdata$swell)
#subdata$wave <- as.factor(subdata$wave)
#subdata$vermillion <- as.factor(subdata$vermillion)
#subdata$bocaccio <- as.factor(subdata$bocaccio)

save(subdata, grid, #mesh,
  file = file.path(index_dir, "data_grid.Rdata"))

indices <- metrics <- NULL

#===============================================================================
# Negative-Binomial GLM model selection
#===============================================================================

# Create data set to use in estimating the indices
covars <- c("year", "site_number", "drop_scaled", "swell", "bocaccio", "vermillion", "offset(log(effort))")

model.full <- MASS::glm.nb(as.formula(
  paste("n", 
        paste(0, "+", paste(covars, collapse = " + ")), 
        sep = " ~ ")),
  data = subdata,
  na.action = "na.fail")

model.suite <- MuMIn::dredge(model.full,
                      rank = "AICc", 
                      fixed= c("offset(log(effort))", "year"))


#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- MuMIn::get.models(model.suite,subset = delta == 0)
best.formula <- best.model$ `8`$call$formula

save(model.suite, 
     file = file.path(index_dir, "nbglm_model_selection_drop_leve.rdata"))
save(Model_selection, file = file.path(index_dir, "model_formula_drop_level.rdata"))

#format table for the document

#===============================================================================
# Negative-Binomial GLM with only main effects: year, site, swell, vermillion
#===============================================================================

name <- "glm_negbin_main_year_site_swell_vermilion"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) +  swell + vermillion,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  data = fit)

do_diagnostics(
  dir = file.path(index_dir, name), 
  data = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#===============================================================================
# Negative-Binomial GLM with only main effects: year, site, swell
#===============================================================================

name <- "glm_negbin_main_year_site_swell"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) +  swell,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  data = fit)

do_diagnostics(
  dir = file.path(index_dir, name), 
  data = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))


#===============================================================================
# Negative-Binomial GLM with only main effects: year, site, drop, swell
#===============================================================================

name <- "glm_negbin_main_year_site_drop_swell"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(drop_scaled) + swell,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  data = fit)

do_diagnostics(
  dir = file.path(index_dir, name), 
  data = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#===============================================================================
# Negative-Binomial GLM with only main effects: year, site, drop
#===============================================================================

name <- "glm_negbin_main_year_site_drop"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(drop_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  data = fit)

do_diagnostics(
  dir = file.path(index_dir, name), 
  data = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))


#===============================================================================
# Negative-Binomial GLM with only main effects: year, site, hook, drop 
#===============================================================================

name <- "glm_negbin_main_year_site_hook_drop_grid"
dir.create(file.path(index_dir, name), showWarnings = FALSE)

# Create the grid
# Year and Sites
year_site <- expand.grid(
  year = unique(subdata$year),
  site_number = unique(subdata$site_number))

## join in location info for all sites
 locs <- dplyr::group_by(subdata, site_number) %>%
   dplyr::summarise(
     lat = lat[1],
     lon = lon[1],
     Y = Y[1],
     X = X[1])
 
grid <- dplyr::left_join(year_site, locs) %>%
   dplyr::filter(!is.na(X + Y))
grid$hook_scaled <- 1
grid$drop_scaled <- 1
grid$crew_scaled <- 0
grid$angler_scaled <- 1
grid$swell_height_m <- 0
# Effort is set equal to 75 because that is the total
# # number of hooks * drops per site
grid$effort <- 75 

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(hook_scaled) + as.factor(drop_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  data = fit)

do_diagnostics(
  dir = file.path(index_dir, name), 
  data = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))


#=========================================================
# Delta Model with main effects 
#=========================================================

name <- "delta_gamma_main_year_site_hook_drop"
dir.create(file.path(index_dir, name))
rm(fit, index)

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(hook_scaled) + as.factor(drop_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_gamma()
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  data = fit)

do_diagnostics(
  dir = file.path(index_dir, name), 
  data = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#=========================================================
# Delta GlM with RE lognormal
#=========================================================

name <- "delta_lognormal_main_year_site_hook_drop"
dir.create(file.path(index_dir, name), showWarnings = FALSE)
rm(fit, index)

fit <- sdmTMB(
  n ~ 0 + as.factor(year) + as.factor(site_number) + as.factor(hook_scaled) + as.factor(drop_scaled),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal()
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  data = fit)

do_diagnostics(
  dir = file.path(index_dir, name), 
  data = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))


#===============================================================================
# Bayesian 
#===============================================================================

library(rstan)

fit_stan <- tmbstan::tmbstan(
  fit$tmb_obj,
  iter = 1000, 
  chains = 2,
  control = list(adapt_delta = 0.9, max_treedepth = 12),
  thin = 10, 
  warmup = 500, #default floor(iter/2)
  seed = 8217 # ensures repeatability
)

# Warning messages:
#   1: There were 1000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 2: Examine the pairs() plot to diagnose sampling problems
# 
# # 3: The largest R-hat is NA, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 

plot(fit_stan)

pars_plot <- c("b_j[1]")

bayesplot::mcmc_trace(fit_stan, pars = pars_plot)
bayesplot::mcmc_pairs(fit_stan, pars = pars_plot)






