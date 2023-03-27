############################################################################################
#   Estimate index for the NWFSC hkl survey
#           using sdmTMB
#          November, 2022
#           Chantel Wetzel
############################################################################################

library(here)
library(dplyr)
library(ggplot2)
library(sdmTMB)
library(sf)
library(sp)
library(tidyr)
library(tmbstan)
library(rstan) # for plot() method
options(mc.cores = parallel::detectCores())

data_dir <- file.path(here(), "data", "nwfsc_hkl")
index_dir <- file.path(here(), "data", "survey_indices", "nwfsc_hkl")

# Load in some helper functions for processing and plotting the data
all <- list.files(file.path(here(), "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(here(), "R", "sdmTMB", all[a]))}

species <- "Copper Rockfish"
d <- read.csv(file.path(data_dir, "H&LSurveyDataThru2022_DWarehouse version_03042023.csv"))

d$ave_lat <- d$ave_long <- NA 
for (aa in unique(d$site_number)) {
  find <-  which(d$site_number == aa) 
  d$ave_long[find] <-  -1 * mean(d[find, "drop_longitude_degrees"])
  d$ave_lat[find] <-  mean(d[find, "drop_latitude_degrees"])
}

# Pull out data for copper rockfish
species_data <- format_hkl_data(
  common_name = species, 
  data = d)

# Does not include wave height, moon phase, or angler location on the vessel (angler)
subdata <- species_data %>%
  group_by(year, site_number, drop) %>% 
  reframe(n = sum(number_caught),
          swell = median(swell_height_m),
          depth = median(drop_depth_meters),
          wave = median(wave_height_m),
          moon = unique(moon_proportion_fullness_r),
          vermilion = sum(vermilion),
          bocaccio = sum(bocaccio),
          lat = mean(drop_latitude_degrees),
          lon = mean(drop_longitude_degrees),
          effort = length(unique(angler)) * length(unique(hook))) 

# Format the data frame by adding factors and 0 centering quantities 
subdata <- subdata %>%
  mutate(
    year = as.factor(year),
    site_number = as.factor(site_number),
    drop = as.factor(drop),
    depth_scaled = (depth - mean(depth)) / sd(depth),
    depth_scaled_2 = depth_scaled^2,
    swell_scaled = swell - mean(swell),
    wave_scaled = wave - mean(wave),
    moon_scaled = moon - mean(moon),
    vermilion_scaled = vermilion - mean(vermilion),
    bocaccio_scaled = bocaccio - mean(bocaccio)
  )

#========================================================================
# Create the prediction grid
#========================================================================
# Year and Sites
year_site <- expand.grid(
  year = unique(subdata$year),
  site_number = unique(subdata$site_number))

## join in location info for all sites
locs <- dplyr::group_by(subdata, site_number) %>%
  dplyr::summarise(
    lat = lat[1],
    lon = lon[1],
    swell_scaled = swell_scaled[1],
    vermilion_scaled = vermilion_scaled[1],
    bocaccio_scaled = bocaccio_scaled[1],
    drop = drop[1])

grid <- dplyr::left_join(year_site, locs) %>%
  dplyr::filter(!is.na(lat + lon))
#grid$moon_scaled <- 0
#grid$wave_scaled <- 0
#grid$swell_scaled <- 0
#grid$depth_scaled <- 0
#grid$depth_scaled2 <- 0
#grid$vermilion_scaled <- 0
#grid$bocaccio_scaled <- 0
#grid$drop <- as.factor(3)

save(subdata, grid, 
  file = file.path(index_dir, "data_grid.Rdata"))

indices <- metrics <- NULL

#===============================================================================
# Negative-Binomial GLM model selection
#===============================================================================

# Create data set to use in estimating the indices
covars <- c("year", "site_number", 'drop', "swell_scaled", "moon_scaled", "bocaccio_scaled", 
            #"depth_scaled", "depth_scaled_2", "wave_scaled", 
            "vermilion_scaled", 
            "offset(log(effort))")

# wave height and swell are correlated let's look at the fit to either in th model
# including swell height included in the best model has an AIC = 5113.264
# including wave height selected in the best model (no swell include) AIC = 5117.084

model.full <- MASS::glm.nb(as.formula(
  paste("n", 
        paste(0, "+", paste(covars, collapse = " + ")), 
        sep = " ~ ")),
  data = subdata,
  na.action = "na.fail")

model.suite <- MuMIn::dredge(model.full,
                      rank = "AICc", 
                      fixed= c("offset(log(effort))", "year", "site_number", 'drop'))


#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- MuMIn::get.models(model.suite,subset = delta == 0)
best.formula <- best.model$ `8`$call$formula

save(model.suite, 
     file = file.path(index_dir, "nbglm_model_selection_drop_level.rdata"))
save(Model_selection, file = file.path(index_dir, "model_formula_drop_level.rdata"))

#format table for the document

out <- Model_selection[, -c(10, ncol(Model_selection))] # remove the logLike and weight columns
out[, 10:11] <- round(out[ , 10:11], 1)
out[, c('bocaccio_scaled', 'moon_scaled', 'swell_scaled', 'vermilion_scaled')] <- round(out[, c('bocaccio_scaled', 'moon_scaled', 'swell_scaled', 'vermilion_scaled')] , 2)
colnames(out) <- c('Bocaccio','Drop', 'Moon', 'Site', 'Swell', 'Vermilion', 'Year', 'Offset-log(effort)', 'DF', "AICc", "Delta") 
write.csv(out, file = file.path(index_dir, "forSS", "model_selection.csv"), row.names = FALSE)

#===============================================================================
# Negative-Binomial GLM with only main effects: year, site, swell, vermillion, bocaccio
#===============================================================================

name <- "glm_negbin_main_year_site_drop_swell_vermilion_bocaccio"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + year + site_number + drop + swell_scaled + vermilion_scaled + bocaccio_scaled,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  control = sdmTMBcontrol(newton_loops = 1)
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

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
  n ~ 0 + year + site_number + drop +  swell_scaled,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

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
  n ~ 0 + year + site_number + drop,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#===============================================================================
# Negative-Binomial GLM with only main effects 
#===============================================================================

name <- "glm_negbin_main_year_site_swell"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + year + site_number,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#===============================================================================
# Negative-Binomial GLM with only main effects 
#===============================================================================

name <- "glm_negbin_main_year_site"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + year + site_number,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))


#===============================================================================
# Negative-Binomial GLM with main and random site effect
#===============================================================================

name <- "glm_negbin_main_year_drop_swell_re_site"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + year + drop + swell_scaled + (1:site_number),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#===============================================================================
# Negative-Binomial GLM with main and random site effect
#===============================================================================

name <- "glm_negbin_main_year_drop_re_site"

dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + year + drop + (1:site_number),
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#=========================================================
# Delta-Model with main effects 
#=========================================================

name <- "delta_gamma_main_year_site_drop_swell"

dir.create(file.path(index_dir, name))
rm(fit, index)

fit <- sdmTMB(
  n  ~ 0 + year + site_number + drop + swell_scaled,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_gamma()
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

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

name <- "delta_lognormal_main_year_site_drop_swell"
dir.create(file.path(index_dir, name), showWarnings = FALSE)
rm(fit, index)

fit <- sdmTMB(
  n  ~ 0 + year + site_number + drop +  swell_scaled,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal()
)

sanity(fit)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))

#===============================================================================
# Negative-Binomial GLM with only main effects excluding CCA data
#===============================================================================

non_cca <- species_data %>%
  filter(cca == 0) %>%
  group_by(common_name, year, site_number, drop) %>% 
  reframe(n = sum(number_caught),
          swell = median(swell_height_m),
          vermilion = sum(vermilion),
          bocaccio = sum(bocaccio),
          lat = mean(drop_latitude_degrees),
          lon = mean(drop_longitude_degrees),
          effort = length(unique(angler)) * length(unique(hook))) 
non_cca$site_number <- droplevels(non_cca$site_number)

# Format the data frame by adding factors and 0 centering quantities 
non_cca <- non_cca %>%
  mutate(
    year = as.factor(year),
    site_number = as.factor(site_number),
    drop = as.factor(drop),
    swell_scaled = swell - mean(swell),
    vermilion_scaled = vermilion - mean(vermilion),
    bocaccio_scaled = bocaccio - mean(bocaccio)
  )


# Year and Sites
year_site <- expand.grid(
  year = unique(non_cca$year),
  site_number = unique(non_cca$site_number))

## join in location info for all sites
locs <- dplyr::group_by(non_cca, site_number) %>%
  dplyr::summarise(
    lat = lat[1],
    lon = lon[1])

grid_non_cca <- dplyr::left_join(year_site, locs) %>%
  dplyr::filter(!is.na(lat + lon))
grid_non_cca$swell_scaled <- 0
grid_non_cca$vermilion_scaled <- 0
grid_non_cca$bocaccio_scaled <- 0
grid_non_cca$drop <- as.factor(3)

name <- "glm_negbin_main_year_site_drop_swell_vermilion_bocaccio_no_cca"
dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + year + site_number + drop + swell_scaled + vermilion_scaled + bocaccio_scaled,
  data = non_cca,
  offset = log(non_cca$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid_non_cca)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))

save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))


#===============================================================================
# Negative-Binomial GLM with only main effects excluding CCA data simple model
#===============================================================================

name <- "glm_negbin_main_year_site_drop_swell_no_cca"
dir.create(file.path(index_dir, name), showWarnings = FALSE)

fit <- sdmTMB(
  n ~ 0 + year + site_number + drop + swell_scaled,
  data = non_cca,
  offset = log(non_cca$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
)

index <- calc_index(
  dir = file.path(index_dir, name), 
  fit = fit,
  grid = grid_non_cca)

do_diagnostics(
  dir = file.path(index_dir, name), 
  fit = fit)

index$model <- name
indices <- rbind(indices, index)
loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(metrics, c(name, loglike, aic))


save(indices, file = file.path(index_dir, "all_indices.rdata"))  
save(metrics, file = file.path(index_dir, "metrics.rdata"))



#===============================================================================
# RSTAN
#===============================================================================
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
library(tidybayes)
library(gridExtra)
library(fitdistrplus)

name <- "rstan_full_nb_glm_year_site_drop_swell_bocaccio_vermilon"
dir.create(file.path(index_dir, name))

start.time <- Sys.time()
stan <- stan_glm.nb(n ~ 0 + year + site_number + drop + swell_scaled + bocaccio_scaled + vermilion_scaled,
                    data = subdata,
                    offset = log(subdata$effort),
                    prior_intercept = normal(location = 0, scale = 10),
                    prior = normal(location = 0, scale = 10),
                    prior_aux = cauchy(0, 5),
                    chains = 4,
                    iter = 5000
) # iterations per chain
Sys.time() - start.time
save(stan, file = file.path(index_dir, name, "rstan_nb_glm_output.rdata"))

## pp_check
prop_zero <- function(y) mean(y == 0)

# figure of proportion zero - does good job
pngfun(wd = file.path(index_dir, name), file = "proportion_zero_south.png")
rstanarm::pp_check(stan, plotfun = "stat", stat = "prop_zero")
dev.off()

pngfun(wd = file.path(index_dir, name), file = "proportion_scatterplot.png")
rstanarm::pp_check(stan, plotfun = "stat_2d", stat = c('mean', 'sd'))
dev.off()

pp_check(stan, plotfun = "stat_grouped", stat = "median", group = "year", binwidth = 50)
pp_check(stan, plotfun = "intervals", x = "year") + ggplot2::xlab("Year")

y <- subdata$n
yrep <- posterior_predict(stan)
loo1 <- loo::loo(stan, save_psis = TRUE, cores = 4)
psis1 <- loo1$psis_object
lw <- weights(psis1)
bayesplot::ppc_loo_pit_qq(y, yrep, lw = lw, compare = 'normal')

keep_obs <- 1:50
ppc_loo_intervals(y, yrep, psis_object = psis1, subset = keep_obs)

pp_check(stan, plotfun = "hist", nreps = 5, binwidth = 5) + 
  geom_vline(xintercept = 0) + 
  xlab("laws")

yearvar <- "year"
yrvec <- as.numeric(levels(droplevels(subdata$year))) # years
yrvecin <- as.numeric(levels(droplevels(subdata$year))) # years

# Create index
# the plotindex_bayes function is from Melissa's Delta_bayes_function file
ppnb <- posterior_predict(stan, draws = 1000)
index <- plotindex_bayes(stan, yrvec,
                       backtrans = "exp", standardize = F,
                       title = "negative binomial")

nbin.draws <- as.data.frame(stan)
nbin.yrs <- data.frame(nbin.draws[, 1:length(yrvec)])
index.draws <- exp(nbin.yrs)

# calculate the index and sd
# logSD goes into the model
Index <- apply(index.draws, 2, mean) # mean(x)
SDIndex <- apply(index.draws, 2, sd) # sd(x)
int95 <- apply(index.draws, 2, quantile, probs = c(0.025, 0.975))
outdf <- cbind.data.frame(Year = yrvec, Index, SDIndex, t(int95))
# index draws already backtransformed
outdf$logIndex <- log(outdf$Index)
outdf$logmean <- apply(index.draws, 2, function(x) {
  mean(log(x))
})
outdf$logSD <- apply(index.draws, 2, function(x) {
  sd(log(x))
})

format <- cbind(outdf$Year, 9, "fleet", round(outdf$Index,5), round(outdf$logSD,3))
colnames(format) <- c("Year", "Month", "Fleet", "Estimate", "logSD")
write.csv(format, file = file.path(index_dir, name, "index_forSS.csv"), row.names = FALSE)


out_file = file.path(index_dir, name, "Index.png")
grDevices::png(filename = out_file,
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)

cex.axis = 1.25
cex.lab = 1.20
ymax = max(outdf[,"97.5%"]) + 0.10 * max(outdf[,"97.5%"])

plot(0, type = "n",
     xlim = range(outdf$Year),
     ylim = c(0, ymax),
     xlab = "", ylab = "", yaxs = "i",
     main = "", cex.axis = cex.axis)

graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
graphics::mtext(side = 2, "Relative Index", cex = cex.lab, line = 2.5)

graphics::arrows(x0 = outdf$Year, y0 = outdf[,"2.5%"], x1 = outdf$Year, y1 = outdf[,"97.5%"], 
                 angle = 90, code = 3, length = 0.01, col = "blue",
                 lty = 2)
graphics::points(outdf$Year, outdf$Index, pch = 16, bg = 1, cex = 1.6, col = 'blue')
graphics::lines(outdf$Year,  outdf$Index, cex = 1, col = 'blue', lty = 2)

dev.off()



raw.cpue <- as.data.frame(species_data) %>%
  group_by(year) %>%
  summarize(
    sites = length(unique(site_number)),
    n = sum(number_caught),
    effort = length(unique(angler)) * length(unique(hook)) * length(unique(drop)),
    cpue = n / effort,
    avg_cpue = mean(cpue)) %>%
  mutate(
      stand_cpue = avg_cpue / mean(avg_cpue)) 

outdf$stand <- outdf$Index / mean(outdf$Index)

ggplot() +
  geom_line(data = raw.cpue, aes(x = c(2004:2019, 2021:2022), y = stand_cpue, colour = 'Raw')) + 
  geom_point(data = raw.cpue,aes(x = c(2004:2019, 2021:2022), y = stand_cpue, colour = 'Raw'), size = 2) +
  geom_line(data = outdf, aes(x = Year, y = stand, colour = 'Estimated')) +
  geom_point(data = outdf, aes(x = Year, y = stand, colour = 'Estimated'), size = 2) +
  scale_colour_manual("", breaks = c("Raw", "Estimated"), values = c("Raw" = "blue", "Estimated" = "green")) +
  ylim(c(0, 2)) + ylab("Standardized CPUE") + xlab("Year")
ggsave(file = file.path(index_dir, "plots", "stand_index_raw_versus_est.png"),
       width = 10, height = 7)

#===============================================================================
# Bayesian 
#===============================================================================


name <- "glm_negbin_main_year_site_drop_swell_vermilion_bocaccio"

fit <- sdmTMB(
  n ~ 0 + year + site_number + drop +  swell_scaled + vermilion_scaled + bocaccio_scaled,
  data = subdata,
  offset = log(subdata$effort),
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  control = sdmTMBcontrol(newton_loops = 1)
)

pars <- sdmTMB::get_pars(fit)
# create a 'map' vector for TMB
# factor NA values cause TMB to fix or map the parameter at the starting value:
kappa_map <- factor(rep(NA, length(pars$ln_kappa)))

fit_mle <- update(
  fit,
  control = sdmTMBcontrol(
    start = list(
      ln_kappa = pars$ln_kappa #<
    ),
    map = list(
      ln_kappa = kappa_map #<
    )
  ),
  do_fit = FALSE #<
)

tictoc::tic()
fit_stan <- tmbstan::tmbstan(
  fit_mle$tmb_obj,
  iter = 8000, 
  chains = 2,
  control = list(adapt_delta = 0.9, max_treedepth = 12),
  thin = 10, 
  warmup = 4000, #default floor(iter/2)
  seed = 8217 # ensures repeatability
)
time = tictoc::toc() #1164.48 seconds

save(fit_stan, file = file.path(index_dir, name, "stan_output.rdata"))

# plot(fit_stan)

pars_plot1 <- paste0("b_j[", 1:12, "]")
pars_plot2 <- paste0("b_j[", 13:24, "]")
pars_plot3 <- paste0("b_j[", 25:36, "]")
pars_plot4 <- paste0("b_j[", 37:48, "]")
pars_plot5 <- paste0("b_j[", 49:60, "]")
pars_plot6 <- paste0("b_j[", 61:72, "]")
pars_plot7 <- paste0("b_j[", 73:84, "]")
pars_plot8 <- c(paste0("b_j[", 85:96, "]"), "ln_phi")

pngfun(wd = file.path(index_dir, name), file = "para_chain_1.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot1)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "para_chain_2.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot2)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "para_chain_3.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot3)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "para_chain_4.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot4)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "para_chain_5.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot5)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "para_chain_6.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot6)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "para_chain_7.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot7)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "para_chain_8.png")
bayesplot::mcmc_trace(fit_stan, pars = pars_plot8)
dev.off()

pngfun(wd = file.path(index_dir, name), file = "mcmc_pairs.png", h = 12, w = 12)
bayesplot::mcmc_pairs(fit_stan)
dev.off()

set.seed(8217)
samps <- sdmTMBextra::extract_mcmc(fit_stan)
mcmc_pred <- predict(fit, mcmc_samples = samps)
post <- rstan::extract(fit_stan)

index_mcmc <- get_index_sims(mcmc_pred)

years =   as.numeric(as.character(index_mcmc$year))
sdmtmb_est <- index_mcmc[,'est']
hi_sdmtmb  <- index_mcmc[, "upr"]
lo_sdmtmb  <- index_mcmc[, "lwr"]

out_file = file.path(index_dir, name, "Index_MCMC.png")
grDevices::png(filename = out_file,
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)
ymax <- NULL
cex.axis = 1.25
cex.lab = 1.20
if (is.null(ymax)) {
  ymax = max(hi_sdmtmb) + 0.10 * max(hi_sdmtmb)
  if(ymax > 3 * max(sdmtmb_est)){
    ymax =  3 * max(sdmtmb_est)
  }
}
x <- 0.04

plot(0, type = "n",
     xlim = range(years),
     ylim = c(0, ymax),
     xlab = "", ylab = "", yaxs = "i",
     main = "", cex.axis = cex.axis)

graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
graphics::mtext(side = 2, "Relative Index", cex = cex.lab, line = 2.5)

graphics::arrows(x0 = years + x, y0 = lo_sdmtmb, x1 = years + x, y1 = hi_sdmtmb, 
                 angle = 90, code = 3, length = 0.01, col = "blue",
                 lty = 2)
graphics::points(years + x, sdmtmb_est, pch = 16, bg = 1, cex = 1.6, col = 'blue')
graphics::lines(years + x,  sdmtmb_est, cex = 1, col = 'blue', lty = 2)

dev.off()

save(index_mcmc, mcmc_pred, samps, file = file.path(index_dir, name, "stan_fit_pred_mcmc_index.rdata"))

format_index <- data.frame(
  year = index_mcmc[,1],
  month = 9,
  fleet = 5,
  obs = index_mcmc$est,
  logse = index_mcmc$se
)
write.csv(format_index, 
          file = file.path(index_dir, name, "index_mcmc_forSS.csv"),
          row.names = FALSE)

#===============================================================================
# Compare the indices
#===============================================================================

# negative binomial with fixed effects
all <- unique(indices$model)[1:5]
tmp <- indices[indices$model %in% all, ]

cex.axis = 1.25
cex.lab = 1.20
ymax <- 2.5
ymin <- 0
colors <- viridis::viridis(length(all))
years <- c(2004:2019, 2021:2022)

grDevices::png(filename = file.path(index_dir, 'plots', "standarized_index_nbn_glm.png"),
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)
x <- 0 ; ind <- 1
for (a in all){
  
std <- tmp[tmp$model == a, 'est'] / mean(tmp[tmp$model == a, 'est'])
  
if(ind == 1){
  plot(0, type = "n",
       xlim = range(years),
       ylim = c(ymin, ymax),
       xlab = "", ylab = "", yaxs = "i",
       main = "", cex.axis = cex.axis)
  graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Standardized Index", cex = cex.lab, line = 2.5)
}

graphics::points(years + x, std, pch = 16, bg = 1, cex = 1.6, col = colors[ind])
graphics::lines(years + x,  std, cex = 1, col = colors[ind], lty = 2)

ind <- ind + 1
}
legend("topleft", bty = 'n', legend = all, col = colors, lty = 2, pch = 16)
dev.off()


# No CCA Comparison
# negative binomial with fixed effects
all <- unique(indices$model)[c(1, 10, 11)]
tmp <- indices[indices$model %in% all, ]

cex.axis = 1.25
cex.lab = 1.20
ymax <- 2
ymin <- 0
colors <- viridis::viridis(length(all))
years <- c(2004:2019, 2021:2022)

grDevices::png(filename = file.path(index_dir, 'plots', "standarized_ccca_comparison_index_nbn_glm.png"),
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)
x <- 0 ; ind <- 1
for (a in all){
  
  std <- tmp[tmp$model == a, 'est'] / mean(tmp[tmp$model == a, 'est'])
  
  if(ind == 1){
    plot(0, type = "n",
         xlim = range(years),
         ylim = c(ymin, ymax),
         xlab = "", ylab = "", yaxs = "i",
         main = "", cex.axis = cex.axis)
    graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
    graphics::mtext(side = 2, "Standardized Index", cex = cex.lab, line = 2.5)
  }
  
  graphics::points(years + x, std, pch = 16, bg = 1, cex = 1.6, col = colors[ind])
  graphics::lines(years + x,  std, cex = 1, col = colors[ind], lty = 2)
  
  ind <- ind + 1
}
legend("topleft", bty = 'n', legend = all, col = colors, lty = 2, pch = 16)
dev.off()

# Delta Compariosn
all <- unique(indices$model)[c(1, 8, 9)]
tmp <- indices[indices$model %in% all, ]

cex.axis = 1.25
cex.lab = 1.20
ymax <- 2
ymin <- 0
colors <- viridis::viridis(length(all))
years <- c(2004:2019, 2021:2022)

grDevices::png(filename = file.path(index_dir, 'plots', "standarized_index_delta_nbn_glm.png"),
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)
x <- 0 ; ind <- 1
for (a in all){
  
  std <- tmp[tmp$model == a, 'est'] / mean(tmp[tmp$model == a, 'est'])
  
  if(ind == 1){
    plot(0, type = "n",
         xlim = range(years),
         ylim = c(ymin, ymax),
         xlab = "", ylab = "", yaxs = "i",
         main = "", cex.axis = cex.axis)
    graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
    graphics::mtext(side = 2, "Standardized Index", cex = cex.lab, line = 2.5)
  }
  
  graphics::points(years + x, std, pch = 16, bg = 1, cex = 1.6, col = colors[ind])
  graphics::lines(years + x,  std, cex = 1, col = colors[ind], lty = 2)
  
  ind <- ind + 1
}
legend("topleft", bty = 'n', legend = all, col = colors, lty = 2, pch = 16)
dev.off()


# RE Comparison
all <- unique(indices$model)[c(1, 6, 7)]
tmp <- indices[indices$model %in% all, ]

cex.axis = 1.25
cex.lab = 1.20
ymax <- 2
ymin <- 0
colors <- viridis::viridis(length(all))
years <- c(2004:2019, 2021:2022)

grDevices::png(filename = file.path(index_dir, 'plots', "standarized_index_re_nbn_glm.png"),
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)
x <- 0 ; ind <- 1
for (a in all){
  
  std <- tmp[tmp$model == a, 'est'] / mean(tmp[tmp$model == a, 'est'])
  
  if(ind == 1){
    plot(0, type = "n",
         xlim = range(years),
         ylim = c(ymin, ymax),
         xlab = "", ylab = "", yaxs = "i",
         main = "", cex.axis = cex.axis)
    graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
    graphics::mtext(side = 2, "Standardized Index", cex = cex.lab, line = 2.5)
  }
  
  graphics::points(years + x, std, pch = 16, bg = 1, cex = 1.6, col = colors[ind])
  graphics::lines(years + x,  std, cex = 1, col = colors[ind], lty = 2)
  
  ind <- ind + 1
}
legend("topleft", bty = 'n', legend = all, col = colors, lty = 2, pch = 16)
dev.off()
