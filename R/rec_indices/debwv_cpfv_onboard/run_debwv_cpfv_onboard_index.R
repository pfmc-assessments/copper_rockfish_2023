#############################################################################
## DebVW index of abundance for copper rockfish for the 2023 assessment
## Model selection and negative binonimial index
## Melissa Monk
#############################################################################

rm(list = ls(all = TRUE))
graphics.off()
options(knitr.table.format = "latex")
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(tidyr)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
library(tidybayes)
library(gridExtra)
library(fitdistrplus)
library(MuMIn)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
indexName <-  "debwv_cpfv_onboard"

# Set working directories
dir <- file.path(here(),"data","rec_indices", indexName)
setwd(dir)

# create output directory
out.dir <- getwd()
plots.dir <- glue(getwd(),"/plots")

load('COPP_filtered_data.RData')
source(file.path(here(), "R","rec_indices","Delta_bayes_functions.R"))
#-------------------------------------------------------------------------------
covars <- c("YEAR", "MegaReef", "WAVE", "DEPTH_bin")
#rename effort and catch columns
  dat <- dat %>%
    rename(Effort = ANGHRS) %>%
    rename(Target = KEPT) %>%
    mutate(SubRegion = MegaReef) %>%
    mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor)


#-------------------------------------------------------------------------------
# What's the percent positive in the raw data
Percent_pos <- as.data.frame(round(with(subset(dat, Target > 0), table(YEAR)) /
  with(dat, table(YEAR)), 2))
Percent_pos


#Model selection
#full model
model.full <- MASS::glm.nb(
  Target ~ YEAR + WAVE + MegaReef + DEPTH_bin + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)

aa <- ggpredict(model.full, terms = "YEAR", back.transform = TRUE)

#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "YEAR"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- get.models(model.suite,subset = delta == 0)
#have to change manually
best.formula <- best.model$`8`$call$formula

#-------------------------------------------------------------------------------
# Negative binomial
### Negative binomial model
## Fit the main effects model in STAN and save workspace
  start.time <- Sys.time()
  
  # use STAN to see how well 'best model' fits the data
  Dnbin <- stan_glm.nb(
    Target ~ YEAR + WAVE + MegaReef + DEPTH_bin ,
  offset = dat$logEffort,
  data = dat,
#  prior_intercept = normal(location = 0, scale = 10),
#  prior = normal(location = 0, scale = 10),
#  prior_aux = cauchy(0, 5),
  chains = 4,
  iter = 5000
  ) # iterations per chain
  Sys.time() - start.time

  # nb Model checks
  # Create index
  yearvar <- "year"
  yrvec <- as.numeric(levels(droplevels(dat$YEAR))) # years
  yrvecin <- as.numeric(levels(droplevels(dat$YEAR))) # years

  # Create index
  ppnb <- posterior_predict(Dnbin, draws = 1000)
  inb <- plotindex_bayes(Dnbin, yrvec,
    backtrans = "exp", standardize = F,
    title = "negative binomial"
  )


  nbin.draws <- as.data.frame(Dnbin)
  nbin.yrs <- cbind.data.frame(nbin.draws[, 1], nbin.draws[, 1] + nbin.draws[, 2:length(yrvec)])
  colnames(nbin.yrs)[1] <- paste0(yearvar, yrvec[1])
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

  # add raw standardized index to outdf
  raw.cpue.year <- dat %>%
    group_by(YEAR) %>%
    summarise(avg_cpue = mean(CPUE)) %>%
    mutate(std.raw.cpue = avg_cpue / mean(avg_cpue))

  outdf$stdzd.raw.cpue <- raw.cpue.year$std.raw.cpue
  outdf$stdzd.Index <- outdf$Index / mean(outdf$Index)
  # write csv
  save(outdf, file = file.path(out.dir,"negativebinomial_Index.RData"))

  ## pp_check
  prop_zero <- function(y) mean(y == 0)
  # figure of proportion zero
  figure_Dnbin_prop_zero <- pp_check(Dnbin, plotfun = "stat", stat = "prop_zero", binwidth = 0.01)
  figure_Dnbin_prop_zero

  ppc_stat_grouped(dat$Target, ppnb, group = dat$YEAR, stat = "prop_zero")
  ggsave(paste0(plots.dir, "prop_zero_by_year.png"))
  
  # figure of mean and sd from model
  pp_check(Dnbin, plotfun = "stat_2d", stat = c("mean", "sd"))
  ggsave(paste0(plots.dir, "/negbin_pp_stat_mean_sd.png"))

  # boxplot of the posterior draws (light blue) compared to data (in dark blue)
  pp_check(Dnbin, plotfun = "boxplot", nreps = 10, notch = FALSE) +
    ggtitle("negative binomial model")

  # plot of mean and sd together  from posterior predictive
  ppc_stat_2d(y = dat$Target, yrep = ppnb, stat = c("mean", "sd")) + ggtitle("Negative Binomial")


  # find max mean and sd for plotting by year
  max_mean1 <- dat %>%
    group_by(YEAR) %>%
    summarise(max_mean = mean(CPUE))
  max_mean <- max(max_mean1$max_mean)
  max_sd1 <- dat %>%
    group_by(YEAR) %>%
    summarise(max_sd = max(sd(CPUE)))
  max_sd <- max(max_sd1$max_sd)
  figure.ppc.mean.by.year <- ppc_stat_grouped(
    y = dat$Target, yrep = ppnb,
    group = dat$YEAR, binwidth = 1
  ) +
    ggtitle("Negative Binomial") +
    coord_cartesian(xlim = c(0, max_mean * 1.8))
  ggsave(paste0(plots.dir, "/negbin_pp_stat_mean_grouped.png"))

  figure.ppc.sd.by.year <- ppc_stat_grouped(
    y = dat$Target, yrep = ppnb,
    group = dat$YEAR, stat = "sd",
    binwidth = 1
  ) +
    coord_cartesian(xlim = c(0, max_sd * 1.8))
  ggtitle("negative binomial")
  ggsave(paste0(plots.dir, "/negbin_pp_stat_sd_grouped.png"))

  
#-------------------------------------------------------------------------------
#Also run as sdmtmb - looks very different - yikes!
  library(sdmTMB)
  library(tmbstan)
  library(sdmTMBextra)

  grid <- expand.grid(
    YEAR = unique(dat$YEAR),
    WAVE = levels(dat$WAVE)[1], 
    MegaReef = levels(dat$MegaReef)[1],
    DEPTH_bin = levels(dat$DEPTH_bin)[1],
    logEffort = log(1.54)
  )
  
  fit.nb <- sdmTMB(
    Target ~ YEAR + WAVE + MegaReef + DEPTH_bin,
    data = dat,
    offset = dat$logEffort,
    time = "YEAR",
    spatial="off",
    spatiotemporal = "off",
    family = nbinom2(link = "log"),
    silent = TRUE,
    do_index = TRUE,
    predict_args = list(newdata = grid, re_form_iid = NA),   
    index_args = list(area = 1)
  )
  
  pred <- predict(fit.nb, return_tmb_object = TRUE, newdata = grid)
  
  index <- get_index(pred, bias_correct = TRUE)
  index

#-------------------------------------------------------------------------------

save.image(file.path(out.dir, "debwv_cpfv_onboard_index.RData")

