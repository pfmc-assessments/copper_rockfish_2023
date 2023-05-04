#############################################################################
## DebVW index of abundance for copper rockfish for the 2023 assessment
## Model selection and negative binonimial index
## Melissa Monk
#############################################################################

rm(list = ls(all = TRUE))
graphics.off()
options(knitr.table.format = "latex")
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(devtools)
library(ggeffects)
library(tidybayes)
library(gridExtra)
library(fitdistrplus)
library(MuMIn)
library(here)
library(glue)
#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
indexName <-  "debwv_cpfv_onboard"
#modelName = "area_weighted"
# Set working directories
dir <- file.path(here(),"data","rec_indices", indexName)
setwd(dir)

load("COPP_filtered_data.RData")
#load(file.path(here(),"data","rec_indices",indexName, 'COPP_filtered_data.RData'))
r_code_location <- "C:/Users/melissa.monk/Documents/Github/copper_rockfish_2023/R"
#-------------------------------------------------------------------------------
covars <- c("year", "reef", "wave")
#rename effort and catch columns
  dat <- dat %>%
    rename(Effort = ANGHRS) %>%
    rename(Target = KEPT,
           year = YEAR,
           wave = WAVE,
           depth = DEPTH,
           cpue = CPUE) %>%
    mutate(reef = MegaReef) %>%
    mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor)


#-------------------------------------------------------------------------------
# What's the percent positive in the raw data
Percent_pos <- as.data.frame(round(with(subset(dat, Target > 0), table(year)) /
  with(dat, table(year)), 2))
Percent_pos

#CPUE plot by reef 
ggplot(dat %>% group_by(reef, year) %>% summarise(average_cpue = mean(cpue)), 
       aes(x = year, y = average_cpue, colour = reef, group = reef)) +
  geom_line()+
  geom_point(size = 3)  + theme_bw() +
  geom_line(aes(x = year, y = average_cpue, 
                colour = reef)) +
  xlab("Year") + ylab("Average CPUE") + ylim(c(0, .2)) + 
  scale_color_viridis_d()
ggsave(file = file.path(getwd(),"average_cpue_by_reef.png"), width = 7, height = 7)

#cpue by depth
ggplot(dat, aes(x = cpue, y = depth)) +
  geom_point(alpha = .5)

summary(dat$wave)
summary(dat$year)
summary(dat$reef)
summary(dat$depth)

dat$depth_2 <- dat$depth^2

#Model selection
#full model - not using wave
model.full <- MASS::glm.nb(
  Target ~ year + reef + depth + depth_2 + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)

aa <- ggpredict(model.full, terms = "year", back.transform = TRUE)

#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection


#-------------------------------------------------------------------------------
# Negative binomial
### Negative binomial model
## Fit the main effects model in STAN and save workspace
#   start.time <- Sys.time()
#   
#   # use STAN to see how well 'best model' fits the data
#   Dnbin <- stan_glm.nb(
#     Target ~ year + wave + reef + depth ,
#   offset = dat$logEffort,
#   data = dat,
# #  prior_intercept = normal(location = 0, scale = 10),
# #  prior = normal(location = 0, scale = 10),
# #  prior_aux = cauchy(0, 5),
#   chains = 4,
#   iter = 5000
#   ) # iterations per chain
#   Sys.time() - start.time
# save(Dnbin, file = file.path(getwd(),"Dnbin.Rdata"))
#   # nb Model checks
#   # Create index
#   yearvar <- "year"
#   yrvec <- as.numeric(levels(droplevels(dat$year))) # years
#   yrvecin <- as.numeric(levels(droplevels(dat$year))) # years
# 
#   # Create index
#   ppnb <- posterior_predict(Dnbin, draws = 1000)
#   inb <- plotindex_bayes(Dnbin, yrvec,
#     backtrans = "exp", standardize = F,
#     title = "negative binomial"
#   )
# 
# 
#   nbin.draws <- as.data.frame(Dnbin)
#   nbin.yrs <- cbind.data.frame(nbin.draws[, 1], nbin.draws[, 1] + nbin.draws[, 2:length(yrvec)])
#   colnames(nbin.yrs)[1] <- paste0(yearvar, yrvec[1])
#   index.draws <- exp(nbin.yrs)
# 
# 
#   # calculate the index and sd
#   # logSD goes into the model
#   Index <- apply(index.draws, 2, mean) # mean(x)
#   SDIndex <- apply(index.draws, 2, sd) # sd(x)
#   int95 <- apply(index.draws, 2, quantile, probs = c(0.025, 0.975))
#   outdf <- cbind.data.frame(year = yrvec, Index, SDIndex, t(int95))
#   # index draws already backtransformed
#   outdf$logIndex <- log(outdf$Index)
#   outdf$logmean <- apply(index.draws, 2, function(x) {
#     mean(log(x))
#   })
#   outdf$logSD <- apply(index.draws, 2, function(x) {
#     sd(log(x))
#   })
# 
#   # add raw standardized index to outdf
#   raw.cpue.year <- dat %>%
#     group_by(year) %>%
#     summarise(avg_cpue = mean(CPUE)) %>%
#     mutate(std.raw.cpue = avg_cpue / mean(avg_cpue))
# 
#   outdf$stdzd.raw.cpue <- raw.cpue.year$std.raw.cpue
#   outdf$stdzd.Index <- outdf$Index / mean(outdf$Index)
#   # write csv
#   save(outdf, file = file.path(out.dir,"negativebinomial_Index.RData"))
# 
#   ## pp_check
#   prop_zero <- function(y) mean(y == 0)
#   # figure of proportion zero
#   figure_Dnbin_prop_zero <- pp_check(Dnbin, plotfun = "stat", stat = "prop_zero", binwidth = 0.01)
#   figure_Dnbin_prop_zero
# 
#   ppc_stat_grouped(dat$Target, ppnb, group = dat$year, stat = "prop_zero")
#   ggsave(paste0(plots.dir, "prop_zero_by_year.png"))
#   
#   # figure of mean and sd from model
#   pp_check(Dnbin, plotfun = "stat_2d", stat = c("mean", "sd"))
#   ggsave(paste0(plots.dir, "/negbin_pp_stat_mean_sd.png"))
# 
#   # boxplot of the posterior draws (light blue) compared to data (in dark blue)
#   pp_check(Dnbin, plotfun = "boxplot", nreps = 10, notch = FALSE) +
#     ggtitle("negative binomial model")
# 
#   # plot of mean and sd together  from posterior predictive
#   ppc_stat_2d(y = dat$Target, yrep = ppnb, stat = c("mean", "sd")) + ggtitle("Negative Binomial")
# 
# 
#   # find max mean and sd for plotting by year
#   max_mean1 <- dat %>%
#     group_by(year) %>%
#     summarise(max_mean = mean(CPUE))
#   max_mean <- max(max_mean1$max_mean)
#   max_sd1 <- dat %>%
#     group_by(year) %>%
#     summarise(max_sd = max(sd(CPUE)))
#   max_sd <- max(max_sd1$max_sd)
#   figure.ppc.mean.by.year <- ppc_stat_grouped(
#     y = dat$Target, yrep = ppnb,
#     group = dat$year, binwidth = 1
#   ) +
#     ggtitle("Negative Binomial") +
#     coord_cartesian(xlim = c(0, max_mean * 1.8))
#   ggsave(paste0(plots.dir, "/negbin_pp_stat_mean_grouped.png"))
# 
#   figure.ppc.sd.by.year <- ppc_stat_grouped(
#     y = dat$Target, yrep = ppnb,
#     group = dat$year, stat = "sd",
#     binwidth = 1
#   ) +
#     coord_cartesian(xlim = c(0, max_sd * 1.8))
#   ggtitle("negative binomial")
#   ggsave(paste0(plots.dir, "/negbin_pp_stat_sd_grouped.png"))
# 
  
#-------------------------------------------------------------------------------
#Also run as sdmtmb - looks very different - yikes!
  library(sdmTMB)
  library(tmbstan)
  #library(sdmTMBextra)

  grid <- expand.grid(
    year = unique(dat$year),
  #  wave = levels(dat$wave)[1], 
    reef = levels(dat$reef)[1],
    depth = dat$depth[1],
    depth_2 = dat$depth_2[1]
  )
  
  fit.nb <- sdmTMB(
    Target ~ year  + reef + poly(depth, 2),
    data = dat,
    offset = dat$logEffort,
    time = "year",
    spatial="off",
    spatiotemporal = "off",
    family = nbinom2(link = "log"),
    control = sdmTMBcontrol(newton_loops = 1)) #not entirely sure what this does
  
----------------------------------------------------------------
  # Load in some helper functions for processing and plotting the data
  all <- list.files(file.path(r_code_location, "sdmTMB"))
  for (a in 1:length(all)) { source(file.path(r_code_location, "sdmTMB", all[a]))}
  
  #Get diagnostics and index for SS
  do_diagnostics(
    dir = file.path(getwd(),"main_effects"), 
    fit = fit.nb)
  
  calc_index(
    dir = file.path(getwd(),"main_effects"), 
    fit = fit.nb,
    grid = grid)
  
  #-------------------------------------------------------------------------------
  #Format data filtering table and the model selection table for document
  View(dataFilters)
  dataFilters <- data_filters %>%
    rowwise() %>%
    filter(!all(is.na(across((everything()))))) %>%
    ungroup() %>%
    rename(`Positive Samples` = Positive_Samples) %>%
    as.data.frame()
  dataFilters <- data.frame(lapply(dataFilters, as.character), stringsasFactors = FALSE)
  
  write.csv(dataFilters, 
            file = file.path(getwd(), "data_filters.csv"), 
            row.names = FALSE)
  
  View(Model_selection)
  #format table for the document
  out <- Model_selection %>%
    dplyr::select(-`(Intercept)`) %>%
    mutate_at(vars(covars,"year","offset(logEffort)"), as.character) %>%
    mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
    replace_na(list( reef = "Excluded")) %>%
    mutate_at(c(covars,"year","offset(logEffort)"), 
              funs(stringr::str_replace(.,"\\+","Included"))) %>%
    rename(`Effort offset` = `offset(logEffort)`, 
           `log-likelihood` = logLik,
           `Depth squared` = depth_2) %>%
    rename_with(stringr::str_to_title,-AICc)
 # View(out)
  write.csv(out, file = file.path(getwd(), "model_selection.csv"), row.names = FALSE)
  
  #summary of trips and  percent pos per year
  summaries <- dat %>%
    group_by(year) %>%
    summarise(tripsWithTarget = sum(Target>0),
              tripsWOTarget = sum(Target==0)) %>%
    mutate(totalTrips = tripsWithTarget+tripsWOTarget,
           percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 
  #View(summaries)
  write.csv(summaries, 
            file.path(getwd(),"percent_pos.csv"),
            row.names=FALSE)
  

  
  
  
#Delta_models ------------------------------------------------------------------- 

  

  
  fit <- sdmTMB(
    Target ~ year  + reef + poly(depth, 2),
    data = dat,
    offset = dat$logEffort,
    time = "year",
    spatial="off",
    spatiotemporal = "off",
    family = delta_lognormal(),
    control = sdmTMBcontrol(newton_loops = 1)
  )
  
  index <- calc_index(
    dir = file.path(getwd(), "deltalogn"), 
    fit = fit,
    grid = grid)
  
  do_diagnostics(
    dir = file.path(getwd(),"deltalogn"), 
    fit = fit)
  
  index$model <- modelName
  indices <- rbind(indices, index)
  loglike <- logLik(fit)
  aic <- AIC(fit)
  metrics <- rbind(metrics, c(name, loglike, aic))
  
  #save(indices, file = file.path(dir, "all_indices.rdata"))  
  #save(metrics, file = file.path(dir, "metrics.rdata"))
  
#Delta gamma


  
  fit <- sdmTMB(
    Target ~ year  + reef + poly(depth, 2),
    data = dat,
    offset = dat$logEffort,
    time = "year",
    spatial="off",
    spatiotemporal = "off",
    family = delta_gamma(),
    control = sdmTMBcontrol(newton_loops = 1)
  )
  
  index <- calc_index(
    dir = file.path(getwd(), "deltagamma"), 
    fit = fit,
    grid = grid)
  
  do_diagnostics(
    dir = file.path(getwd(),"deltagamma"), 
    fit = fit)
  