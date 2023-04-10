#########################################################################
### Run the CDFW CRFS onboard observer index of abundance
### Copper assessment 2023
### Melissa Monk
#########################################################################
# Run the CRFS onboard index

rm(list = ls(all = TRUE))
graphics.off()

library(sdmTMB)
library(tmbstan)
library(ggeffects)
library(MuMIn)
library(here)
library(glue)
library(tidyr)
library(dplyr)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
library(tidybayes)
library(gridExtra)
library(fitdistrplus)
#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
indexName <-  "crfs_cpfv_onboard"

#keep depth as continuous
covars <- c("month", "region", "year")


# Load in some helper functions for processing and plotting the data
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}


all <- list.files(file.path(user_dir, "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(user_dir, "R", "sdmTMB", all[a]))}
# Set working directories
#set working directory
dir <- file.path(here(), "data", "rec_indices", "crfs_cpfv_onboard", modelArea)

#dir <- file.path(here(),"data","rec_indices", indexName, modelArea)
setwd(dir)

# load data
load("data_for_GLM.RData")


#Ensure columns named appropriately and covariates are factors
dat <- onboard %>%
  mutate(logEffort = log(effort)) %>%
  mutate_at(covars, as.factor) %>% # make sure covariates are factors
  mutate(depth_2 = depth^2)


if(modelArea == "north"){
#going to have to combine 4-6
dat <- dat %>%
  mutate(region = ifelse(region %in% c(4,5,6), "4_6", "3")) %>%
  mutate_at(vars(region), as.factor)
}
#-------------------------------------------------------------------------------
#Main effects model
#Model selection
#full model
model.full <- MASS::glm.nb(
  number.fish ~ year + region + month + depth + depth_2 + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
#use ggpredict to get an estimate of the logEffort for sdmTMB predictions
#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
dplyr::select(-weight)
Model_selection

#drop month and depth^2
if(modelArea=="north"){
 
#set the grid
  #dropping month - difference in AIC is 7.8
grid <- expand.grid(
  year = unique(dat$year),
  region = levels(dat$region)[1],
  depth = dat$depth[1],
  depth_2 = dat$depth_2[1])


fit.nb <- sdmTMB(
  number.fish ~ year + region + poly(depth, 2),
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  control = sdmTMBcontrol(newton_loops = 1))

} else {
  
  #set the grid for the south
  grid <- expand.grid(
    year = unique(dat$year),
    region = levels(dat$region)[1],
    month = levels(dat$month)[1],
    depth = dat$depth[1],
    depth_2 = dat$depth_2[1])
    
  
  fit.nb <- sdmTMB(
    number.fish ~ year + poly(depth, 2) + month + region,
    data = dat,
    offset = dat$logEffort,
    time = "year",
    spatial="off",
    spatiotemporal = "off",
    family = nbinom2(link = "log"),
    silent = TRUE,
    do_index = TRUE,
    predict_args = list(newdata = grid, re_form_iid = NA),   
    index_args = list(area = 1),
    control = sdmTMBcontrol(newton_loops = 1) #not entirely sure what this does
  )
}

#Get diagnostics and index for SS
do_diagnostics(
  dir = file.path(dir, "main_effects"), 
  fit = fit.nb)

calc_index(
  dir = file.path(dir, "main_effects"), 
  fit = fit.nb,
  grid = grid)

#-------------------------------------------------------------------------------
#Format data filtering table and the model selection table for document
View(dataFilters)

dataFilters <- dataFilters %>%
rowwise() %>%
filter(!all(is.na(across((everything()))))) %>%
ungroup() %>%
  mutate()
rename(`Positive Samples` = Positive_Samples) %>% 
as.data.frame()

write.csv(dataFilters, file = file.path(dir, "main_effects", "data_filters.csv"), row.names = FALSE)

View(Model_selection)
#format table for the document
out <- Model_selection %>%
dplyr::select(-`(Intercept)`) %>%
  mutate(depth = round(depth, 3),
         depth_2 = round(depth_2), 3) %>%
  rename(DepthSquared = depth_2,
         Depth = depth) %>%
mutate_at(vars(covars,"year","offset(logEffort)" ,"DepthSquared",  "Depth"), as.character) %>%
mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
replace_na(list(month = "Excluded", region = "Excluded", Depth = "Excluded" ,DepthSquared = "Excluded")) %>%
mutate_at(c(covars,"year","offset(logEffort)", "DepthSquared"), 
       funs(stringr::str_replace(.,"\\+","Included"))) %>%
rename(`Effort offset` = `offset(logEffort)`, 
       `log-likelihood` = logLik,
        `Depth squared` = DepthSquared) %>%
rename_with(stringr::str_to_title,-AICc)
View(out)
write.csv(out, file = file.path(dir, "main_effects", "model_selection.csv"), row.names = FALSE)

#summary of trips and  percent pos per year
summaries <- dat %>%
  group_by(year) %>%
  summarise(tripsWithTarget = sum(kept>0),
            tripsWOTarget = sum(kept==0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
         percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 
View(summaries)
write.csv(summaries, 
file.path(dir, "main_effects", "percent_pos.csv"),
row.names=FALSE)

#-------------------------------------------------------------------------------
#Area-weighted index
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#sdmTMB - NORTH only
#fraction of rocky habitat by district in state waters only
north_district_weights <- data.frame(district = c(3,4,5,6),
                                  area_weight = c(0.3227, 0.321, 0.162, 0.1943))


#Model selection
#full model
model.full <- MASS::glm.nb(
  number.fish ~ year + region + depth + depth_2 + year:region + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
#use ggpredict to get an estimate of the logEffort for sdmTMB predictions
ggpredict(model.full, terms = "year")
#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection

# if(modelArea=="north"){
  #set the grid
grid <- expand.grid(
  year = unique(dat$year),
  region = levels(dat$region)[1],
  depth = dat$depth[1],
  depth_2 = dat$depth_2[1])

 grid$district_year <- 1

 district3 <- round(0.32 * 100, 0)
 district4_6 <- round(0.68 * 100, 0)

 
 grid_north <- NULL
 for (a in 1:32){
   grid_north <- rbind(grid_north, grid[grid$region == "3", ])
 }
 for (a in 1:68){
   grid_north <- rbind(grid_north, grid[grid$region == "4_6", ])
 }

  
  fit.nb <- sdmTMB(
    kept ~ year + region + poly(depth, 2) + year:region,
    data = dat,
    offset = dat$logEffort,
    time = "year",
    spatial="off",
    spatiotemporal = "off",
    family = nbinom2(link = "log"),
    control = sdmTMBcontrol(newton_loops = 1))
 
  do_diagnostics(
    dir = file.path(dir, "area_weighted"), 
    fit = fit.nb,
    plot_resid = FALSE)
  
  calc_index(
    dir = file.path(dir, "area_weighted"), 
    fit = fit.nb,
    grid = grid_north)

  
  
# } else {
#   #set the grid
#   grid <- expand.grid(
#     year = unique(dat$year),
#     district = levels(dat$district)[1],
#     month = levels(dat$month)[1],
#     targetSpecies = levels(dat$targetSpecies)[1]
#   )
#   
#   fit.nb <- sdmTMB(
#     kept ~ year + district + month + targetSpecies,
#     data = dat,
#     offset = dat$logEffort,
#     time = "year",
#     spatial="off",
#     spatiotemporal = "off",
#     family = nbinom2(link = "log"),
#     silent = TRUE,
#     do_index = TRUE,
#     predict_args = list(newdata = grid, re_form_iid = NA),   
#     index_args = list(area = 1),
#     control = sdmTMBcontrol(newton_loops = 1) #not entirely sure what this does
#   )
# }
# 

#Get diagnostics and index for SS
# do_diagnostics(
#   dir = file.path(dir, "area_weighted"), 
#   fit = fit.nb)
# 
# calc_index(
#   dir = file.path(dir, "area_weighted"), 
#   fit = fit.nb,
#   grid = grid)
# 


#-------------------------------------------------------------------------------
#Format data filtering table and the model selection table for document
write.csv(dataFilters, 
          file = file.path(dir, "area_weighted", "data_filters.csv"), 
          row.names = FALSE)

View(Model_selection)
#format table for the document
out <- Model_selection %>%
  dplyr::select(-`(Intercept)`) %>%
  mutate(depth = round(depth, 3),
         depth_2 = round(depth_2, 3)) %>%
  mutate_at(vars("year","offset(logEffort)", "depth", "depth_2", "region", "region:year"), as.character) %>%
  mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
  replace_na(list(region = "Excluded", `region:year` = "Excluded",
                  depth = "Excluded", depth_2 = "Excluded")) %>%
  mutate_at(c("year","offset(logEffort)", "depth", "depth_2", "region", "region:year"), 
            funs(stringr::str_replace(.,"\\+","Included"))) %>%
  rename(`Effort offset` = `offset(logEffort)`, 
         `log-likelihood` = logLik,
         `Depth squared` = depth_2,
         `Interaction` = `region:year`,
          Depth = depth) %>%
  rename_with(stringr::str_to_title,-AICc)
View(out)
write.csv(out, file = file.path(dir, "area_weighted", "model_selection.csv"), 
          row.names = FALSE)


#-------------------------------------------------------------------------------
#not running on melissa's machine
# Source delta glm plotting functions
 source(file.path(user_dir, "R", "rec_indices", "Delta_bayes_functions.R"))
  # use STAN to see how well 'best model' fits the data
  Dnbin <- stan_glm.nb(
     number.fish ~ year + depth + region + year:region,
   offset = logEffort,
   data = dat,
   prior_intercept = normal(location = 0, scale = 10),
   prior = normal(location = 0, scale = 10),
   prior_aux = cauchy(0, 5),
   chains = 4,
   iter = 5000) # iterations per chain
   Sys.time() - start.time
   save(Dnbin, file = file.path(dir, "area_weighted_bayesian", "Dnbin.RData"))
   

#   # nb Model checks
#   # Create index
   yearvar <- "year"
   yrvec <- as.numeric(levels(droplevels(dat$year))) # years
   yrvecin <- as.numeric(levels(droplevels(dat$year))) # years
#   # Create index
   ppnb <- posterior_predict(Dnbin, draws = 1000)
   inb <- plotindex_bayes(Dnbin, yrvec,
     backtrans = "exp", standardize = F,
     title = "negative binomial"
   )


   nbin.draws <- as.data.frame(Dnbin)
   nbin.yrs <- cbind.data.frame(nbin.draws[, 1], nbin.draws[, 1] + nbin.draws[, 2:length(yrvec)])
   colnames(nbin.yrs)[1] <- paste0(yearvar, yrvec[1])
   index.draws <- exp(nbin.yrs)

#   # calculate the index and sd
#   # logSD goes into the model
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

#   # add raw standardized index to outdf
   raw.cpue.year <- dat %>%
     group_by(year) %>%
     summarise(avg_cpue = mean(cpue)) %>%
     mutate(std.raw.cpue = avg_cpue / mean(avg_cpue))

   outdf$stdzd.raw.cpue <- raw.cpue.year$std.raw.cpue
   outdf$stdzd.Index <- outdf$Index / mean(outdf$Index)
   # write csv
   #write.csv(outdf, paste0(
   #  out.dir, "/", Model_region[Model.number], "_negativebinomial_",
   #  species.name, "_",
   #  survey.name, "_Index.csv"
   #))
   write.csv(outdf, file = file.path(dir, "area_weighted_bayesian", "Index.csv"))

#   ## pp_check
   prop_zero <- function(y) mean(y == 0)
   # figure of proportion zero
   figure_Dnbin_prop_zero <- pp_check(Dnbin,
   plotfun = "stat", stat = "prop_zero", binwidth = 0.001)
   HandyCode::pngfun(wd = file.path(dir, "area_weighted_bayesian"), file = "negbin_prop_zero.png")
   figure_Dnbin_prop_zero
   dev.off()
   #ggsave(file.path(dir, "area_weighted_bayesian", "negbin_prop_zero.png"))
 # figure of mean and sd from model
   HandyCode::pngfun(wd = file.path(dir, "area_weighted_bayesian"), file = "negbin_pp_stat_mean_sd.png")
   pp_check(Dnbin, plotfun = "stat_2d", stat = c("mean", "sd"))
   dev.off()
   #ggsave(file.path(dir, "area_weighted_bayesian", "negbin_pp_stat_mean_sd.png"))

# # boxplot of the posterior draws (light blue) compared to data (in dark blue)
   HandyCode::pngfun(wd = file.path(dir, "area_weighted_bayesian"), file = "negbin_pp_stat_boxplot.png")
   pp_check(Dnbin, plotfun = "boxplot", nreps = 10, notch = FALSE) +
     ggtitle("negative binomial model")
   dev.off()

# # plot of mean and sd together  from posterior predictive
   HandyCode::pngfun(wd = file.path(dir, "area_weighted_bayesian"), file = "negbin_pp_stat_mean_sd_kept.png")
   ppc_stat_2d(y = dat$kept, yrep = ppnb, stat = c("mean", "sd")) + ggtitle("Negative Binomial")
   dev.off()

