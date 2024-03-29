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
#library(rstanarm)
#options(mc.cores = parallel::detectCores())
library(ggplot2)
#library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
#library(tidybayes)
#library(gridExtra)
#library(fitdistrplus)
#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "south"
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
dir <- file.path(here(), "data", "rec_indices", "crfs_cpfv_onboard", modelArea, "start2004")

#dir <- file.path(here(),"data","rec_indices", indexName, modelArea)
setwd(dir)

# load data
load("data_for_GLM.RData")


#Ensure columns named appropriately and covariates are factors
dat <- onboard %>%
  mutate(logEffort = log(effort)) %>%
  mutate_at(covars, as.factor) %>% # make sure covariates are factors
  mutate(depth = depth/6) %>%
  mutate(depth_2 = depth^2) %>%
  mutate(depth_scaled = (depth - mean(depth)) / sd(depth),
         depth_scaled2 = depth_scaled^2) %>%
  droplevels

#-----------------------------------------------------------------------------
#Main effects model
#Model selection
#full model
model.full <- MASS::glm.nb(
  number.fish ~ year + region + month + depth_scaled + depth_scaled2 + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)

model.full <- MASS::glm.nb(
  number.fish ~ year + region + month + depth_scaled + depth_scaled2 + offset(logEffort),
  data = dat,
  na.action = "na.fail")
#use ggpredict to get an estimate of the logEffort for sdmTMB predictions
#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection

#set the grid for the south
grid <- expand.grid(
  year = unique(dat$year),
  region = levels(dat$region)[1],
  month = levels(dat$month)[1],
  depth_scaled = dat$depth_scaled[1],
  depth_scaled2 = dat$depth_scaled2[1])

#neg binomial
fit.nb <- sdmTMB(
  number.fish ~ year + poly(depth_scaled,2) + month + region,
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


#Get diagnostics and index for SS
do_diagnostics(
  dir = file.path(dir, "negbin"), 
  fit = fit.nb)

calc_index(
  dir = file.path(dir, "negbin"), 
  fit = fit.nb,
  grid = grid)


# delta gamma ----
fit.gam = sdmTMB(
  number.fish ~ year + month + poly(depth_scaled, 2) + region,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_gamma(),
  control = sdmTMBcontrol(newton_loops = 1))

do_diagnostics(
  dir = file.path(dir, "deltagamma"), 
  fit = fit.gam,
  plot_resid = FALSE)

calc_index(
  dir = file.path(dir, "deltagamma"), 
  fit = fit.gam,
  grid = grid)

# delta logn ----
fit.logn = sdmTMB(
  number.fish ~ year + poly(depth_scaled, 2) + month + region,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 1))

do_diagnostics(
  dir = file.path(dir, "deltalogn"), 
  fit = fit.logn,
  plot_resid = FALSE)

calc_index(
  dir = file.path(dir, "deltalogn"), 
  fit = fit.logn,
  grid = grid)



#-------------------------------------------------------------------------------
#Format data filtering table and the model selection table for document
#View(dataFilters)

dataFilters <- dataFilters %>%
  rowwise() %>%
  filter(!all(is.na(across((everything()))))) %>%
  ungroup() %>%
#  rename(`Positive Samples` = Positive_Samples) %>% 
  as.data.frame()
dataFilters <- data.frame(lapply(dataFilters, as.character), stringsasFactors = FALSE)
write.csv(dataFilters, file = file.path(dir, "data_filters.csv"), row.names = FALSE)

#View(Model_selection)
#format table for the document
# out <- Model_selection %>%
#   dplyr::select(-`(Intercept)`) %>%
#   mutate(depth = round(depth, 3)) %>%
#   rename(Depth = depth,
#          Region = region) %>%
#   mutate_at(vars("year","offset(logEffort)" ,"Region",  "Depth"), as.character) %>%
#   mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
#   rename(`Effort offset` = `offset(logEffort)`, 
#          `log-likelihood` = logLik) %>%
#   rename_with(stringr::str_to_title,-AICc)
#View(out)
write.csv(out, file = file.path(dir, "Model_selection.csv"), row.names = FALSE)

#summary of trips and  percent pos per year
summaries <- dat %>%
  group_by(year) %>%
  summarise(tripsWithTarget = sum(kept>0),
            tripsWOTarget = sum(kept==0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
         percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 
#View(summaries)
write.csv(summaries, file.path(dir,  "percent_pos.csv"), row.names=FALSE)



#area-weighted index ----
#-------------------------------------------------------------------------------
model.full <- MASS::glm.nb(
  number.fish ~ year + region + month + depth_scaled2 +depth_scaled + year:region + offset(logEffort),
  data = dat,
  na.action = "na.fail")
#use ggpredict to get an estimate of the logEffort for sdmTMB predictions
#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection

pos <- dat %>% filter(number.fish >0)
with(pos, table(year, region))

grid_south <- expand.grid(
  year = unique(dat$year),
  region = levels(dat$region),
  month = levels(dat$month)[1],
  depth_scaled = dat$depth_scaled[1],
  depth_Scaled2 = dat$depth_scaled2[1])

grid_south$region_year <- 1

# District1 <- round(0.12 * 100, 0)
# District2 <- round(0.07 * 100, 0)
# Nchannel <- round(0.44 * 100, 0)
# Schannel <- round(0.36 * 100, 0)

grid_south <- NULL
for (a in 1:12){
  grid_south <- rbind(grid_south, grid[grid$region ==  "District 1 mainland", ])
}
for (a in 1:7){
  grid_south <- rbind(grid_south, grid[grid$region == "District 2 mainland", ])
}
for (a in 1:44){
  grid_south <- rbind(grid_south, grid[grid$region == "Northern Channel Islands", ])
}
for (a in 1:36){
  grid_south <- rbind(grid_south, grid[grid$region == "Southern Channel Islands", ])
}

fit.logn.w = sdmTMB(
  number.fish ~ year*region + month + poly(depth_scaled,2),
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 1))

do_diagnostics(
  dir = file.path(dir, "area_weighted_logn"), 
  fit = fit.logn.w,
  plot_resid = FALSE)

calc_index(
  dir = file.path(dir, "area_weighted_logn"), 
  fit = fit.logn.w,
  grid = grid_south)
