#########################################################################
### Run the CDFW PR dockside data to get an index of abundance
### Copper assessment 2023
### Melissa Monk
#########################################################################
# Run the MRFSS cpfv dockside index
# Melissa Monk June 2021; modified for copper 2023

rm(list = ls(all = TRUE))
graphics.off()

library(sdmTMB)
library(tmbstan)
library(sdmTMBextra)
library(ggeffects)
library(MuMIn)
library(here)
library(glue)
library(tidyr)
library(dplyr)
#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
indexName <-  "crfs_pr_dockside"
covars <- c("month", "district", "year", "targetSpecies")

# Load in some helper functions for processing and plotting the data
all <- list.files(file.path(here(), "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(here(), "R", "sdmTMB", all[a]))}

# Set working directories
dir <- file.path(here(),"data","rec_indices", indexName, modelArea)
setwd(dir)
# create output directory for each model
plots.dir <- glue(getwd(),"/plots")

# load data
load(glue(getwd(),"/",indexName,"_data_for_GLM.RData"))

#Ensure columns named appropriately and covariates are factors
dat <- tripData %>%
  rename(effort = NUMBER_OF_ANGLERS,
         year = RECFIN_YEAR,
         month = RECFIN_MONTH,
         district = RECFIN_PORT_CODE,
         targetSpecies = PRIMARY_TARGET_SPECIES_NAME,
         catch = targetCatch) %>%
  mutate(logEffort = log(effort)) %>%
  mutate_at(covars, as.factor) # make sure covariates are factors

#Model selection
#full model
model.full <- MASS::glm.nb(
  catch ~ year + district + month + targetSpecies + offset(logEffort),
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
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- get.models(model.suite,subset = delta == 0)


if(modelArea=="north"){
#set the grid
grid <- expand.grid(
  year = unique(dat$year),
  district = levels(dat$district)[1],
  targetSpecies = levels(dat$targetSpecies)[1],
  month = levels(dat$month)[1],
  logEffort = log(0.85)
)

fit <- sdmTMB(
  catch ~ year + district + month + targetSpecies,
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
  control = sdmTMBcontrol(newton_loops = 1)
)
} else {
  #set the grid
  grid <- expand.grid(
    year = unique(dat$year),
    district = levels(dat$district)[1],
 #   Month = levels(dat$Month)[1],
    logEffort = log(0.85)
  )
  
  fit <- sdmTMB(
    catch ~ year + district,
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

sanity(fit)

index <- calc_index(
  dir = dir, 
  fit = fit,
  grid = grid)

do_diagnostics(
  dir = dir, 
  fit = fit)

loglike <- logLik(fit)
aic <- AIC(fit)
metrics <- rbind(c(indexName, loglike, aic))

save(index, file = file.path(dir, "index.rdata"))  
save(metrics, file = file.path(dir, "metrics.rdata"))

