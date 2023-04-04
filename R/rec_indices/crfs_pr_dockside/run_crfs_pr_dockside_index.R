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
modelArea = "south"
indexName <-  "crfs_pr_dockside"
covars <- c("month", "district", "year", "prim1Common", "geara")


# Set working directories
#set working directory
dir <- file.path("S:/copper_rockfish_2023/data/rec_indices/crfs_pr_dockside",modelArea)

#dir <- file.path(here(),"data","rec_indices", indexName, modelArea)
setwd(dir)

# load data
load("data_for_GLM.RData")

#Ensure columns named appropriately and covariates are factors
dat <- cdfwpr %>%
rename(Effort = anglers) %>%
  mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor) # make sure covariates are factors

#Model selection
#full model
model.full <- MASS::glm.nb(
  kept ~ year + district + month + prim1Common + geara + offset(logEffort),
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


if(modelArea=="north"){
#set the grid
grid <- expand.grid(
  year = unique(dat$year),
  district = levels(dat$district)[1],
  targetSpecies = levels(dat$targetSpecies)[1],
  month = levels(dat$month)[1],
  logEffort = log(0.85)
)

fit.nb <- sdmTMB(
  kept ~ year + district + month + prim1Common,
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
    month = levels(dat$month)[1],
    prim1Common = levels(dat$prim1Common)[1]
  )
  
  fit.nb <- sdmTMB(
   kept ~ year + district + month + prim1Common,
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



#-------------------------------------------------------------------------------
pred <- predict(fit.nb, return_tmb_object = TRUE, newdata = grid)
index <- get_index(pred, bias_correct = TRUE)
index

#-------------------------------------------------------------------------------
# Load in some helper functions for processing and plotting the data
all <- list.files(file.path(here(), "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(here(), "R", "sdmTMB", all[a]))}

#Get diagnostics and index for SS
do_diagnostics(
  dir = dir, 
  fit = fit.nb)

calc_index(
  dir = file.path(dir, "forSS"), 
  fit = fit.nb,
  grid = grid)

#-------------------------------------------------------------------------------
#Format data filtering table and the model selection table for document
View(dataFilters)
dataFilters <- dataFilters %>%
rowwise() %>%
filter(!all(is.na(across((everything()))))) %>%
ungroup() %>%
rename(`Positive Samples` = Positive_Samples) %>%
as.data.frame()

write.csv(dataFilters, 
file = file.path(dir, "forSS", "data_filters.csv"), 
row.names = FALSE)

View(Model_selection)
#format table for the document
out <- Model_selection %>%
dplyr::select(-`(Intercept)`) %>%
mutate_at(vars(covars,"year","offset(logEffort)"), as.character) %>%
mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
replace_na(list(district = "Excluded", geara = "Excluded",
          prim1Common = "Excluded", month = "Excluded")) %>%
mutate_at(c(covars,"year","offset(logEffort)"), 
       funs(stringr::str_replace(.,"\\+","Included"))) %>%
rename(`Effort offset` = `offset(logEffort)`, 
`log-likelihood` = logLik,
`Primary target species` = prim1Common,
`Primary gear` = geara) %>%
rename_with(stringr::str_to_title,-AICc)
View(out)
write.csv(out, file = file.path(dir, "forSS", "model_selection.csv"), row.names = FALSE)
