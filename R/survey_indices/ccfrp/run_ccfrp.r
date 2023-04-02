################################################################################
### CCFRP index
### Copper rockfish assessment 2023
### Melissa Monk
################################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RODBC)
library(here)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
ccfrpSpeciesCode <- "CPR"
#setwd to the north or the south

#setwd(glue::glue(here(),"/data/survey_indices/ccfrp/"))
dir <- file.path("S:/copper_rockfish_2023/data/survey_indices/ccfrp", modelArea)
setwd(dir)

#Assuming 20% of the habitat is in MPAs in the north and between 
#5% and 8% in the south
load("Filtered_data_CCFRP.RData")
#source delta bayes functions
source(file.path(here(),"rec_indices","Delta_bayes_functions.R"))

#-------------------------------------------------------------------------------
#Ensure columns named appropriately and covariates are factors
covars <- c("year", "month", "name", "site", "gridCellID")
dat <- dat %>%
  rename(Effort = effort) %>%
  mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor) # make sure covariates are factors

#-------------------------------------------------------------------------------
#Model selection
#full model
#Need to get depths from Becky
model.full <- MASS:glm.nb(
  Target ~ year + month + name + site  + offset(logEffort),
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

#in the north month is significant, but aix less than 5 so remove


