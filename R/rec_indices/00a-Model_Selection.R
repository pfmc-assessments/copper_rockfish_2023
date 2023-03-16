# Model selection for indices
# Melissa Monk June 2021; modified for copper 2023

rm(list = ls(all = TRUE))
graphics.off()
library(tidyr)
library(dplyr)
library(ggplot2)
library(grid)
library(devtools)
library(ggeffects)
library(gridExtra)
library(glue)
library(here)
library(MuMIn)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
indexName <-  "mrfss_cpfv_dockside"
run_delta_glm <- FALSE
run_neg_binomial <- TRUE

# covariates you want to explore
mrfss_covars <- c("YEAR", "CNTY", "WAVE")

# CNTY for models with southern CA

# CA_CPFV_onboard_covars <- if (Model.number == 1) {
#   c("YEAR", "SubRegion", "WAVE", "DEPTH_bin")
# } else {
#   c("YEAR", "DISTRICT", "WAVE", "DEPTH_bin")
# }
# 
# DebWV_onboard_covars <- c("YEAR", "MegaReef", "WAVE", "DEPTH_bin")

crfs_pr_dockside_covars <-  c("YEAR", "DISTRICT", "WAVE", "PRIMARY_TARGET_SPECIES_NAME") 

# CCFRP_covars <- c("YEAR", "AREA", "SITE", "DEPTH_bin")

covars <- mrfss_covars #crfs_pr_dockside_covars #get(index.vars[6])

nb.covars <- c(covars, "offset(log(Effort))")
#-------------------------------------------------------------------------------
# Set working directories
dir <- file.path(here(),"data", "rec_indices",indexName, modelArea)
setwd(dir)
# create output directory for each model
out.dir <- getwd()
plots.dir <- glue(getwd(),"/plots")

# load data
load(glue(getwd(),"/",indexName,"_data_for_GLM.RData"
))

#-------------------------------------------------------------------------------
#select what you want as the positives, either KEPT or total as NUMENC
#rename effort

if (indexName == "mrfss_cpfv_dockside") {
  dat <- area.dat.sm %>%
    rename(Effort = ANGLERxHRS) 
  } else if (indexName == "debwv_cpfv_onboard") {
  dat <- dat %>%
    rename(Effort = ANGHRS) %>%
    rename(Target = KEPT) %>%
    mutate(SubRegion = MegaReef) 
} else if(indexName == "crfs_cpfv_onboard"){
  dat <- dat %>%
    mutate(SubRegion = MegaReef) 
} else if(indexName == "crfs_pr_dockside"){
  dat <- prTrips %>%
    rename(Effort = NUMBER_OF_ANGLERS) %>%
    rename(Target = targetKeptObs) %>%
    rename(YEAR = RECFIN_YEAR) %>%
    rename(DISTRICT = RECFIN_PORT_CODE)
}

#-------------------------------------------------------------------------------
# mutate columns
dat <- dat %>%
  mutate(
    Target_cpue = Target / Effort,
    Targetbin = as.numeric(Target > 0),# create 0/1 column for binomial
    logTarget_cpue = ifelse(Target_cpue > 0, log(Target_cpue), NA)
  ) %>%
  mutate_at(covars, as.factor) # make sure covariates are factors

#-------------------------------------------------------------------------------
# response names for lognormal, gamma, binomial, negative binomial
response.name <- c("logTarget_cpue", "Target_cpue", "Targetbin", "Target")

#-------------------------------------------------------------------------------
# Start models
# What's the percent positive in the raw data
Percent_pos <- as.data.frame(round(with(subset(dat, Targetbin == 1), table(YEAR)) /
                                     with(dat, table(YEAR)), 2))
Percent_pos
#-------------------------------------------------------------------------------
# Negative binomial model selection
#if (run_neg_binomial == TRUE) {
#full model
model.full <- MASS::glm.nb(as.formula(
  paste(response.name[4], 
        paste(nb.covars, collapse = " + "), 
        sep = " ~ ")),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)

#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                      rank = "AICc", 
                      fixed= c("offset(log(Effort))", "YEAR"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- get.models(model.suite,subset = delta == 0)
#have to change manually
best.formula <- best.model$`2`$call$formula


#save image
save.image(paste0(
  out.dir, "/", indexName , "_", speciesName,
  "_", modelArea, "_modelselection.RData"
))

#}

if(run_delta_glm == TRUE){
#-------------------------------------------------------------------------------
# Start delta model selection
#-------------------------------------------------------------------------------
# binomial
# Models to fit and model selection
  #full model
  Bmodel.full <- glm(as.formula(
    paste(response.name[3], paste(covars, collapse = " + "), sep = " ~ ")),
    data = dat,
    family = binomial,
    na.action = "na.fail")
  
  #MuMIn will fit all models and then rank them by AICc
  Bmodel.suite <- dredge(Bmodel.full,
                        rank = "AICc", 
                        fixed= c("YEAR"))
  
  #Create model selection dataframe for the document
  Model_selection <- as.data.frame(Bmodel.suite)
  Model_selection
  #pull out the best model
  best.model <- get.models(model.suite,subset = delta == 0)
  best.formula <- best.model$`8`$call$formula

#-------------------------------------------------------------------------------
# Model selection
# Look at lognormal and gamma models
posdata <- dat %>%
    filter(Target > 0)
  
logn.full <- tryCatch(glm(as.formula(
  paste(response.name[1], 
        paste(covars, collapse = " + "),sep = " ~ ")),
  data = subset(dat, Target>0),
  family = gaussian
),
error = function(e) NA
)
if (length(logn.full) > 1) {
  summary(logn.full)
  anova(logn.full, test = "Chisq")
} else {
  rm(logn.full)
  logn.full <- as.data.frame(0)
  logn.full$aic <- 99999
}

# gamma model
gamma.full <- tryCatch(glm(as.formula(
  paste(response.name[2], paste(covars, collapse = " + "), sep = " ~ ")),
  data = subset(dat, Target>0),
  family = Gamma(link = "log")
),
error = function(e) NA
)
 if (length(gamma.full) > 1) {
  summary(gamma.full)
  anova(gamma.full, test = "Chisq")
} else {
  rm(gamma.full)
  gamma.full <- as.data.frame(0)
  gamma.full$aic <- 99999
}

# choose lognormal or gamma based on AIC
if (gamma.full$aic < 90000 & logn.full$aic < 90000) {
  
  logn_or_gamma_aic <- c(logn.full$aic, gamma.full$aic)
  
  pos.mod.dist <- ifelse(logn_or_gamma_aic[1] < logn_or_gamma_aic[2],
                         "Lognormal", "Gamma")
  
} else if (gamma.full$aic == 99999) {
  pos.mod.dist <- "Lognormal"
  #stop("One of the models did not converge")
}


#Look at diagnostics of the positive data between lognormal and gamma
fitlog   <- fitdistrplus::fitdist(posdata$CPUE,"lnorm")
fitgamma <- fitdistrplus::fitdist(posdata$CPUE,"gamma")

#-------------------------
# positive lognormal
if (pos.mod.dist == "Lognormal") {
  # Models to fit and model selection
  #full model
  model.full <- glm(as.formula(
    paste(response.name[1], paste(covars, collapse = " + "), sep = " ~ ")),
    data = subset(dat, Target>0),
    family = gaussian,
    na.action = "na.fail")
  
  #MuMIn will fit all models and then rank them by AICc
  model.suite <- dredge(model.full,
                         rank = "AICc", 
                         fixed= c("YEAR"))
  
  #Create model selection dataframe for the document
  Model_selection <- as.data.frame(model.suite)
  Model_selection
  #pull out the best model
  best.model <- get.models(model.suite,subset = delta == 0)
  #best.formula <- best.model$`8`$call$formula
  
}
#--------------------------------------------------------------
# positive gamma

if (pos.mod.dist == "Gamma") {
  # Models to fit and model selection
  #full model
  model.full <- glm(as.formula(
    paste(response.name[2], paste(covars, collapse = " + "), sep = " ~ ")),
    data = posdata,
    family = Gamma(link = "log"),
    na.action = "na.fail")
  
  #MuMIn will fit all models and then rank them by AICc
  model.suite <- dredge(model.full,
                        rank = "AICc", 
                        fixed= c("YEAR"))
  
  #Create model selection dataframe for the document
  Model_selection <- as.data.frame(model.suite)
  Model_selection
  #pull out the best model
  best.model <- get.models(model.suite,subset = delta == 0)
  #best.formula <- best.model$`8`$call$formula
}
#--------------------------------------------
# save image
save.image(paste0(
  out.dir, "/", indexName , "_", speciesName,
  "_", modelArea, "_modelselection.RData"
))

}
