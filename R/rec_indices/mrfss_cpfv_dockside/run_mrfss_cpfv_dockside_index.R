# Run the MRFSS cpfv dockside index
# Melissa Monk June 2021; modified for copper 2023

rm(list = ls(all = TRUE))
graphics.off()

library(sdmTMB)
library(tmbstan)
library(sdmTMBextra)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
indexName <-  "mrfss_cpfv_dockside"
covars <- c("YEAR", "CNTY", "WAVE")

# Load in some helper functions for processing and plotting the data
all <- list.files(file.path(here(), "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(here(), "R", "sdmTMB", all[a]))}

# Set working directories
dir <- file.path(here(),"data","rec_indices", indexName, modelArea)
setwd(dir)
# create output directory for each model
out.dir <- getwd()
plots.dir <- glue(getwd(),"/plots")

# load data
load(glue(getwd(),"/",indexName,"_data_for_GLM.RData"
))

#Ensure columns named appropriately and covariates are factors
dat <- area.dat.sm %>%
  rename(Effort = ANGLERxHRS) %>%
  mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor) # make sure covariates are factors

#Model selection
#full model
model.full <- MASS::glm.nb(
  Target ~ YEAR + CNTY + WAVE + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
#use ggpredict to get an estimate of the logEffort for sdmTMB predictions
ggpredict(model.full, terms = "YEAR")
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
best.formula <- best.model$`2`$call$formula
#pull out the best model
best.model <- get.models(model.suite,subset = delta == 0)
#have to change manually
best.formula <- best.model$`2`$call$formula

#set the grid
grid <- expand.grid(
  YEAR = unique(dat$YEAR),
  CNTY = levels(dat$CNTY)[1],
  logEffort = log(3.23)
)

fit.nb <- sdmTMB(
  Target ~ YEAR + CNTY,
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

do_diagnostics(
  dir = plots.dir, 
  fit = fit.nb)

get_diag_tables(fit.nb, plots.dir)
plot_qq_sdm(fit.nb, plots.dir)
plot_residuals(fit.nb, plots.dir) #doesn't work
plot_fixed_effects_para(fit.nb, plots.dir) #?
calc_index(fit.nb, dir = file.path(dir, "forSS"))
format_index(index, 
             dir = file.path(dir,"forSS"), month = 7, fleet = NA)

