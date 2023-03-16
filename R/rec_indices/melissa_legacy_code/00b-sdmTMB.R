# Generalized negative binomial and delta glm code
# turn into a function
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

# Set working directories
dir<- file.path(here(),"data","rec_indices", indexName, modelArea)
setwd(dir)
# create output directory for each model
out.dir <- getwd()
plots.dir <- glue(getwd(),"/plots")


load(file.path(out.dir, glue(indexName , "_", speciesName,
  "_", modelArea, "_modelselection.RData"
)))

#pull out the best model
best.model <- get.models(model.suite,subset = delta == 0)
Model_selection #need to get model #
#have to change manually
best.formula <- best.model$`2`$call$formula

#create a grid
# year
# grid <- expand.grid(
#   YEAR = unique(dat$YEAR),
#   DISTRICT = levels(dat$DISTRICT)[1],
#   PRIMARY_TARGET_SPECIES_NAME = levels(dat$PRIMARY_TARGET_SPECIES_NAME)[1]
# )

#However, the part I am a bit unclear on is whether we should be using a grid 
#or not. If you run the above code but also pass the grid into the predict function 
#(newdata = grid) the estimates are slightly different which kinda makes sense, 
#but I am not entirely sure which we should be doing here. 

fit.nb <- sdmTMB(
  Target ~ CNTY + YEAR,
  data = dat,
  offset = log(dat$Effort),
  time = "YEAR",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log")
# silent = TRUE,
# do_index = TRUE,
# predict_args = list(newdata = grid, re_form_iid = NA),
#  index_args = list(area = 1)
)

pred <- predict(fit.nb, return_tmb_object = TRUE)
get_index(pred, bias_correct = TRUE)



#sdmTMB stan trial with crfs_pr_dockside
# fit.stan <- tmbstan::tmbstan(
#   fit$tmb_obj,
#   iter = 5000,
#   chains = 4,
#   seed = 8222
# )
# fit.stan
# plot(fit.stan)
# set.seed(19292)
# samps <- sdmTMBextra::extract_mcmc(fit.stan)
# s <- simulate(fit, mcmc_samples = samps, nsim = 50)
# bayesplot::pp_check(
#   # dat$Target,
#   yrep = t(s),
#   fun = bayesplot::ppc_dens_overlay
# )
# 
# predict.sdtmb <- predict(fit, newdata = )
# inb <- plotindex_bayes(Dnbin, yrvec,
#                        backtrans = "exp", standardize = F,
#                        title = "negative binomial"
# )
# fit_pred <- predict(fit, newdata = grid,
#                     mcmc_samples = samps) 
# grid$post_mean <- apply(fit_pred,1,mean)
# grid$post_mean_exp <- exp(grid$post_mean) 
# grid$sd <- apply(fit_pred,1, sd) 
# grid
# save(fit.stan, file = "stan_test.RData")
