library(sdmTMB)
library(tmbstan)
library(sdmTMBextra)


#create a grid
# year and ditrict
grid <- expand.grid(
  YEAR = unique(dat$YEAR)
)
grid$DISTRICT <- as.factor(1)
grid$PRIMARY_TARGET_SPECIES_NAME <- "rockfish genus"

fit <- sdmTMB(
  Target ~  YEAR + DISTRICT + PRIMARY_TARGET_SPECIES_NAME,
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

get_index(fit, bias_correct = TRUE)

fit
nb.mle


fit.stan <- tmbstan::tmbstan(
  fit$tmb_obj,
  iter = 5000,
  chains = 4,
  seed = 8222
)
 fit.stan
 plot(fit.stan)
 
 set.seed(19292)
 samps <- sdmTMBextra::extract_mcmc(fit.stan)
 s <- simulate(fit, mcmc_samples = samps, nsim = 50)
 bayesplot::pp_check(
  # dat$Target,
   yrep = t(s),
   fun = bayesplot::ppc_dens_overlay
 )

 predict.sdtmb <- predict(fit, newdata = )
 inb <- plotindex_bayes(Dnbin, yrvec,
                        backtrans = "exp", standardize = F,
                        title = "negative binomial"
 )
 

 
 fit_pred <- predict(fit, newdata = grid,
                     mcmc_samples = samps) 
 
grid$post_mean <- apply(fit_pred,1,mean)
grid$post_mean_exp <- exp(grid$post_mean) 
grid$sd <- apply(fit_pred,1, sd) 
grid
# save(fit.stan, file = "stan_test.RData")
 