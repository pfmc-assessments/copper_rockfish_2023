# pak::pkg_install("pfmc-assessments/PEPtools")

library(PEPtools)
library(r4ss)
library(shinystan)

# Specify the directory
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

model_dir <- file.path(user_dir, "models", "nca", "_sensitivities", "9.0_update_deb_index_selex")


#run_regularization(
  dir_wd = model_dir#,
  model = "ss"#,
  extension = ".exe"#,
  interactive = TRUE#,
  verbose = TRUE
#)

wd <- getwd()
on.exit(setwd(wd), add = TRUE)
stopifnot(file.exists(dir_wd))
setwd(dir_wd)

# Run the model if need be
if (!file.exists("Report.sso")) {
  system(paste(model, "-nohess"))
}

# Regularization
p <- "_mcmc"
dir.create(p, showWarnings = FALSE)
dir.create(file.path(p, "fits"))
r4ss::copy_SS_inputs(
  dir.old = getwd(),
  dir.new = p,
  overwrite = TRUE,
  verbose = verbose
)
file.copy(
  paste0(model, extension),
  file.path(p, paste0(model, extension))
)
# optimize w/ -mcmc flag b/c of bias adjustment.
setwd(p)
system(paste(model, "-nox -mcmc 100"))
setwd("..")

# Now test it works in parallel
fit <- adnuts::sample_rwm(
  model = model,
  path = p,
  iter = 2000,
  chains = 2
)
# This thin rate will lead to run time of ~60 mins below
thin60min <- floor((60 * 60) / mean(fit$time.total))
# ------------------------------------------------------------
# Task 1: Run and demonstrate MCMC convergence diagnostics.
chains <- parallel::detectCores() - 3
# I recommend using 1000-2000 iterations, with first 10-25%
# warmup. Start with thin=1, then increase thin rate until
# convergence diagnostics passed (ESS>200 & Rhat<1.1).
# printed to screen live!!
thin <- thin60min # change this as needed
iter <- 2000 * thin
# Duration argument will stop after 40 minutes, only used
# for the workshop to keep things organized
fit <- adnuts::sample_rwm(
  model = model, path = p, iter = iter, warmup = iter * 0.25,
  chains = chains, thin = thin, duration = 60
)
# Good idea to save the output, I recommend RDS format.
saveRDS(fit, file = file.path(p, "fits", "mcmc.RDS"))
# Marginal comparisons as multipage PDF for easy scrolling
grDevices::pdf(
  file.path(p, "fits", "marginals.pdf"),
  onefile = TRUE, width = 7, height = 5
)
adnuts::plot_marginals(fit)
grDevices::dev.off()
# fit <- readRDS(file=file.path(p, "fits", "mcmc.RDS"))

# Key information from run. Including the two recommended
# convergence diagnostics:
if (verbose) summary(fit)

# Interactive tools (must close out browser to regain console)
if (interactive) adnuts::launch_shinyadmb(fit)

# Extract posterior samples as a data.frame
# ?extract_samples
post <- adnuts::extract_samples(fit)

# If more thinning is needed, increase and rerun, repeating.

# ------------------------------------------------------------
# Task 2: Model diagnostics using failed convergence
# diagnostics. When the MLE and MCMC estimate completely
# different things that is usually a parameterization issue.

# Read in a longer previous run or use yours
output_r4ss <- r4ss::SS_output(dir = p, covar = FALSE)
names_good <- cbind(
  fit$par_names[-NROW(fit$par_names)],
  output_r4ss$parameters$Label[output_r4ss$parameters$Phase >= 0]
)
# The 6 slowest/fastest mixing parameters
grDevices::png(file.path(p, "fits", "regularization-pairs-chains-slow6.png"))
adnuts::pairs_admb(fit, pars = 1:6, order = "slow")
grDevices::dev.off()
grDevices::png(file.path(p, "fits", "regularization-pairs-hists-slow6.png"))
adnuts::pairs_admb(fit, pars = 1:6, order = "slow", diag = "hist")
grDevices::dev.off()
grDevices::png(file.path(p, "fits", "regularization-pairs-hists-slow12.png"))
adnuts::pairs_admb(fit, pars = 7:12, order = "slow", diag = "hist")
grDevices::dev.off()
grDevices::png(file.path(p, "fits", "regularization-pairs-hists-slow18.png"))
adnuts::pairs_admb(fit, pars = 13:18, order = "slow", diag = "hist")
grDevices::dev.off()
grDevices::png(file.path(p, "fits", "regularization-pairs-hists-fast6.png"))
adnuts::pairs_admb(fit, pars=1:6, order='fast')
grDevices::dev.off()
# Can also specify names or use grep
# pairs_admb(fit, pars=c('recdev_early[21]','recdev_early[22]',
#                        'recdev_early[23]'))
# pairs_admb(fit, pars=grep('_parm', fit$par_names))

# Marginal MLE vs posterior
# adnuts::plot_marginals(fit, pars=1:15)
grDevices::png(
  file.path(p, "fits", "regularization-uncertainty-mle-v-mcmc.png")
)
x <- adnuts::plot_uncertainties(fit)
grDevices::dev.off()
grDevices::png(file.path(p, "fits", "regularization-marginals-worst-sd.png"))
adnuts::plot_marginals(fit, pars = which.max(x$sd.post))
grDevices::dev.off()

grDevices::png(file.path(p, "fits", "regularization-marginals-best-sd.png"))
adnuts::plot_marginals(fit, pars = which.min(x$sd.post))
grDevices::dev.off()

# ------------------------------------------------------------
# Task 3: Posterior extraction for inference. mceval can be run
# from command line b/c post-warmup samples from all chains
# were merged into main folder, so any mceval output files
# contain all this information
)
