# Generalized negative binomial and delta glm code
# turn into a function
# Melissa Monk June 2021; modified for copper 2023

rm(list = ls(all = TRUE))
graphics.off()
options(knitr.table.format = "latex")
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(tidyr)
library(dplyr)
library(ggplot2)
library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
library(tidybayes)
library(gridExtra)
library(fitdistrplus)
library(glue)
library(here)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "south"
indexName <-  "crfs_pr_dockside"


run_delta_glm <- TRUE

#-------------------------------------------------------------------------------
# Set working directories
setwd(glue(here(),"/data/rec_indices/",indexName,"/", modelArea, "/"))

# Source delta glm plotting functions
source(glue(here(),"/R/indices/Delta_bayes_functions.R"))

# create output directory for each model
out.dir <- getwd()
plots.dir <- glue(getwd(),"/plots")

# load data
load(paste0(
  out.dir, "/", indexName , "_", speciesName,
  "_", modelArea, "_deltaglmmodelselection.RData"
))

#force lognormal for test
pos.mod.dist = "Lognormal"
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# STAN runs
# run binomial stan model
# start AIC search at the second model because the first has no year effect
# as.formula(
#paste(response.name[3],
#paste(Model_selection$Model[which.min(binAIC$AIC[2:length(binAIC$AIC)]) + 1]),
#  sep = " ~ "
#)),

starttime <- Sys.time()
Dbin <- stan_glm(
Targetbin ~ YEAR + DISTRICT + PRIMARY_TARGET_SPECIES_NAME,
data = dat,
family = binomial(link = "logit"),
#prior = normal(0, 2),
#prior_intercept = normal(0, 3),
iter = 5000 #, QR = TRUE
)


if (pos.mod.dist == "Lognormal") {
  ## run lognormal stan
  starttime <- Sys.time()
  Dlno <- stan_lm(as.formula(paste(response.name[1],
    paste(Model_selection$Model[which.min(posAIC$AIC[2:length(posAIC$AIC)]) + 1]),
    sep = " ~ "
  )),
  data = posdata,
  prior = R2(0.5),
  iter = 5000
  )
  endtime <- Sys.time()
  endtime - starttime

  # Model checks
  # Run check on lognormal
  pp_check(Dlno, plotfun = "boxplot", nreps = 10, notch = FALSE) +
    ggtitle("lognormal model")
}


if (pos.mod.dist == "Gamma") {
  ## run gamma stan
  starttime <- Sys.time()
  Dgam <- stan_glm(as.formula(paste(response.name[2],
    paste(Model_selection$Model[which.min(posAIC$AIC[2:length(posAIC$AIC)]) + 1]),
    sep = " ~ "
  )),
  data = posdata,
  family = Gamma(link = "log"),
#  prior = normal(location = 0, scale = 3),
#  prior_intercept = normal(location = 0, scale = 3),
  iter = 5000#, QR = TRUE
  )
  endtime <- Sys.time()
  endtime - starttime

  # Run check on gamma
  pp_check(Dgam, plotfun = "boxplot", nreps = 10, notch = FALSE)
}
#-------------------------------------------------------------------------------
# Create index
yearvar <- "year"
yrvec <- as.numeric(levels(droplevels(dat$YEAR))) # years
yrvecin <- as.numeric(levels(droplevels(dat$YEAR))) # years

# binomial draws - logit so backtrasnform is exp(x)/(1+exp(x))
bin.draws <- as.data.frame(Dbin)
bin.yrs.logit <- cbind.data.frame(bin.draws[, 1], bin.draws[, 1] + bin.draws[, 2:length(yrvec)])
colnames(bin.yrs.logit)[1] <- paste0(yearvar, yrvec[1])
bin.yrs.prop <- exp(bin.yrs.logit) / (1 + exp(bin.yrs.logit))

# positive model draws

pos.draws <- if (pos.mod.dist == "Lognormal") {
  as.data.frame(Dlno)
} else {
  as.data.frame(Dgam)
}
pos.yrs.log <- cbind.data.frame(pos.draws[, 1], pos.draws[, 1] + pos.draws[, 2:length(yrvec)])
colnames(pos.yrs.log)[1] <- paste0(yearvar, yrvec[1])
pos.yrs <- exp(pos.yrs.log)

index.draws <- bin.yrs.prop * pos.yrs

# calculate the index and sd
# logSD goes into the model
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

# add raw standardized index to outdf
raw.cpue.year <- dat %>%
  group_by(YEAR) %>%
  summarise(avg_cpue = mean(CPUE)) %>%
  mutate(std.raw.cpue = avg_cpue / mean(avg_cpue))

outdf$stdzd.raw.cpue <- raw.cpue.year$std.raw.cpue
outdf$stdzd.Index <- outdf$Index / mean(outdf$Index)
# write csv
write.csv(outdf, paste0(
  out.dir, "/",indexName,"_", speciesName,"_",
  modelArea, "_delta",pos.mod.dist, "_Index.csv"
))





#-------------------------------------------------------------------------------
# Model checks on final model
ppDbin <- posterior_predict(Dbin, draws = 1000)
ppD <- ppDbin
for (i in 1:nrow(ppDbin)) {
  nd <- dat[ppDbin[i, ] == 1, ]
  ppnew <- if (pos.mod.dist == "Lognormal") {
    posterior_predict(Dlno, nd, fun = exp, draws = 1)
  } else {
    posterior_predict(Dgam, nd, draws = 1)
  }
  ppD[i, ppDbin[i, ] == 1] <- ppnew[1, ]
}

# Model checks
if (pos.mod.dist == "Lognormal") {
  # Run checks
  pp_check(Dlno, plotfun = "boxplot", nreps = 10, notch = FALSE)
  ppc_stat_2d(y = dat$CPUE, yrep = ppD, stat = c("mean", "sd")) + ggtitle("delta model")
}

prop_zero <- function(y) mean(y == 0)

figure.ppc.stat.grouped <- ppc_stat(y = dat$CPUE, 
                                    yrep = ppD, 
                                    stat = "prop_zero", 
                                    binwidth = 0.01)
ggsave(
  path = out.dir, filename = paste0("delta_pp_stat_grouped_", pos.mod.dist, ".png"),
  width = 4,
  height = 4,
  units = "in"
)

# mean by year
max_mean1 <- dat %>%
  group_by(YEAR) %>%
  summarise(max_mean = mean(CPUE))
max_mean <- max(max_mean1$max_mean)
figure.ppc.mean.by.year <- ppc_stat_grouped(
  y = dat$CPUE, yrep = ppD,
  group = dat$YEAR, binwidth = 0.05
) +
  coord_cartesian(xlim = c(0, max_mean * 1.8))
ggsave(
  path = out.dir, filename = paste0(
    "delta_model_pp_stat_mean_grouped_",
    pos.mod.dist, ".png"
  ),
  width = 4,
  height = 4,
  units = "in"
)

# standard deviation by year
max_sd1 <- dat %>%
  group_by(YEAR) %>%
  summarise(max_sd = max(sd(CPUE)))
max_sd <- max(max_sd1$max_sd)

figure.ppc.sd.by.year <- ppc_stat_grouped(
  y = dat$CPUE, yrep = ppD,
  group = dat$YEAR, stat = "sd",
  binwidth = 0.05
) + coord_cartesian(xlim = c(0, max_sd * 1.8))
ggsave(
  path = out.dir, filename = paste0(
    "delta_model_pp_stat_mean_grouped_",
    pos.mod.dist, ".png"
  ),
  width = 4,
  height = 4,
  units = "in"
)


model.type <- ifelse(run_delta_glm=="TRUE", "_deltaglm", "_negbinomial")

save.image(paste0(
  out.dir, "/", indexName , "_", speciesName,
  "_", modelArea, model.type,".RData"
))

