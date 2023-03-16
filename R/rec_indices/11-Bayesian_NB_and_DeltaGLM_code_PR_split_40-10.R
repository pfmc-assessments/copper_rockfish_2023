# Generalized negative binomial and delta glm code
# turn into a function
# Melissa Monk June 2021

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


#CRFS_PR_dockside
 index.vars <- c(1, "vermilion", "VRML", "CRFS_PR_dockside", "CRFS_PR_dockside", "CRFS_PR_dockside_covars")

Model.number <- as.numeric(index.vars[1])
species.name <- index.vars[2]
assess.folder <- index.vars[3]
index.subfolder <- index.vars[4]
survey.name <- index.vars[5]
#-------------------------------------------------------------------------------
Model_region <- c("NCA")

# Set working directories
setwd(paste0(
  "C:/Stock_Assessments/", assess.folder, "_Assessment_2021/Indices_of_Abundance/",
  index.subfolder, "/"
))
# Source delta glm plotting functions
source("../Delta_bayes_functions.R")

# create output directory for each model
dir.path <- paste0(
  getwd(), "/", Model_region[Model.number], "/"
)
source("../dir_recent.R")
model.dir <- dir_recent(dir = dir.path, pattern = "2021")
out.dir <- paste0("C:/Stock_Assessments/", assess.folder, "_Assessment_2021/Indices_of_Abundance/CRFS_PR_management_split")
# load data
load(paste0(
  model.dir, "/Filtered_data_", index.subfolder, "_", species.name, "_",
  Model_region[Model.number], ".RData"
))
setwd(paste0("C:/Stock_Assessments/", assess.folder, "_Assessment_2021/Indices_of_Abundance/CRFS_PR_management_split")
)

#-------------------------------------------------------------------------------
# select what you want as the positives, either KEPT or total as NUMENC
#-------------------------------------------------------------------------------
#keep only 2016-2019 - exclude 2020 due to covid
dat <- dat %>% filter(YEAR %in% c(2016:2019)) %>%
    droplevels

# mutate columns
dat <- dat %>%
  mutate(
    CPUE = Target / Effort,
    Targetbin = as.numeric(Target > 0)) %>% # create 0/1 column for binomial
  mutate(across(c(DISTRICT, AREA_X), as.factor)) %>% # make sure covariates are factors
  droplevels() 

#-------------------------------------------------------------------------------
# response names for lognormal, gamma, binomial, negative binomial
response.name <- c("logTargetcpue", "Target_cpue", "Targetbin", "Target")
# create dataframe of just positives and add logcpue

#-------------------------------------------------------------------------------
# Negative binomial
### Negative binomial model
# set up the models you want to explore
# these will be the same for the binomial and positive model
# First model will just be an intercept only model
# main effects only right now
  nb1 <- as.formula(Target ~ 1 + offset(log(Effort)))
  nb2 <- as.formula(Target ~ DISTRICT + offset(log(Effort)))
#  nb3 <- as.formula(Target ~ DISTRICT + AREA_X + offset(log(Effort)))
 
  nbin.mod1 <- MASS::glm.nb(nb1, data = dat)
  summary(nbin.mod1)
  nbin.mod2 <- MASS::glm.nb(nb2, data = dat)
  summary(nbin.mod2)
  # nbin.mod3 <- MASS::glm.nb(nb3, data = dat)
  # summary(nbin.mod3)
  # AIC
  nbinAIC <- AIC(
    nbin.mod1, nbin.mod2
  )
  nbinAIC
  ## Fit the main effects model in STAN ans save workspace
  start.time <- Sys.time()
  # use STAN to see how well 'best model' fits the data
Dnbin_both_areax <- stan_glm.nb(Target ~ DISTRICT,
  data = dat,
  offset = log(Effort),
  prior_intercept = normal(location = 0, scale = 10),
  prior = normal(location = 0, scale = 10),
  prior_aux = cauchy(0, 5),
  chains = 4,
  iter = 5000
  ) # iterations per chain
  Sys.time() - start.time

 #get values
  var <- "DISTRICT"
  dvec <- as.numeric(levels(droplevels(dat$DISTRICT))) # years
  dvecin <- as.numeric(levels(droplevels(dat$DISTRICT))) # years
# Create index
  ppnb <- posterior_predict( Dnbin_both_areax, draws = 1000)
#draws
  nbin.draws <- as.data.frame(Dnbin_both_areax)
  nbin.districts <- cbind.data.frame(nbin.draws[, 1], nbin.draws[, 1] + nbin.draws[, 2:length(dvec)])
  colnames(nbin.districts)[1] <- paste0(var, dvec[1])
  district.draws <- exp(nbin.districts)
# calculate the index and sd
# logSD goes into the model
  Dist.cpue <- apply(district.draws, 2, mean) # mean(x)
  SD.Dist.cpue <- apply(district.draws, 2, sd) # sd(x)
  int95 <- apply(district.draws, 2, quantile, probs = c(0.025, 0.975))
  outdf <- cbind.data.frame(District = dvec, Dist.cpue, SD.Dist.cpue, t(int95))
  outdf$logSD <- apply(district.draws, 2, function(x) {
    sd(log(x))
  })

## pp_check
  prop_zero <- function(y) mean(y == 0)
# figure of proportion zero
  figure_Dnbin_prop_zero_both_areas <- pp_check(Dnbin_both_areax, plotfun = "stat", stat = "prop_zero", binwidth = 0.01)

# Marginal effects and cpue - need to install package sjPlot
# Dnbin.ggpredict.both_areas <- ggeffects::ggpredict(Dnbin_both_areax, "DISTRICT", ppd = FALSE) 
# both_plot <-   plot(Dnbin.ggpredict.both_areas,
#       rawdata = FALSE,
#       show.title = FALSE,
#       show.legend = FALSE) +
#       theme_bw() +
#       theme(text = element_text(size = 8)) +
#       theme(axis.title.y = element_blank())

#----------------------------------------------------------------------------------
###Now leave out AREA_X=2 
# main effects only right now
# nb1 <- as.formula(Target ~ 1 + offset(log(Effort)))
# nb2 <- as.formula(Target ~ DISTRICT + offset(log(Effort)))
# #  nb3 <- as.formula(Target ~ DISTRICT + AREA_X + offset(log(Effort)))

nbin.mod1a <- MASS::glm.nb(nb1, data = subset(dat, AREA_X==1))
summary(nbin.mod1a)
nbin.mod2a <- MASS::glm.nb(nb2, data = subset(dat, AREA_X==1))
summary(nbin.mod2a)

# AIC
nbinAICa <- AIC(
  nbin.mod1a, nbin.mod2a
)
nbinAICa
## Fit the main effects model in STAN ans save workspace
start.time <- Sys.time()
# use STAN to see how well 'best model' fits the data
Dnbin_area1 <- stan_glm.nb(Target ~ DISTRICT,
                                data = subset(dat, AREA_X==1),
                                offset = log(Effort),
                                prior_intercept = normal(location = 0, scale = 10),
                                prior = normal(location = 0, scale = 10),
                                prior_aux = cauchy(0, 5),
                                chains = 4,
                                iter = 5000
) # iterations per chain
Sys.time() - start.time

#get values
var <- "DISTRICT"
dvec <- as.numeric(levels(droplevels(dat$DISTRICT))) # years
dvecin <- as.numeric(levels(droplevels(dat$DISTRICT))) # years
 #Create values
ppnba <- posterior_predict( Dnbin_area1, draws = 1000)
#draws
nbin.drawsa <- as.data.frame(Dnbin_area1)
nbin.districtsa <- cbind.data.frame(nbin.drawsa[, 1], nbin.drawsa[, 1] + nbin.drawsa[, 2:length(dvec)])
colnames(nbin.districtsa)[1] <- paste0(var, dvec[1])
district.drawsa <- exp(nbin.districtsa)
# calculate the index and sd
# logSD goes into the model
Dist.cpue1 <- apply(district.drawsa, 2, mean) # mean(x)
SD.Dist.cpue1 <- apply(district.drawsa, 2, sd) # sd(x)
int951 <- apply(district.drawsa, 2, quantile, probs = c(0.025, 0.975))
outdf$Dist.cpue.areax1 <- Dist.cpue1
outdf$logSD.areax1 <- apply(district.drawsa, 2, function(x) {
  sd(log(x))
})

## pp_check
prop_zero1 <- function(y) mean(y == 0)
# figure of proportion zero
figure_Dnbin_prop_zero_area1 <- pp_check(Dnbin_area1, plotfun = "stat", stat = "prop_zero", binwidth = 0.01)

# Marginal effects and cpue - need to install package sjPlot
Dnbin.ggpredict.area1<- ggeffects::ggpredict(Dnbin_area1, "DISTRICT", ppd = FALSE) %>%
  plot(Dnbin.ggpredict,
       rawdata = FALSE,
       show.title = FALSE,
       show.legend = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 8)) +
  theme(axis.title.y = element_blank()) 
 

# add raw standardized index to outdf
raw.cpue.district_both <- dat %>%
  group_by(DISTRICT) %>%
  summarise(avg_cpue = mean(CPUE))
raw.cpue.district_area1 <- dat %>%
  filter(AREA_X==1) %>%
  group_by(DISTRICT) %>%
  summarise(avg_cpue_areax1 = mean(CPUE))

outdf$raw.both <- raw.cpue.district_both$avg_cpue
outdf$raw.areax1 <- raw.cpue.district_area1$avg_cpue_areax1

save.image("PR_management_split.RData")


write.csv(outdf,"outdf.csv")
