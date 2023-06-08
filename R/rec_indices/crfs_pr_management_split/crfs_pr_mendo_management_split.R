###############################################################################
## R script to determine the management split at 40-10 for copper rockfish
## based upon the CRFS PR data from 2016-2019 and the availalbe habitat data
## 
##   Melissa Monk 6/7/2023
#################################################################################
library(sdmTMB)
library(tmbstan)
library(ggeffects)
library(here)
library(glue)
library(tidyr)
library(dplyr)
library(rstanarm)
options(mc.cores = parallel::detectCores())
library(ggplot2)
library(bayesplot)
library(grid)
library(devtools)
library(ggeffects)
library(tidybayes)

#set the working directory
setwd("S:/copper_rockfish_2023/data/rec_indices/crfs_pr_dockside/north")
outdir <- "S:/copper_rockfish_2023/data/rec_indices/crfs_pr_management_split"

load("data_for_glm.RData")

#GEMM <- readxl::read_xlsx("GEMM_report_copy.xlsx")
#CALCOM <- readxl::read_xlsx("CALCOM Commercial Landings VRML.xlsx")
#CRFS <- readxl::read_xlsx("CRFS_landings.xlsx")
#-------------------------------------------------------------------------------
# select what you want as the positives, either KEPT or total as NUMENC
#-------------------------------------------------------------------------------
#keep only 2016-2019 - exclude 2020 due to covid
dat <- cdfwpr %>% 
filter(year %in% c(2016:2019)) %>%
rename(Effort = anglers) %>%
mutate(logEffort = log(Effort)) %>%
droplevels

# mutate columns
dat <- dat %>%
  mutate(across(c(district), as.factor)) %>% # make sure covariates are factors
  droplevels() 

# Negative binomial
### Negative binomial model
# set up the models you want to explore
# these will be the same for the binomial and positive model
# First model will just be an intercept only model
# main effects only right now
nb1 <- as.formula(kept ~ 1 + offset(logEffort))
nb2 <- as.formula(kept ~ district + offset(logEffort))

 
nbin.mod1 <- MASS::glm.nb(nb1, data = dat)
summary(nbin.mod1)
nbin.mod2 <- MASS::glm.nb(nb2, data = dat)
summary(nbin.mod2)

# AIC
nbinAIC <- AIC(nbin.mod1, nbin.mod2)
nbinAIC
AIC(nbin.mod2) -AIC(nbin.mod1)

## Fit the main effects model in STAN ans save workspace
start.time <- Sys.time()
  # use STAN to see how well 'best model' fits the data
Dnbin <- stan_glm.nb(kept ~ district,
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
  var <- "district"
  dvec <- as.numeric(levels(droplevels(dat$district))) # years
  dvecin <- as.numeric(levels(droplevels(dat$district))) # years
# Create index
ppnb <- posterior_predict( Dnbin, draws = 1000)
#draws
nbin.draws <- as.data.frame(Dnbin)
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

write.csv(outdf,file.path(outdir,"outdf.csv"))


##Sample size by year and region and sample size by region with both areas
levels(dat$district) = c("Central","Bay","Wine","Redwood")
Trips_region <- dat %>%
  group_by(year, district) %>%
  tally()  %>%
  tidyr::pivot_wider(names_from=district, values_from=n)

#reef area by district
outdf$Area = c(272.7073719,
271.2794496,
136.9369742,
164.1927776
)

#percent of reef by district
outdf$Percent.area <- c(0.323,
0.321,
0.162,
0.194
)

#
CPUE_table <- outdf %>%
  mutate(district = c("Central (3)","Bay (4)","Wine (5)","Redwood (6)")) %>%
  rename(CPUE = Dist.cpue, `CRFS District`= district, `Percent of Area`= Percent.area) %>%
  select( `CRFS District`, CPUE, `Area (km2)`, `Percent of Area`) %>%
  mutate(CPUExAREA = CPUE*`Percent of Area`) %>%
  mutate_at(c(2,3,5), round,3) %>%
  mutate(`Percent of Area` = scales::percent(`Percent of Area`, accuracy=0.01)) %>%
  mutate(`Relative Abundance` = scales::percent(CPUExAREA/sum(CPUExAREA), accuracy = .01))

write.csv(CPUE_table, file.path(outdir,"cpue_by_district.csv"), row.names = F)
