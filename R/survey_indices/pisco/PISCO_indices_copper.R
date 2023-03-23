# Develop index of abundance for PISCO for copper assessment 2023
# Adapted from Melissa Monk's script for index of abundance for PISCO for the gby assessment in 2019

#################### Set up: ####################
##### Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

##### Load packages
library(tidyverse)  # includes dplyr, tidyr, ggplot2, stringr
library(cowplot)  # for multipanel figures
library(here)  # useful for providing directory paths - it sets the base at the folder level (usually - where a RProject or Git repo or other identifiers are)

##### Load data (data cleaned and processed in PISCO_data_filtering_copper.R)
# Data for northern index
PISCO.CPR.all.UCSC = readRDS(file=here("Outputs/Copper","PISCO.CPR.all.UCSC.RDS"))  # UCSC sites, transects for all sites that have seen copper at least once regardless of sampling frequency or habitat quality
PISCO.CPR.25.UCSC = readRDS(file=here("Outputs/Copper","PISCO.CPR.25.UCSC.RDS"))  # UCSC sites, transects for sites that were both sampled and saw copper at least a quarter of UCSC sampling years 
PISCO.CPR.50.UCSC = readRDS(file=here("Outputs/Copper","PISCO.CPR.50.UCSC.RDS"))  # UCSC sites, transects for sites that were both sampled and saw copper at least half of UCSB sampling years

# Data for southern index
PISCO.CPR.all.UCSB = readRDS(file=here("Outputs/Copper","PISCO.CPR.all.UCSB.RDS"))  # UCSB sites, transects for all sites that have seen copper at least once regardless of sampling frequency or habitat quality
PISCO.CPR.25.UCSB = readRDS(file=here("Outputs/Copper","PISCO.CPR.25.UCSB.RDS"))  # UCSB sites, transects for sites that were both sampled and saw copper at least a quarter of UCSB sampling years
PISCO.CPR.50.UCSB = readRDS(file=here("Outputs/Copper","PISCO.CPR.50.UCSB.RDS"))  # UCSC sites, transects for sites that were both sampled and saw copper at least half of UCSB sampling years

##### Set any constants
site_filtering_color_scheme = c("#fde0dd", "#fa9fb5", "#c51b8a")


#################### Prep data: ####################
##### Pull out transects that were positive for copper (transects that sighted copper)
# Northern index
CPR.pos.all.UCSC = subset(PISCO.CPR.all.UCSC, SCAU>0)
CPR.pos.25.UCSC = subset(PISCO.CPR.25.UCSC, SCAU>0)
CPR.pos.50.UCSC = subset(PISCO.CPR.50.UCSC, SCAU>0)

# Southern index
CPR.pos.all.UCSB = subset(PISCO.CPR.all.UCSB, SCAU>0)
CPR.pos.25.UCSB = subset(PISCO.CPR.25.UCSB, SCAU>0)
CPR.pos.50.UCSB = subset(PISCO.CPR.50.UCSB, SCAU>0)

##### Sum across transects to get effort as number of transects
# Northern index
PISCO.aggregate.transect.all.UCSC = PISCO.CPR.all.UCSC %>%
  group_by(campus, site, year, month, day, zone) %>%
  summarize(CPRtot = sum(SCAU),
            ntransect = n_distinct(transect))

PISCO.aggregate.transect.25.UCSC = PISCO.CPR.25.UCSC %>%
  group_by(campus, site, year, month, day, zone) %>%
  summarize(CPRtot = sum(SCAU),
            ntransect = n_distinct(transect))

PISCO.aggregate.transect.50.UCSC = PISCO.CPR.50.UCSC %>%
  group_by(campus, site, year, month, day, zone) %>%
  summarize(CPRtot = sum(SCAU),
            ntransect = n_distinct(transect))

# Southern index
PISCO.aggregate.transect.all.UCSB = PISCO.CPR.all.UCSB %>%
  group_by(campus, site, year, month, day, zone) %>%
  summarize(CPRtot = sum(SCAU),
            ntransect = n_distinct(transect))

PISCO.aggregate.transect.25.UCSB = PISCO.CPR.25.UCSB %>%
  group_by(campus, site, year, month, day, zone) %>%
  summarize(CPRtot = sum(SCAU),
            ntransect = n_distinct(transect))

PISCO.aggregate.transect.50.UCSB = PISCO.CPR.50.UCSB %>%
  group_by(campus, site, year, month, day, zone) %>%
  summarize(CPRtot = sum(SCAU),
            ntransect = n_distinct(transect))

##### Turn year and month to factors
# Northern index
PISCO.aggregate.transect.all.UCSC$year = as.factor(PISCO.aggregate.transect.all.UCSC$year)
PISCO.aggregate.transect.all.UCSC$month = as.factor(PISCO.aggregate.transect.all.UCSC$month)

PISCO.aggregate.transect.25.UCSC$year = as.factor(PISCO.aggregate.transect.25.UCSC$year)
PISCO.aggregate.transect.25.UCSC$month = as.factor(PISCO.aggregate.transect.25.UCSC$month)

PISCO.aggregate.transect.50.UCSC$year = as.factor(PISCO.aggregate.transect.50.UCSC$year)
PISCO.aggregate.transect.50.UCSC$month = as.factor(PISCO.aggregate.transect.50.UCSC$month)

# Southern index
PISCO.aggregate.transect.all.UCSB$year = as.factor(PISCO.aggregate.transect.all.UCSB$year)
PISCO.aggregate.transect.all.UCSB$month = as.factor(PISCO.aggregate.transect.all.UCSB$month)

PISCO.aggregate.transect.25.UCSB$year = as.factor(PISCO.aggregate.transect.25.UCSB$year)
PISCO.aggregate.transect.25.UCSB$month = as.factor(PISCO.aggregate.transect.25.UCSB$month)

PISCO.aggregate.transect.50.UCSB$year = as.factor(PISCO.aggregate.transect.50.UCSB$year)
PISCO.aggregate.transect.50.UCSB$month = as.factor(PISCO.aggregate.transect.50.UCSB$month)


#################### Fit models: ####################

##### Try a basic negative binomial
library(MASS)

##### Northern assessment

##### All UCSC sites that saw copper
# Model 1: No factors
PISCO.all.UCSC.m1 = glm.nb(CPRtot ~ 1 + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSC)
summary(PISCO.all.UCSC.m1)

# Model 2: year
PISCO.all.UCSC.m2 = glm.nb(CPRtot ~ year + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSC)
summary(PISCO.all.UCSC.m2)
anova(PISCO.all.UCSC.m2, test="Chi")

# Model 3: year and month significant
PISCO.all.UCSC.m3 = glm.nb(CPRtot ~ year + month + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSC)
summary(PISCO.all.UCSC.m3)
anova(PISCO.all.UCSC.m3, test="Chi")

# Model 4: year, month and site - all significant
PISCO.all.UCSC.m4 = glm.nb(CPRtot ~ year + month + site + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSC)
summary(PISCO.all.UCSC.m4)
anova(PISCO.all.UCSC.m4, test="Chi")

# Model 5: year, month and site and zone - all significant
PISCO.all.UCSC.m5 = glm.nb(CPRtot ~ year + month + site + zone + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSC)
summary(PISCO.all.UCSC.m5)
anova(PISCO.all.UCSC.m5, test="Chi")

# AIC - same ordering of models as GBY
AIC(PISCO.all.UCSC.m1, PISCO.all.UCSC.m2, PISCO.all.UCSC.m3, PISCO.all.UCSC.m4, PISCO.all.UCSC.m5)


##### UCSC sites with 25% cutoff for frequency of sampling and years seen copper
# Model 1: No factors
PISCO.25.UCSC.m1 = glm.nb(CPRtot ~ 1 + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSC)
summary(PISCO.25.UCSC.m1)

# Model 2: year
PISCO.25.UCSC.m2 = glm.nb(CPRtot ~ year + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSC)
summary(PISCO.25.UCSC.m2)
anova(PISCO.25.UCSC.m2, test="Chi")

# Model 3: year and month - year significant
PISCO.25.UCSC.m3 = glm.nb(CPRtot ~ year + month + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSC)
summary(PISCO.25.UCSC.m3)
anova(PISCO.25.UCSC.m3, test="Chi")

# Model 4: year, month and site - year and month significant
PISCO.25.UCSC.m4 = glm.nb(CPRtot ~ year + month + site + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSC)
summary(PISCO.25.UCSC.m4)
anova(PISCO.25.UCSC.m4, test="Chi")

# Model 5: year, month and site and zone - year, site, zone significant
PISCO.25.UCSC.m5 = glm.nb(CPRtot ~ year + month + site + zone + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSC)
summary(PISCO.25.UCSC.m5)
anova(PISCO.25.UCSC.m5, test="Chi")

# AIC - same ordering of models as all.UCSC ones
AIC(PISCO.25.UCSC.m1, PISCO.25.UCSC.m2, PISCO.25.UCSC.m3, PISCO.25.UCSC.m4, PISCO.25.UCSC.m5)


##### UCSC sites with 50% cutoff for frequency of sampling and years seen copper
# Model 1: No factors
PISCO.50.UCSC.m1 = glm.nb(CPRtot ~ 1 + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSC)
summary(PISCO.50.UCSC.m1)

# Model 2: year
PISCO.50.UCSC.m2 = glm.nb(CPRtot ~ year + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSC)
summary(PISCO.50.UCSC.m2)
anova(PISCO.50.UCSC.m2, test="Chi")

# Model 3: year and month - both significant, month more so
PISCO.50.UCSC.m3 = glm.nb(CPRtot ~ year + month + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSC)
summary(PISCO.50.UCSC.m3)
anova(PISCO.50.UCSC.m3, test="Chi")

# Model 4: year, month and site - all significant
PISCO.50.UCSC.m4 = glm.nb(CPRtot ~ year + month + site + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSC)
summary(PISCO.50.UCSC.m4)
anova(PISCO.50.UCSC.m4, test="Chi")

# Model 5: year, month and site and zone - all significant
PISCO.50.UCSC.m5 = glm.nb(CPRtot ~ year + month + site + zone + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSC)
summary(PISCO.50.UCSC.m5)
anova(PISCO.50.UCSC.m5, test="Chi")

# AIC - same ordering of models as other two copper data sets
AIC(PISCO.50.UCSC.m1, PISCO.50.UCSC.m2, PISCO.50.UCSC.m3, PISCO.50.UCSC.m4, PISCO.50.UCSC.m5)


##### Southern assessment

##### All UCSB sites that saw copper
# Model 1: No factors
PISCO.all.UCSB.m1 = glm.nb(CPRtot ~ 1 + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSB)
summary(PISCO.all.UCSB.m1)

# Model 2: year
PISCO.all.UCSB.m2 = glm.nb(CPRtot ~ year + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSB)
summary(PISCO.all.UCSB.m2)
anova(PISCO.all.UCSB.m2, test="Chi")

# Model 3: year and month - both significant, month more than year
PISCO.all.UCSB.m3 = glm.nb(CPRtot ~ year + month + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSB)
summary(PISCO.all.UCSB.m3)
anova(PISCO.all.UCSB.m3, test="Chi")

# Model 4: year, month and site - all significant
PISCO.all.UCSB.m4 = glm.nb(CPRtot ~ year + month + site + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSB)
summary(PISCO.all.UCSB.m4)
anova(PISCO.all.UCSB.m4, test="Chi")

# Model 5: year, month and site and zone - all significant
PISCO.all.UCSB.m5 = glm.nb(CPRtot ~ year + month + site + zone + offset(log(ntransect)), data = PISCO.aggregate.transect.all.UCSB)
summary(PISCO.all.UCSB.m5)
anova(PISCO.all.UCSB.m5, test="Chi")

# AIC - same ordering of models as GBY and UCSC CPR ones
AIC(PISCO.all.UCSB.m1, PISCO.all.UCSB.m2, PISCO.all.UCSB.m3, PISCO.all.UCSB.m4, PISCO.all.UCSB.m5)


##### UCSB sites with 25% cutoff for frequency of sampling and years seen copper
# Model 1: No factors
PISCO.25.UCSB.m1 = glm.nb(CPRtot ~ 1 + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSB)
summary(PISCO.25.UCSB.m1)

# Model 2: year
PISCO.25.UCSB.m2 = glm.nb(CPRtot ~ year + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSB)
summary(PISCO.25.UCSB.m2)
anova(PISCO.25.UCSB.m2, test="Chi")

# Model 3: year and month - month significant
PISCO.25.UCSB.m3 = glm.nb(CPRtot ~ year + month + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSB)
summary(PISCO.25.UCSB.m3)
anova(PISCO.25.UCSB.m3, test="Chi")

# Model 4: year, month and site - all significant, year less so
PISCO.25.UCSB.m4 = glm.nb(CPRtot ~ year + month + site + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSB)
summary(PISCO.25.UCSB.m4)
anova(PISCO.25.UCSB.m4, test="Chi")

# Model 5: year, month and site and zone - all significant
PISCO.25.UCSB.m5 = glm.nb(CPRtot ~ year + month + site + zone + offset(log(ntransect)), data = PISCO.aggregate.transect.25.UCSB)
summary(PISCO.25.UCSB.m5)
anova(PISCO.25.UCSB.m5, test="Chi")

# AIC - same ordering of models as UCSC and all.UCSB ones
AIC(PISCO.25.UCSB.m1, PISCO.25.UCSB.m2, PISCO.25.UCSB.m3, PISCO.25.UCSB.m4, PISCO.25.UCSB.m5)


##### UCSB sites with 50% cutoff for frequency of sampling and years seen copper
# Model 1: No factors
PISCO.50.UCSB.m1 = glm.nb(CPRtot ~ 1 + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSB)
summary(PISCO.50.UCSB.m1)

# Model 2: year
PISCO.50.UCSB.m2 = glm.nb(CPRtot ~ year + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSB)
summary(PISCO.50.UCSB.m2)
anova(PISCO.50.UCSB.m2, test="Chi")

# Model 3: year and month - neither significant
PISCO.50.UCSB.m3 = glm.nb(CPRtot ~ year + month + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSB)
summary(PISCO.50.UCSB.m3)
anova(PISCO.50.UCSB.m3, test="Chi")

# Model 4: year, month and site - site significant
PISCO.50.UCSB.m4 = glm.nb(CPRtot ~ year + month + site + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSB)
summary(PISCO.50.UCSB.m4)
anova(PISCO.50.UCSB.m4, test="Chi")

# Model 5: year, month and site and zone - site, zone significant, year less so
PISCO.50.UCSB.m5 = glm.nb(CPRtot ~ year + month + site + zone + offset(log(ntransect)), data = PISCO.aggregate.transect.50.UCSB)
summary(PISCO.50.UCSB.m5)
anova(PISCO.50.UCSB.m5, test="Chi")

# AIC - same ordering of models as UCSC data and other UCSB data
AIC(PISCO.50.UCSB.m1, PISCO.50.UCSB.m2, PISCO.50.UCSB.m3, PISCO.50.UCSB.m4, PISCO.50.UCSB.m5)



#################### Estimate posterior and index: ####################
########## STAN set up
library(rstanarm)
library(HDInterval)
options(mc.cores = parallel::detectCores())  # AD comment - for execution on a local, multicore CPU with excess RAM

########## Northern assessment
##### Transects at all UCSC sites that saw copper

##### Use STAN to see how well 'best model' fits the data

start.time <- Sys.time()
PISCO.all.UCSC.m5.STAN <- stan_glm.nb(CPRtot ~ year + month + site + zone
                             + offset(log(ntransect)), data=PISCO.aggregate.transect.all.UCSC,
                             prior_intercept=normal(location=0, scale=10),
                             prior=normal(location=0, scale=10),
                             prior_aux = cauchy(0,5),
                             chains = 4,
                             iter = 5000) # iterations per chain

Sys.time() - start.time
summary(PISCO.all.UCSC.m5.STAN)

##### Compare model and posterior
# compare MLEs to posterior medians; 
png(filename=here("Outputs/Copper","compare_MLEs_to_posterior_medians_all.UCSC.m5.png"), width=6, height=6, units="in", res=300, pointsize=10)
plot(coef(PISCO.all.UCSC.m5.STAN), coef(PISCO.all.UCSC.m5), main="MLEs vs. Posterior Medians", xlab="STAN", ylab="glm.nb"); abline(0,1)
dev.off()

# is the proportion of zeros in the observed data consistent with replicate data sets from the model? 
prop_zero_test1_all_UCSC <- pp_check(PISCO.all.UCSC.m5.STAN, plotfun = "stat", stat = "prop_zero")  # quite high, in the 80-90%
# pretty good (look at scale); dark vertical line is observed proportion of zeros, compared to distribution of model-generated data sets
png(filename=here("Outputs/Copper","proportion_zero_PISCO_STAN_all_UCSC.png"), width=6, height=3, units="in", res=300, pointsize=10)
prop_zero_test1_all_UCSC
dev.off()

##### Extract posterior and build index
# extract posterior draws (log scale)
posterior.df_all_UCSC <- as.data.frame(PISCO.all.UCSC.m5.STAN)
str(posterior.df_all_UCSC)

# calculate distributions by year
log.index.df_all_UCSC <-  cbind.data.frame(posterior.df_all_UCSC[[1]], posterior.df_all_UCSC[[1]] + posterior.df_all_UCSC[2:21])

names(log.index.df_all_UCSC) <- paste('YEAR',c(2001:2021),sep="")
str(log.index.df_all_UCSC)
head(log.index.df_all_UCSC)

PISCO.index.posterior_all_UCSC = exp(log.index.df_all_UCSC)

# calculate summary statistics (mean and logSE) that will go into SS
PISCO.index_all_UCSC <- cbind.data.frame(Mean = apply(PISCO.index.posterior_all_UCSC, 2, mean),
                                logSE = apply(log(PISCO.index.posterior_all_UCSC), 2, sd),
                                HPD_lower = hdi(PISCO.index.posterior_all_UCSC)[1,],
                                HPD_upper = hdi(PISCO.index.posterior_all_UCSC)[2,])
PISCO.index_all_UCSC$Year <- c(2001:2021)
PISCO.index_all_UCSC
# write.csv(PISCO.index_UCSC, quote=F, row.names=F, file=here("Copper output","PISCO_Index_for_SS_UCSC.csv"))  # will just write the one we choose

# Plot index over time
png(filename = here("Outputs/Copper","PISCO_Index_all_UCSC.png"), width = 6, height = 4, units = "in", res = 600, pointsize=10)
plot(PISCO.index_all_UCSC$Year, PISCO.index_all_UCSC$Mean, ylim=c(0,1),xlab="Year", ylab="Central CA PISCO CPFV Index", pch=20)
for (i in 1:nrow(PISCO.index_all_UCSC)) { segments(PISCO.index_all_UCSC[i,"Year"], PISCO.index_all_UCSC[i,"HPD_lower"],
                                          PISCO.index_all_UCSC[i,"Year"], PISCO.index_all_UCSC[i,"HPD_upper"]) }
dev.off()


##### Transects at UCSC sites with at least 25% sampling and copper frequency

##### Use STAN to see how well 'best model' fits the data

start.time <- Sys.time()
PISCO.25.UCSC.m5.STAN <- stan_glm.nb(CPRtot ~ year + month + site + zone
                                      + offset(log(ntransect)), data=PISCO.aggregate.transect.25.UCSC,
                                      prior_intercept=normal(location=0, scale=10),
                                      prior=normal(location=0, scale=10),
                                      prior_aux = cauchy(0,5),
                                      chains = 4,
                                      iter = 5000) # iterations per chain

Sys.time() - start.time
summary(PISCO.25.UCSC.m5.STAN)

##### Compare model and posterior
# compare MLEs to posterior medians; 
png(filename=here("Outputs/Copper","compare_MLEs_to_posterior_medians_25.UCSC.m5.png"), width=6, height=6, units="in", res=300, pointsize=10)
plot(coef(PISCO.25.UCSC.m5.STAN), coef(PISCO.25.UCSC.m5), main="MLEs vs. Posterior Medians", xlab="STAN", ylab="glm.nb"); abline(0,1)
dev.off()

# is the proportion of zeros in the observed data consistent with replicate data sets from the model? 
prop_zero_test1_25_UCSC <- pp_check(PISCO.25.UCSC.m5.STAN, plotfun = "stat", stat = "prop_zero")  # quite high, in the high 70s-80s
# pretty good (look at scale); dark vertical line is observed proportion of zeros, compared to distribution of model-generated data sets
png(filename=here("Outputs/Copper","proportion_zero_PISCO_STAN_25_UCSC.png"), width=6, height=3, units="in", res=300, pointsize=10)
prop_zero_test1_25_UCSC
dev.off()

##### Extract posterior and build index
# extract posterior draws (log scale)
posterior.df_25_UCSC <- as.data.frame(PISCO.25.UCSC.m5.STAN)
str(posterior.df_25_UCSC)

# calculate distributions by year
log.index.df_25_UCSC <-  cbind.data.frame(posterior.df_25_UCSC[[1]], posterior.df_25_UCSC[[1]] + posterior.df_25_UCSC[2:21])

names(log.index.df_25_UCSC) <- paste('YEAR',c(2001:2021),sep="")
str(log.index.df_25_UCSC)
head(log.index.df_25_UCSC)

PISCO.index.posterior_25_UCSC = exp(log.index.df_25_UCSC)

# calculate summary statistics (mean and logSE) that will go into SS
PISCO.index_25_UCSC <- cbind.data.frame(Mean = apply(PISCO.index.posterior_25_UCSC, 2, mean),
                                         logSE = apply(log(PISCO.index.posterior_25_UCSC), 2, sd),
                                         HPD_lower = hdi(PISCO.index.posterior_25_UCSC)[1,],
                                         HPD_upper = hdi(PISCO.index.posterior_25_UCSC)[2,])
PISCO.index_25_UCSC$Year <- c(2001:2021)
PISCO.index_25_UCSC
# write.csv(PISCO.index_UCSC, quote=F, row.names=F, file=here("Copper output","PISCO_Index_for_SS_UCSC.csv"))  # will just write the one we choose

# Plot index over time
png(filename = here("Outputs/Copper","PISCO_Index_25_UCSC.png"), width = 6, height = 4, units = "in", res = 600, pointsize=10)
plot(PISCO.index_25_UCSC$Year, PISCO.index_all_UCSC$Mean, ylim=c(0,1),xlab="Year", ylab="Central CA PISCO CPFV Index", pch=20)
for (i in 1:nrow(PISCO.index_25_UCSC)) { segments(PISCO.index_25_UCSC[i,"Year"], PISCO.index_25_UCSC[i,"HPD_lower"],
                                                   PISCO.index_25_UCSC[i,"Year"], PISCO.index_25_UCSC[i,"HPD_upper"]) }
dev.off()


##### Transects at UCSC sites with at least 50% sampling and copper sightings

##### Use STAN to see how well 'best model' fits the data

start.time <- Sys.time()
PISCO.50.UCSC.m5.STAN <- stan_glm.nb(CPRtot ~ year + month + site + zone
                                      + offset(log(ntransect)), data=PISCO.aggregate.transect.50.UCSC,
                                      prior_intercept=normal(location=0, scale=10),
                                      prior=normal(location=0, scale=10),
                                      prior_aux = cauchy(0,5),
                                      chains = 4,
                                      iter = 5000) # iterations per chain

Sys.time() - start.time
summary(PISCO.50.UCSC.m5.STAN)

##### Compare model and posterior
# compare MLEs to posterior medians; 
png(filename=here("Outputs/Copper","compare_MLEs_to_posterior_medians_50.UCSC.m5.png"), width=6, height=6, units="in", res=300, pointsize=10)
plot(coef(PISCO.50.UCSC.m5.STAN), coef(PISCO.50.UCSC.m5), main="MLEs vs. Posterior Medians", xlab="STAN", ylab="glm.nb"); abline(0,1)
dev.off()

# is the proportion of zeros in the observed data consistent with replicate data sets from the model? 
prop_zero_test1_50_UCSC <- pp_check(PISCO.50.UCSC.m5.STAN, plotfun = "stat", stat = "prop_zero")  # in the 65-75% range
# pretty good (look at scale); dark vertical line is observed proportion of zeros, compared to distribution of model-generated data sets
png(filename=here("Outputs/Copper","proportion_zero_PISCO_STAN_50_UCSC.png"), width=6, height=3, units="in", res=300, pointsize=10)
prop_zero_test1_50_UCSC
dev.off()

##### Extract posterior and build index
# extract posterior draws (log scale)
posterior.df_50_UCSC <- as.data.frame(PISCO.50.UCSC.m5.STAN)
str(posterior.df_50_UCSC)

# calculate distributions by year
log.index.df_50_UCSC <-  cbind.data.frame(posterior.df_50_UCSC[[1]], posterior.df_50_UCSC[[1]] + posterior.df_50_UCSC[2:21])

names(log.index.df_50_UCSC) <- paste('YEAR',c(2001:2021),sep="")
str(log.index.df_50_UCSC)
head(log.index.df_50_UCSC)

PISCO.index.posterior_50_UCSC = exp(log.index.df_50_UCSC)

# calculate summary statistics (mean and logSE) that will go into SS
PISCO.index_50_UCSC <- cbind.data.frame(Mean = apply(PISCO.index.posterior_50_UCSC, 2, mean),
                                         logSE = apply(log(PISCO.index.posterior_50_UCSC), 2, sd),
                                         HPD_lower = hdi(PISCO.index.posterior_50_UCSC)[1,],
                                         HPD_upper = hdi(PISCO.index.posterior_50_UCSC)[2,])
PISCO.index_50_UCSC$Year <- c(2001:2021)
PISCO.index_50_UCSC
# write.csv(PISCO.index_UCSC, quote=F, row.names=F, file=here("Copper output","PISCO_Index_for_SS_UCSC.csv"))  # will just write the one we choose

# Plot index over time
png(filename = here("Outputs/Copper","PISCO_Index_50_UCSC.png"), width = 6, height = 4, units = "in", res = 600, pointsize=10)
plot(PISCO.index_50_UCSC$Year, PISCO.index_50_UCSC$Mean, ylim=c(0,1),xlab="Year", ylab="Central CA PISCO CPFV Index", pch=20)
for (i in 1:nrow(PISCO.index_50_UCSC)) { segments(PISCO.index_50_UCSC[i,"Year"], PISCO.index_50_UCSC[i,"HPD_lower"],
                                                   PISCO.index_50_UCSC[i,"Year"], PISCO.index_50_UCSC[i,"HPD_upper"]) }
dev.off()


########## Southern assessment
##### Transects at all UCSB sites that saw copper

##### Use STAN to see how well 'best model' fits the data

start.time <- Sys.time()
PISCO.all.UCSB.m5.STAN <- stan_glm.nb(CPRtot ~ year + month + site + zone
                                      + offset(log(ntransect)), data=PISCO.aggregate.transect.all.UCSB,
                                      prior_intercept=normal(location=0, scale=10),
                                      prior=normal(location=0, scale=10),
                                      prior_aux = cauchy(0,5),
                                      chains = 4,
                                      iter = 5000) # iterations per chain

Sys.time() - start.time
summary(PISCO.all.UCSB.m5.STAN)

##### Compare model and posterior
# compare MLEs to posterior medians; - most good, a few too high
png(filename=here("Outputs/Copper","compare_MLEs_to_posterior_medians_all.UCSB.m5.png"), width=6, height=6, units="in", res=300, pointsize=10)
plot(coef(PISCO.all.UCSB.m5.STAN), coef(PISCO.all.UCSB.m5), main="MLEs vs. Posterior Medians", xlab="STAN", ylab="glm.nb"); abline(0,1)
dev.off()

# is the proportion of zeros in the observed data consistent with replicate data sets from the model? 
prop_zero_test1_all_UCSB <- pp_check(PISCO.all.UCSB.m5.STAN, plotfun = "stat", stat = "prop_zero")  # in the 80s, observed is a bit low compared to distribution peak
# pretty good (look at scale); dark vertical line is observed proportion of zeros, compared to distribution of model-generated data sets
png(filename=here("Outputs/Copper","proportion_zero_PISCO_STAN_all_UCSB.png"), width=6, height=3, units="in", res=300, pointsize=10)
prop_zero_test1_all_UCSB
dev.off()

##### Extract posterior and build index
# extract posterior draws (log scale)
posterior.df_all_UCSB <- as.data.frame(PISCO.all.UCSB.m5.STAN)
str(posterior.df_all_UCSB)

# calculate distributions by year
log.index.df_all_UCSB <-  cbind.data.frame(posterior.df_all_UCSB[[1]], posterior.df_all_UCSB[[1]] + posterior.df_all_UCSB[2:18])

names(log.index.df_all_UCSB) <- paste('YEAR',c(2004:2021),sep="")
str(log.index.df_all_UCSB)
head(log.index.df_all_UCSB)

PISCO.index.posterior_all_UCSB = exp(log.index.df_all_UCSB)

# calculate summary statistics (mean and logSE) that will go into SS
PISCO.index_all_UCSB <- cbind.data.frame(Mean = apply(PISCO.index.posterior_all_UCSB, 2, mean),
                                         logSE = apply(log(PISCO.index.posterior_all_UCSB), 2, sd),
                                         HPD_lower = hdi(PISCO.index.posterior_all_UCSB)[1,],
                                         HPD_upper = hdi(PISCO.index.posterior_all_UCSB)[2,])
PISCO.index_all_UCSB$Year <- c(2004:2021)
PISCO.index_all_UCSB
# write.csv(PISCO.index_UCSC, quote=F, row.names=F, file=here("Copper output","PISCO_Index_for_SS_UCSC.csv"))  # will just write the one we choose

# Plot index over time
png(filename = here("Outputs/Copper","PISCO_Index_all_UCSB.png"), width = 6, height = 4, units = "in", res = 600, pointsize=10)
plot(PISCO.index_all_UCSB$Year, PISCO.index_all_UCSB$Mean, ylim=c(0,2.5),xlab="Year", ylab="Southern CA PISCO CPFV Index", pch=20)
for (i in 1:nrow(PISCO.index_all_UCSB)) { segments(PISCO.index_all_UCSB[i,"Year"], PISCO.index_all_UCSB[i,"HPD_lower"],
                                                   PISCO.index_all_UCSB[i,"Year"], PISCO.index_all_UCSB[i,"HPD_upper"]) }
dev.off()


##### Transects at UCSB sites with at least 25% sampling and copper sighting

##### Use STAN to see how well 'best model' fits the data

start.time <- Sys.time()
PISCO.25.UCSB.m5.STAN <- stan_glm.nb(CPRtot ~ year + month + site + zone
                                      + offset(log(ntransect)), data=PISCO.aggregate.transect.25.UCSB,
                                      prior_intercept=normal(location=0, scale=10),
                                      prior=normal(location=0, scale=10),
                                      prior_aux = cauchy(0,5),
                                      chains = 4,
                                      iter = 5000) # iterations per chain

Sys.time() - start.time
summary(PISCO.25.UCSB.m5.STAN)

##### Compare model and posterior
# compare MLEs to posterior medians; - looks good
png(filename=here("Outputs/Copper","compare_MLEs_to_posterior_medians_25.UCSB.m5.png"), width=6, height=6, units="in", res=300, pointsize=10)
plot(coef(PISCO.25.UCSB.m5.STAN), coef(PISCO.25.UCSB.m5), main="MLEs vs. Posterior Medians", xlab="STAN", ylab="glm.nb"); abline(0,1)
dev.off()

# is the proportion of zeros in the observed data consistent with replicate data sets from the model? 
prop_zero_test1_25_UCSB <- pp_check(PISCO.25.UCSB.m5.STAN, plotfun = "stat", stat = "prop_zero")  # in the 75-80s
# pretty good (look at scale); dark vertical line is observed proportion of zeros, compared to distribution of model-generated data sets
png(filename=here("Outputs/Copper","proportion_zero_PISCO_STAN_25_UCSB.png"), width=6, height=3, units="in", res=300, pointsize=10)
prop_zero_test1_25_UCSB
dev.off()

##### Extract posterior and build index
# extract posterior draws (log scale)
posterior.df_25_UCSB <- as.data.frame(PISCO.25.UCSB.m5.STAN)
str(posterior.df_25_UCSB)

# calculate distributions by year
log.index.df_25_UCSB <-  cbind.data.frame(posterior.df_25_UCSB[[1]], posterior.df_25_UCSB[[1]] + posterior.df_25_UCSB[2:18])

names(log.index.df_25_UCSB) <- paste('YEAR',c(2004:2021),sep="")
str(log.index.df_25_UCSB)
head(log.index.df_25_UCSB)

PISCO.index.posterior_25_UCSB = exp(log.index.df_25_UCSB)

# calculate summary statistics (mean and logSE) that will go into SS
PISCO.index_25_UCSB <- cbind.data.frame(Mean = apply(PISCO.index.posterior_25_UCSB, 2, mean),
                                         logSE = apply(log(PISCO.index.posterior_25_UCSB), 2, sd),
                                         HPD_lower = hdi(PISCO.index.posterior_25_UCSB)[1,],
                                         HPD_upper = hdi(PISCO.index.posterior_25_UCSB)[2,])
PISCO.index_25_UCSB$Year <- c(2004:2021)
PISCO.index_25_UCSB
# write.csv(PISCO.index_UCSC, quote=F, row.names=F, file=here("Copper output","PISCO_Index_for_SS_UCSC.csv"))  # will just write the one we choose

# Plot index over time
png(filename = here("Outputs/Copper","PISCO_Index_25_UCSB.png"), width = 6, height = 4, units = "in", res = 600, pointsize=10)
plot(PISCO.index_25_UCSB$Year, PISCO.index_25_UCSB$Mean, ylim=c(0,0.2),xlab="Year", ylab="Southern CA PISCO CPFV Index", pch=20)
for (i in 1:nrow(PISCO.index_25_UCSB)) { segments(PISCO.index_25_UCSB[i,"Year"], PISCO.index_25_UCSB[i,"HPD_lower"],
                                                   PISCO.index_25_UCSB[i,"Year"], PISCO.index_25_UCSB[i,"HPD_upper"]) }
dev.off()


##### Transects at UCSB sites with at least 50% sampling frequency and copper sighting

##### Use STAN to see how well 'best model' fits the data

start.time <- Sys.time()
PISCO.50.UCSB.m5.STAN <- stan_glm.nb(CPRtot ~ year + month + site + zone
                                      + offset(log(ntransect)), data=PISCO.aggregate.transect.50.UCSB,
                                      prior_intercept=normal(location=0, scale=10),
                                      prior=normal(location=0, scale=10),
                                      prior_aux = cauchy(0,5),
                                      chains = 4,
                                      iter = 5000) # iterations per chain

Sys.time() - start.time
summary(PISCO.50.UCSB.m5.STAN)

##### Compare model and posterior
# compare MLEs to posterior medians; - most good, a few too high
png(filename=here("Outputs/Copper","compare_MLEs_to_posterior_medians_50.UCSB.m5.png"), width=6, height=6, units="in", res=300, pointsize=10)
plot(coef(PISCO.50.UCSB.m5.STAN), coef(PISCO.50.UCSB.m5), main="MLEs vs. Posterior Medians", xlab="STAN", ylab="glm.nb"); abline(0,1)
dev.off()

# is the proportion of zeros in the observed data consistent with replicate data sets from the model? 
prop_zero_test1_50_UCSB <- pp_check(PISCO.50.UCSB.m5.STAN, plotfun = "stat", stat = "prop_zero")  # in the 55-70%, observed is a bit low compared to distribution peak
# pretty good (look at scale); dark vertical line is observed proportion of zeros, compared to distribution of model-generated data sets
png(filename=here("Outputs/Copper","proportion_zero_PISCO_STAN_50_UCSB.png"), width=6, height=3, units="in", res=300, pointsize=10)
prop_zero_test1_50_UCSB
dev.off()

##### Extract posterior and build index
# extract posterior draws (log scale)
posterior.df_50_UCSB <- as.data.frame(PISCO.50.UCSB.m5.STAN)
str(posterior.df_50_UCSB)

# calculate distributions by year
log.index.df_50_UCSB <-  cbind.data.frame(posterior.df_50_UCSB[[1]], posterior.df_50_UCSB[[1]] + posterior.df_50_UCSB[2:18])

names(log.index.df_50_UCSB) <- paste('YEAR',c(2004:2021),sep="")
str(log.index.df_50_UCSB)
head(log.index.df_50_UCSB)

PISCO.index.posterior_50_UCSB = exp(log.index.df_50_UCSB)

# calculate summary statistics (mean and logSE) that will go into SS
PISCO.index_50_UCSB <- cbind.data.frame(Mean = apply(PISCO.index.posterior_50_UCSB, 2, mean),
                                         logSE = apply(log(PISCO.index.posterior_50_UCSB), 2, sd),
                                         HPD_lower = hdi(PISCO.index.posterior_50_UCSB)[1,],
                                         HPD_upper = hdi(PISCO.index.posterior_50_UCSB)[2,])
PISCO.index_50_UCSB$Year <- c(2004:2021)
PISCO.index_50_UCSB
# write.csv(PISCO.index_UCSC, quote=F, row.names=F, file=here("Copper output","PISCO_Index_for_SS_UCSC.csv"))  # will just write the one we choose

# Plot index over time
png(filename = here("Outputs/Copper","PISCO_Index_50_UCSB.png"), width = 6, height = 4, units = "in", res = 600, pointsize=10)
plot(PISCO.index_50_UCSB$Year, PISCO.index_50_UCSB$Mean, ylim=c(0,0.2),xlab="Year", ylab="Southern CA PISCO CPFV Index", pch=20)
for (i in 1:nrow(PISCO.index_50_UCSB)) { segments(PISCO.index_50_UCSB[i,"Year"], PISCO.index_50_UCSB[i,"HPD_lower"],
                                                   PISCO.index_50_UCSB[i,"Year"], PISCO.index_50_UCSB[i,"HPD_upper"]) }
dev.off()

########## Compare indices from different site filtering
# Add site filtering and region to index data frames
PISCO.index_all_UCSC = PISCO.index_all_UCSC %>%
  mutate(sites = "all",
         region = "north")
PISCO.index_25_UCSC = PISCO.index_25_UCSC %>%
  mutate(sites = "25% sampling",
         region = "north")
PISCO.index_50_UCSC = PISCO.index_50_UCSC %>%
  mutate(sites = "50% sampling",
         region = "north")
PISCO.index_all_UCSB = PISCO.index_all_UCSB %>%
  mutate(sites = "all",
         region = "south")
PISCO.index_25_UCSB = PISCO.index_25_UCSB %>%
  mutate(sites = "25% sampling",
         region = "south")
PISCO.index_50_UCSB = PISCO.index_50_UCSB %>%
  mutate(sites = "50% sampling",
         region = "south")

# Combine into one data frame
PISCO.index.all = rbind(PISCO.index_all_UCSC, PISCO.index_25_UCSC, PISCO.index_50_UCSC,
                        PISCO.index_all_UCSB, PISCO.index_25_UCSB, PISCO.index_50_UCSB)

# All site filtering on same plot, subplots by region
compare_index_plot1 = ggplot(data = PISCO.index.all, aes(x = Year, y = Mean, color = sites, fill = sites)) +
  geom_point(alpha = 0.75) +
  geom_errorbar(aes(ymin = HPD_lower, ymax = HPD_upper), alpha = 0.75) +
  xlab("Year") + ylab("Index") +
  scale_color_manual(values = site_filtering_color_scheme) +
  scale_fill_manual(values = site_filtering_color_scheme) +
  theme_bw() +
  facet_wrap(~region)

ggsave(here("Outputs/Copper","Index_CPR_site_filtering_comp1.png"), compare_index_plot1)

# Subplots by region and site filtering
compare_index_plot2 = ggplot(data = PISCO.index.all, aes(x = Year, y = Mean, color = sites, fill = sites)) +
  geom_point() +
  geom_errorbar(aes(ymin = HPD_lower, ymax = HPD_upper)) +
  xlab("Year") + ylab("Index") +
  scale_color_manual(values = site_filtering_color_scheme) +
  scale_fill_manual(values = site_filtering_color_scheme) +
  theme_bw() +
  facet_wrap(~region + sites)

ggsave(here("Outputs/Copper","Index_CPR_site_filtering_comp2.png"), compare_index_plot2, width = 8)

# Just north, subplots by site filtering
compare_index_north = ggplot(data = PISCO.index.all %>% filter(region == "north"), aes(x = Year, y = Mean, color = sites, fill = sites)) +
  geom_point() +
  geom_errorbar(aes(ymin = HPD_lower, ymax = HPD_upper)) +
  xlab("Year") + ylab("Index") +
  scale_color_manual(values = site_filtering_color_scheme) +
  scale_fill_manual(values = site_filtering_color_scheme) +
  theme_bw() +
  facet_wrap(~sites)

ggsave(here("Outputs/Copper","Index_CPR_site_filtering_comp_north.png"), compare_index_north, width = 8)

# Just south, subplots by site filtering
compare_index_south = ggplot(data = PISCO.index.all %>% filter(region == "south"), aes(x = Year, y = Mean, color = sites, fill = sites)) +
  geom_point() +
  geom_errorbar(aes(ymin = HPD_lower, ymax = HPD_upper)) +
  xlab("Year") + ylab("Index") +
  scale_color_manual(values = site_filtering_color_scheme) +
  scale_fill_manual(values = site_filtering_color_scheme) +
  theme_bw() +
  facet_wrap(~sites)

ggsave(here("Outputs/Copper","Index_CPR_site_filtering_comp_south.png"), compare_index_south, width = 8)


