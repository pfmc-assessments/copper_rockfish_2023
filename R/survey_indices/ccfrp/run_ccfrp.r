################################################################################
### CCFRP index
### Copper rockfish assessment 2023
### Melissa Monk
################################################################################
rm(list = ls(all = TRUE))
graphics.off()

library(sdmTMB)
library(tmbstan)
library(ggeffects)
library(MuMIn)
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
library(gridExtra)
library(fitdistrplus)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
ccfrpSpeciesCode <- "CPR"
#setwd to the north or the south

#setwd(glue::glue(here(),"/data/survey_indices/ccfrp/"))
dir <- file.path("S:/copper_rockfish_2023/data/survey_indices/ccfrp", modelArea)
setwd(dir)

#Assuming 20% of the habitat is in MPAs in both areas
#Also assign the areas to a district to then use the habiat weights
load("Filtered_data_CCFRP.RData")
orginal_dat <- dat


#source delta bayes functions
#source(file.path(here(),"rec_indices","Delta_bayes_functions.R"))
# Load in some helper functions for processing and plotting the data
#R path
github_path <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
all <- list.files(file.path(github_path, "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(github_path, "R", "sdmTMB", all[a]))}


#-------------------------------------------------------------------------------
# models to explore
# inside/outside MPAs the same in northern and southern CA 80/20
# region here represents the location of the MPAs - can likely use district
# region weights will be the interpreted habitat by district or finer if needed
#
#
# use depth and depth^2 and add gridCellID as a random effect
## Target ~ year + poly(depth, 2) + MPAorREF + (1|gridCellID) + offset(logeffort)
#
# see if the MPA/REF effect is significant - if not, then no habitat weighting
## Target ~ year + poly(depth, 2) + MPAorREF:year + (1|gridCellID) + offset(logeffort)
#
#
# OR look at region:year and ignore MPAs - if the difference in trends is greater
# than the difference between MPA:REF sites we could use a year:region model
## Target ~ year + poly(depth, 2) + year:region + (1|gridCellID)
#
# also plot trends inside/outside for each region and see if the 3-way interaction
# is significant (main effects and 2-way interaction as well)
## Target ~  year + poly(depth, 2)  + MPAorREF:year:region + (1|gridCellID) + offset(logeffort)
#
# do we have enough data for this??   if the trends among regions are different
# we want to weight them

#-------------------------------------------------------------------------------
#Ensure columns named appropriately and covariates are factors
covars <- c("year", "month", "siteName", "MPAorREF", "gridCellID")

dat <- dat %>%
  dplyr::select(year, month, depth, name, site, effort, gridCellID, Full.Name, 
                Nearest.Port, monitoringGroup,
                anglers, driftTime, Target) %>%
  mutate(Effort = anglers * (driftTime * 60)) %>% #need cpue > 1 to take log
  rename(siteName = name,
         MPAorREF = site,
         fullName = Full.Name,
         nearestPort = Nearest.Port) %>%
  mutate(logEffort = log(Effort),
         cpue = Target/Effort) %>%
  mutate_at(covars, as.factor) # make sure covariates are factors

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#more visualizations
year_site_mpa <- dat %>% 
  group_by(year, siteName, MPAorREF) %>% 
  summarise(avg_cpue = mean(cpue))
#look at 
ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = MPAorREF, group = MPAorREF)) +
  geom_point() +
  geom_path() +
  facet_wrap(~siteName) +
  scale_color_viridis_d()

ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = siteName, group = siteName)) +
  geom_point() +
  geom_line() +
  facet_wrap(~MPAorREF) +
  scale_color_viridis_d()


ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = MPAorREF, 
                          linetype = year_site_mpa$siteName,
                          group = interaction(siteName:MPAorREF))) +
  geom_line(linewidth = 1.5) +
  scale_color_viridis_d(begin = .1, end = .5)

ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = siteName, 
                          linetype = year_site_mpa$MPAorREF,
                          group = interaction(siteName:MPAorREF))) +
  geom_line(linewidth = 1.5) +
  scale_color_viridis_d(begin = .1, end = .5)



#Model selection
#full model
#Need to get depths from Becky
model.full <- MASS::glm.nb(
  Target ~  month + year:MPAorREF + offset(logEffort) + (1|gridCellID),
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


