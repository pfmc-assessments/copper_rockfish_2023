################################################################################
### CCFRP index
### Copper rockfish assessment 2023
### Melissa Monk
### Random effects models do not converge - "model is nearly unidentifiable"
### "model failed to converge with max gradient"
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

# Load in some helper functions for processing and plotting the data
#R path
github_path <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
all <- list.files(file.path(github_path, "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(github_path, "R", "sdmTMB", all[a]))}

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "south"
ccfrpSpeciesCode <- "CPR"
#setwd to the north or the south

#setwd(glue::glue(here(),"/data/survey_indices/ccfrp/"))
dir <- file.path("S:/copper_rockfish_2023/data/survey_indices/ccfrp", modelArea)
setwd(dir)

#Assuming 20% of the habitat is in MPAs in both areas
#Also assign the areas to a district to then use the habiat weights
load("Filtered_data_CCFRP.RData")
orginal_dat <- dat
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
  mutate_at(covars, as.factor) %>% # make sure covariates are factors
  mutate(depth = depth/6,
         depth_2 = depth^2)

#assign regions to the data
if(modelArea == "north"){
#Point Lobos and north and Morro/Avila separate
dat <- dat %>%
  mutate(region = case_when(monitoringGroup == "Cal Poly" ~ "Morro/Avila",
                            TRUE ~ "Point Lobos north")) %>%
  mutate_at(vars(region), as.factor)
}

if(modelArea == "south"){
  #
  dat <- dat %>%
    mutate(region = monitoringGroup) %>%
    mutate_at(vars(region), as.factor)
}

#-------------------------------------------------------------------------------

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
# can't do this in northern CA because not all of the years are represented
# by region
# could possibly split by south and north of Morro...
# do we have enough data for this??   if the trends among regions are different
# we want to weight them
# 
# plots
# 1) All data combined; 1 trend
# 2) By Region (ignore MPAs; 3 trends with the north since 2017)
# 3) By MPA or Ref (all regions combined; 2 trends)
# 4) By Region and MPA (6 trends, perhaps 1 panel per region)
# 
# Then, build your models following that order, first without gridCellID, and then with gridCellID as a random effect.
# Whenever you include an interaction term, include the main effect term for each of the interacting variables,
# e.g., when you include MPAorREF:year, include both year and MPAorREF as main effects. R may do this
# automatically, but it's good to be explicit just in case. I think it should be OK to include gridCellID as a random
# effect even when region is specified as a main effect. You are assuming that the variance of the random
# effect is the same across all regions, but that seems reasonable to me.
# 
# Target ~ year + poly(depth, 2) + offset(logeffort)
# Target ~ year + poly(depth, 2)+ (1|gridCellID) + offset(logeffort)
# 
# Target ~ year + region + poly(depth, 2) + offset(logeffort)
# Target ~ year + region + poly(depth, 2) + (1|gridCellID) + offset(logeffort)
# Target ~ year + region + year:region + poly(depth, 2) + offset(logeffort)
# Target ~ year + region + year:region + poly(depth, 2) + (1|gridCellID) + offset(logeffort)
# # or
# Target ~ year + region + (1|year:region) + poly(depth, 2) + (1|gridCellID) + offset(logeffort)
# 
# Target ~ year + MPAorREF  + poly(depth, 2) + offset(logeffort)
# Target ~ year + MPAorREF  + poly(depth, 2) + (1|gridCellID) + offset(logeffort)
# Target ~ year + MPAorREF  + year:MPAorREF + poly(depth, 2) + offset(logeffort)
# Target ~ year + MPAorREF  + year:MPAorREF + poly(depth, 2) + (1|gridCellID) + offset(logeffort)
# 
# # This one interests me (E.J.) the most. 
##After accounting for differences in trend by region, is there a difference between MPA and Ref sites?
# Target ~ year + region + year:region + MPAorREF + poly(depth, 2) + (1|gridCellID) + offset(logeffort)

#-------------------------------------------------------------------------------
average_cpue <- dat %>% group_by(year) %>% summarise(avg_cpue = mean(cpue))

#All data combined
ggplot(average_cpue, aes(x = year, y = avg_cpue, group = 1)) +
  geom_point() +
  geom_line() 
ggsave(file = file.path(dir, "all_cpue.png"), width = 7, height = 7)


#more visualizations
year_site_mpa <- dat %>% 
  group_by(year, siteName, MPAorREF) %>% 
  summarise(avg_cpue = mean(cpue))


#cpue by mpa and site
ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = MPAorREF, group = MPAorREF)) +
  geom_point() +
  geom_path() + xlab("Year") + ylab("Average CPUE") +
  facet_wrap(~siteName) +
  scale_color_viridis_d()
ggsave(file = file.path(dir, "mpa_site_cpue.png"), width = 7, height = 7)


#same as above but flip the facet
ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = siteName, group = siteName)) +
  geom_point() +
  geom_line() +
  facet_wrap(~MPAorREF) +
  scale_color_viridis_d()
ggsave(file = file.path(dir, "site_mpa_cpue.png"), width = 7, height = 7)

#all 6 trends
ggplot(year_site_mpa, aes(x = year, y = avg_cpue, colour = MPAorREF, 
                          linetype = year_site_mpa$siteName,
                          group = interaction(siteName:MPAorREF))) +
  geom_line(linewidth = 1.5) +
  scale_color_viridis_d(begin = .1, end = .5)
ggsave(file = file.path(dir, "mpa_site_3way_cpue.png"), width = 7, height = 7)

#not collapse regions in the north - north and south of Lobos
year_region_mpa <- dat %>% 
  group_by(year, region, MPAorREF) %>% 
  summarise(avg_cpue = mean(cpue))

ggplot(year_region_mpa, aes(x = year, y = avg_cpue, colour = region, 
                          linetype = year_region_mpa$MPAorREF,
                          group = interaction(region:MPAorREF))) +
  geom_line(linewidth = 1.5) +
  scale_color_viridis_d(begin = .1, end = .5)
ggsave(file = file.path(dir, "mpa_region_3way_cpue.png"), width = 7, height = 7)

#for the north Regions are the same - weight by inside outside/mpa

#-------------------------------------------------------------------------------
#NORTHERN CA models
# region is north and south of Point Lobos to ensure a full time series
#full model with year:region interaction
model.full <- MASS::glm.nb(
  Target ~  year + region + depth + depth_2 + MPAorREF + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
main_effects <- ggpredict(model.full, terms = "year")

#see if the year:region interaction is significant in the north
#not significant in the south
model.full <- MASS::glm.nb(
  Target ~  year*region + MPAorREF + depth + depth_2 + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
#year_region_interxn <- ggpredict(model.full, terms = "year")

#see if the year:MPAorREF
model.full <- MASS::glm.nb(
  Target ~  region + depth + depth_2 + year*MPAorREF + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
#year_mpa_interxn <- ggpredict(model.full, terms = "year")
#MPAorREF:year is significant in the north
#not significant in the south

#see if we can add the gridCellID as a random effect - removing the interaction first
#DOES NOT CONVERGE
# model.full <- lme4::glmer.nb(
#   Target ~  year + MPAorREF + depth + depth_2 + (1|gridCellID) + offset(logEffort),
#   data = dat,
#   na.action = "na.fail")
# summary(model.full)
# anova(model.full)

#look at 3 way interaction
model.full <- MASS::glm.nb(
  Target ~ depth + depth_2 + year*MPAorREF*region + offset(logEffort),
  data = dat,
  na.action = "na.fail")
summary(model.full)
anova(model.full)
year_region_mpa_interxn <- ggpredict(model.full, terms = "year")
#significant in the north
#not significan in the south
#combine ggpredict output
all_indices <- cbind(main_effects[, 1:2], year_region_interxn[, 2], 
                     year_mpa_interxn[, 2], year_region_mpa_interxn[, 2])
colnames(all_indices) <- 
  c("year", "main_effects", "year_region_interxn", "year_mpa_interxn", "year_region_mpa_interaction")
all_indices <- all_indices %>%
  pivot_longer(!year, names_to = "index", values_to = "Standardizedcpue")

ggplot(all_indices, aes(x = year, y = Standardizedcpue, colour = index, group = index )) +
  geom_point() +
  geom_line() + xlab("Year") + ylab("Index")
ggsave(file = file.path(dir, "index_comparison.png"), width = 7, height = 7)



model.full <- MASS::glm.nb(Target ~  region + depth + depth_2 + offset(logEffort),
  data = dat,
  na.action = "na.fail")
#MuMIn will fit all models and then rank them by AICc
model.suite <- MuMIn::dredge(model.full,
                             rank = "AICc", 
                             fixed= c("offset(logEffort)", "year"))

#Create model selection dataframe for the document
Model_selection <- as.data.frame(model.suite) %>%
  dplyr::select(-weight)
Model_selection

#in the north region is not signficant and the AIC differents is <2 so drop
#northern grid

if(modelArea == "north"){

grid <- expand.grid(
  year = unique(dat$year),
  depth = dat$depth[1],
  depth_2 = dat$depth_2[1],
  MPAorREF = levels(dat$MPAorREF)[1])

grid2 <- NULL
for (a in 1:20){
  grid2 <- rbind(grid2, grid[grid$MPAorREF == "MPA", ])
}
for (a in 1:80){
  grid2 <- rbind(grid2, grid[grid$MPAorREF == "REF", ])
}

fit.nb <- sdmTMB(
  Target ~  poly(depth, 2) + year*MPAorREF,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  control = sdmTMBcontrol(newton_loops = 1))

do_diagnostics(
  dir = file.path(dir, "area_weighted"), 
  fit = fit.nb,
  plot_resid = FALSE)

calc_index(
  dir = file.path(dir, "area_weighted"), 
  fit = fit.nb,
  grid = grid2)
}


if(modelArea == "south"){
  grid <- expand.grid(
    year = unique(dat$year),
    depth = dat$depth[1],
    depth_2 = dat$depth_2[1],
    region = dat$region[1],
    MPAorREF = levels(dat$MPAorREF)[1])
  
  grid2 <- NULL
  for (a in 1:20){
    grid2 <- rbind(grid2, grid[grid$MPAorREF == "MPA", ])
  }
  for (a in 1:80){
    grid2 <- rbind(grid2, grid[grid$MPAorREF == "REF", ])
  }
  
  fit.nb <- sdmTMB(
    Target ~  poly(depth, 2) + region + year*MPAorREF,
    data = dat,
    offset = dat$logEffort,
    time = "year",
    spatial="off",
    spatiotemporal = "off",
    family = nbinom2(link = "log"),
    control = sdmTMBcontrol(newton_loops = 1))
  
  do_diagnostics(
    dir = file.path(dir, "area_weighted"), 
    fit = fit.nb,
    plot_resid = FALSE)
  
  calc_index(
    dir = file.path(dir, "area_weighted"), 
    fit = fit.nb,
    grid = grid2)
}



pred <- predict(fit.nb, newdata = grid2, return_tmb_object = TRUE)
index_area_weighted <- get_index(pred, bias_correct = TRUE)
index_for_plot <- index_area_weighted %>%
  dplyr::select(year, est) %>%
  rename(Standardizedcpue = est) %>%
  mutate(index = "year_mpa_interxn_area_weighted") %>%
  relocate(Standardizedcpue, .after=last_col())
all_indices <- rbind(all_indices, index_for_plot)  


all_indices <- all_indices %>%
  group_by(index) %>%
  rename(Index = index) %>%
  mutate(StandardizedCPUE = Standardizedcpue/mean(Standardizedcpue)) %>%
   ungroup()

ggplot(all_indices, aes(x = year, y = StandardizedCPUE, colour = Index, group = Index )) +
  geom_point() +
  geom_line() + xlab("Year") + ylab("Standardized Index") +
  theme_bw() + ylim(c(min(all_indices$StandardizedCPUE)*.9, max(all_indices$StandardizedCPUE)*1.1)) +
  theme(legend.position = c(.25, .85)) +
  scale_color_viridis_d()
ggsave(file = file.path(dir, "index_comparison_include_areaweighted.png"), width = 7, height = 7)

# Model selection and data filter tables ---------------------------------------
#Format data filtering table and the model selection table for document
dataFilters <- data.frame(lapply(data_filters, as.character), stringsasFactors = FALSE)
write.csv(dataFilters, 
          file = file.path(dir, "dataFilters.csv"), 
          row.names = FALSE)

#View(Model_selection)
#format table for the document
out <- Model_selection %>%
  dplyr::select(-`(Intercept)`) %>%
  mutate_at(vars("region",  "MPAorREF" ,"year","offset(logEffort)"), as.character) %>%
  mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
#  replace_na(list(region = "Excluded", 
        #          MPAorREF:year = "Excluded",
#                  MPAorREF = "Excluded", 
            #      depth_2 = "Excluded", 
 #                 depth = "Excluded")) %>%
  mutate_at(c("region",  "MPAorREF" ,"year", "offset(logEffort)"), 
            funs(stringr::str_replace(.,"\\+","Included"))) %>%
  rename(`Effort offset` = `offset(logEffort)`, 
         `log-likelihood` = logLik,
         `Depth squared` = depth_2,
         `Interaction` = `MPAorREF:year`) %>%
  rename_with(stringr::str_to_title,-AICc)
#View(out)
write.csv(Model_selection, file = file.path(dir, "model_selection.csv"), 
          row.names = FALSE)


write.csv(data_filters)


#summary of trips and  percent pos per year
summaries <- dat %>%
  group_by(year) %>%
  summarise(tripsWithTarget = sum(Target>0),
            tripsWOTarget = sum(Target==0)) %>%
  mutate(totalTrips = tripsWithTarget+tripsWOTarget,
         percentpos = tripsWithTarget/(tripsWithTarget+tripsWOTarget)) 
View(summaries)
write.csv(summaries, 
          file.path(dir,  "percent_pos.csv"),
          row.names=FALSE)




#south delta logn
grid <- expand.grid(
  year = unique(dat$year),
  depth = dat$depth[1],
  depth_2 = dat$depth_2[1],
  region = levels(dat$region)[1],
  MPAorREF = levels(dat$MPAorREF)[1])

fit.logn <- sdmTMB(
  Target ~  poly(depth, 2) + region + year + MPAorREF,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = delta_lognormal(),
  control = sdmTMBcontrol(newton_loops = 1))

do_diagnostics(
  dir = file.path(dir, "deltalogn"), 
  fit = fit.logn,
  plot_resid = FALSE)

calc_index(
  dir = file.path(dir, "deltalogn"), 
  fit = fit.logn,
  grid = grid)

