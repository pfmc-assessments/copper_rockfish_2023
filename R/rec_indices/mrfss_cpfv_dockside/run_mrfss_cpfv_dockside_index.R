# Run the MRFSS cpfv dockside index
# Melissa Monk June 2021; modified for copper 2023

rm(list = ls(all = TRUE))
graphics.off()

library(sdmTMB)
library(tmbstan)
library(sdmTMBextra)
library(ggeffects)

#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"
modelArea = "north"
#other information
indexName <-  "mrfss_cpfv_dockside"
covars <- c("year", "county", "wave")

#setwd to the north or the south
#set working directory
dir <- file.path("S:/copper_rockfish_2023/data/rec_indices/mrfss_cpfv_dockside",modelArea)
#setwd(glue::glue(here(),"/data/rec_indices/crfs_pr_dockside/"))
##out.dir <- glue::glue(getwd(),'/',modelArea,'/')
setwd(dir)

# Set working directories
#dir <- file.path(here(),"data","rec_indices", indexName, modelArea)
#setwd(dir)
# create output directory for each model
#out.dir <- getwd()
#plots.dir <- glue(getwd(),"/plots")

# load data
load(glue(getwd(),"/data_for_GLM.RData"
))

#Ensure columns named appropriately and covariates are factors
dat <- area.dat.sm %>%
  rename(Effort = ANGLERxHRS,
  year = YEAR,
  county = CNTY,
  wave = WAVE) %>%
  mutate(logEffort = log(Effort)) %>%
  mutate_at(covars, as.factor) %>% # make sure covariates are factors
  mutate(cpue = Target/Effort)

#plots of average cpue by covariates
#plot raw cpue by county
cpue_cnty <- dat %>% group_by(year, county) %>% 
summarise(average_cpue = mean(cpue))

ggplot(cpue_cnty, aes(x = year, y = average_cpue, colour = county)) +
geom_point(size = 3)  + theme_bw() +
geom_line(aes(x = year, y = average_cpue, colour = county, group = county)) +
xlab("Year") + ylab("Average CPUE") + ylim(c(0, .8)) + 
scale_color_viridis_d(name = "FIPS county code")
ggsave(file = file.path(dir, "plots", "average_cpue_by_cnty.png"), width = 7, height = 7)

cpue_wave <- dat %>% group_by(year, wave) %>% 
summarise(average_cpue = mean(cpue))

ggplot(cpue_wave, aes(x = year, y = average_cpue, colour = wave)) +
geom_point(size = 3)  + theme_bw() +
geom_line(aes(x = year, y = average_cpue, colour = wave, group = wave)) +
xlab("Year") + ylab("Average CPUE") + ylim(c(0, .8)) + 
scale_color_viridis_d()
ggsave(file = file.path(dir, "plots", "raw_cpue_by_wave.png"), width = 7, height = 7)

#-------------------------------------------------------------------------------
#Model selection
#full model
model.full <- MASS::glm.nb(
  Target ~ year + county + wave + offset(logEffort),
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
Model_selection <- as.data.frame(model.suite)
Model_selection
#pull out the best model
best.model <- MuMIn::get.models(model.suite,subset = delta == 0)
best.model

if(modelArea=="south"){
#set the grid
grid <- expand.grid(
  year = unique(dat$year),
  county = levels(dat$county)[1],
  wave = levels(dat$wave)[1])

fit.nb <- sdmTMB(
  Target ~ year + county + wave,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  silent = TRUE,
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),   
  index_args = list(area = 1))
} else {
  #set the grid
grid <- expand.grid(
  year = unique(dat$year),
  county = levels(dat$county)[1])
#northern model
fit.nb <- sdmTMB(
  Target ~ year + county ,
  data = dat,
  offset = dat$logEffort,
  time = "year",
  spatial="off",
  spatiotemporal = "off",
  family = nbinom2(link = "log"),
  silent = TRUE,
  do_index = TRUE,
  predict_args = list(newdata = grid, re_form_iid = NA),   
  index_args = list(area = 1))
}
pred <- predict(fit.nb, return_tmb_object = TRUE, newdata = grid)
index <- get_index(pred, bias_correct = TRUE)
index

#-------------------------------------------------------------------------------
# Load in some helper functions for processing and plotting the data
all <- list.files(file.path(here(), "R", "sdmTMB"))
for (a in 1:length(all)) { source(file.path(here(), "R", "sdmTMB", all[a]))}

#Get diagnostics and index for SS
do_diagnostics(
  dir = dir, 
  fit = fit.nb)

calc_index(
  dir = file.path(dir, "forSS"), 
  fit = fit.nb,
  grid = grid)

#-------------------------------------------------------------------------------
#Format data filtering table and the model selection table for document
View(dataFilters)
dataFilters <- dataFilters %>%
rowwise() %>%
filter(!all(is.na(across((everything()))))) %>%
ungroup() %>%
rename(`Positive Samples` = Positive_Samples)
write.csv(dataFilters, file = file.path(dir, "forSS","data_filters.csv"), row.names = FALSE)

View(Model_selection)
#format table for the document
out <- Model_selection %>%
dplyr::select(-weight, -`(Intercept)`) %>%
mutate_at(vars(covars,"year","offset(logEffort)"), as.character) %>%
mutate(across(c("logLik","AICc","delta"), round, 1)) %>%
replace_na(list(county = "Excluded", wave = "Excluded")) %>%
mutate_at(c(covars,"year","offset(logEffort)"), funs(stringr::str_replace(.,"\\+","Included"))) %>%
rename(`Effort offset` = `offset(logEffort)`, `log-likelihood` = logLik) %>%
rename_with(stringr::str_to_title,-AICc)
View(out)
write.csv(out, file = file.path(dir, "forSS", "model_selection.csv"), row.names = FALSE)
