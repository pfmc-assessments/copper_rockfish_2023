#########################################################################
### Process the MRFSS PC data for an index of abundance
### Apply Stephens-MacCall
### Copper assessment 2023
### Have to move SLO samples from the south to the north
### Melissa Monk
#########################################################################

rm(list = ls(all = TRUE))
graphics.off()
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)
options(scipen=999)

#species and area identifiers - eventually put in function
targetSpecies <- 'Copper_Rockfish'
speciesName = "copper"
modelArea = 'north'

#set working directory
dir <- file.path(here(), "data", "rec_indices", "mrfss_cpfv_dockside",modelArea)
setwd(dir)
out.dir <- getwd()
#plots.dir <- glue::glue(out.dir,"/plots")

#Read in data
load(paste0("mrfss_new_",modelArea,".RData"))

if(modelArea == "north"){ dat <- mrfss_new_north
} else { dat <- mrfss_new_south
}

# turn correct column name into target
colnames(dat)[which(colnames(dat) == targetSpecies)] = "Target" 

# Data filter dataframe
filter.num <- 1
dataFilters <- data.frame(matrix(vector(), 10, 4,
                                 dimnames = list(c(), c(
                                   "Filter", "Description", "Samples",
                                   "Positive_Samples"
                                 ))
),
stringsAsFactors = F
)



# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("All data")
dataFilters$Description[filter.num] <- c("California data prior to 2000")
dataFilters$Samples[filter.num] <- length(unique(dat$id_code))
dataFilters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1

with(dat, table(CNTY, YEAR))

#remove counties not well sampled; sparse sampling
dat <- dat %>% 
  filter(!CNTY %in% c(13, 15, 23, 75, 77)) 

# Remove Humboldt and Del Norte
dataFilters$Filter[filter.num] <- c("County")
dataFilters$Description[filter.num] <- c("Remove counties with sparse samples (13,15,23,75,77)")
dataFilters$Samples[filter.num] <- length(unique(dat$id_code))
dataFilters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1

#remove 1993 and 1994 in the north
dat <- dat %>%
  filter(!(area == "north" & YEAR %in% c(1993, 1994)))

# Remove 1993/1994 in the north
dataFilters$Filter[filter.num] <- c("Northern sampling")
dataFilters$Description[filter.num] <- c("Remove 1993 and 1994 in northern CA")
dataFilters$Samples[filter.num] <- length(unique(dat$id_code))
dataFilters$Positive_Samples[filter.num] <- dim(subset(dat, Target > 0))[1]
filter.num <- filter.num + 1

#--------------------------------------------------
# Stephens-MacCall filtering - for now...
#
#
#--------------------------------------------------
#SM filtering ; make copies
area.dat.sm <- area.dat <- dat

#get the columns you need to keep for the index
area.dat.sm <- area.dat.sm %>%
  dplyr::select(id_code, CNTY, WAVE, YEAR, ANGLERxHRS, Target, area)


#Subset to columns needed to look at species comps
#keep year, species,and area, id_code
area.dat = area.dat %>%
  dplyr::select(-c(CNTY, WAVE, ANGLERxHRS, area))

# set the threshold to select the minimum number of trips species 
# is present - remove rare species and limits the species you use for SM
#set at 5% for now
spp_cutoff =  .05*dim(area.dat)[1]
spp_cutoff
area.dat = area.dat %>% 
  select_if(function(x) length(which(x>0)) > spp_cutoff)

# Remove any trips with no catches in subsetted species
area.dat =  area.dat %>% mutate(sum=Reduce("+",.[2:(ncol(area.dat)-2)]))
area.dat = subset(area.dat, sum>0)
dim(subset(area.dat, Target>0))

#make a copy of the data for SM filtering
dockside.sm = area.dat %>% 
  dplyr::select(-c('sum'))

# reduce the area.dat.sm to the remaining trips
area.dat.sm <- area.dat.sm %>%
  filter(id_code %in% dockside.sm$id_code)

# replace values >0 in dataframe with 1's, except for the YEAR, id_code column
dockside.sm = dockside.sm %>% mutate_at(vars(-one_of('YEAR', 'id_code')), 
                                  ~case_when(.x> 0~ 1,
                                             T ~ 0))


###############################
###Stephens-MacCall filtering
###############################
my.lm = glm(formula = Target ~  . -YEAR - id_code, 
            data = dockside.sm, 
            family=binomial)

# GLM summary - check to see standard errors are all reasonable
summary(my.lm)

# Add fitted values (predicted) to fish and write out 
area.dat.sm = cbind(fitted.values(my.lm), area.dat.sm) 
colnames(area.dat.sm)[1] = 'fitted.vals'   

# create a dataframe of #threshold values to get the total number of trips by 
# threshold and positive
sm.decision <- as.data.frame(seq(0,1,by=0.01)) 
colnames(sm.decision)[1] <- "threshold"
sm.decision$trips.kept <- 9999
sm.decision$pos.trips.kept <- 9999
sm.decision$falseneg.kept <- 9999

#move to dplyr eventually
#the decision just needs to be made on the 0's since we are assuming
#we will keep all of the positive values
for (i in 1:length(sm.decision$threshold) ) {
  sm.decision$trips.kept[i] = dim(subset(area.dat.sm,fitted.vals > 
                                               sm.decision$threshold[i]))[1]
  sm.decision$pos.trips.kept[i]  = 
    dim(subset(area.dat.sm, Target > 0 & fitted.vals > 
                 sm.decision$threshold[i]))[1]
  
 sm.decision$falseneg.kept[i] = 
   dim(subset(area.dat.sm, Target == 0 & fitted.vals > 
                sm.decision$threshold[i]))[1]
  
}


sm.decision <- sm.decision %>%
  mutate(percent.pos = (pos.trips.kept/trips.kept)*100) %>%
#get the percent of trips retained for both total and pos
  mutate(fraction.total = trips.kept/length(dockside.sm$id_code),
         fraction.pos = pos.trips.kept/dim(dockside.sm %>%filter(Target>0))[1]) 

#plot total trips vs number retained with threshold values
sm.decision %>%
  dplyr::select(threshold, falseneg.kept, pos.trips.kept) %>%
  pivot_longer(cols = - threshold) %>%
ggplot(aes(x = threshold, y = value, color = name))  + 
  geom_line() + 
  xlab("Fitted probability") + ylab("Trips kept") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) 
ggsave(filename = file.path(dir, "plots" ,"mrfss_dockside_sm_falseneg.png"),
       width = 10, height = 8)



#Plot Stephens-MacCall barplot with CIs
# plot regression coefficients with asymptotic 95% confidence intervals

bin.coefs <- data.frame(summary(my.lm)$coef[,1:2])
bin.coefs <- bin.coefs[order(-bin.coefs$Estimate),]
ggplot(bin.coefs, aes(x=factor(rownames(bin.coefs), levels=rownames(bin.coefs)[order(Estimate)]), y=Estimate)) + 
  geom_bar(position=position_dodge(), stat="identity", fill='blue') +
  geom_errorbar(aes(ymin=Estimate-1.96*Std..Error, ymax=Estimate+1.96*Std..Error),
                width=.6,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(y = "Binomial GLM Coef. w/ 95% C.I.", x = "Species") +
  coord_flip()
ggsave(filename = file.path(dir, "plots" ,"mrfss_dockside_sm_coeff.png"),
       width = 10, height = 8)


#Andi's code for finding the best threshold
# sum the target species presence
obs = sum(dockside.sm$Target)
# set the threshold
thresh=seq(0,1,by=0.01)
thresh.effect = thresh
thresh.count  = thresh
thresh.percent = thresh

# for each level of the threshold
for ( i in 1:length(thresh) ) {
  # number of observed trips - number of prediced trips
  thresh.effect[i] = abs(obs - sum(fitted.values(my.lm) > thresh[i]))
  thresh.count[i]  = sum(fitted.values(my.lm) > thresh[i])
}
# percent of trips selected at each threshold level 
thresh.percent = (thresh.count/nrow(dockside.sm))*100 
# combines data of threshold level, difference between obs and predicted, and number of trips selected
mythresh = cbind(thresh, thresh.effect, thresh.count)
best = min(thresh.effect)
best.thresh = thresh[thresh.effect == best]
best.thresh

area.dat.sm <- area.dat.sm %>%
filter((fitted.vals > best.thresh & Target == 0) | Target > 0)

# SM Filter
dataFilters$Filter[filter.num] <- c("Stephens-MacCall")
dataFilters$Description[filter.num] <- c("Remove predicted false negatives")
dataFilters$Samples[filter.num] <- length(unique(area.dat.sm$id_code))
dataFilters$Positive_Samples[filter.num] <- dim(subset(area.dat.sm, Target > 0))[1]
filter.num <- filter.num + 1

save(area.dat.sm, dataFilters, best.thresh, file = glue(
  out.dir, "/mrfss_cpfv_dockside_data_for_GLM.RData"
))

save.image(paste0(out.dir,"/SM_processed_MRFSSdockside.RData"))












