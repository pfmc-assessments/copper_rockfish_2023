#############################################################################
## DebVW index of abundance for copper rockfish for the 2023 assessment
## Data pulled from Deb's database on pinniger in the explore file
## Melissa Monk
##        Note - only Monterey sampled in 1987
#############################################################################

rm(list=ls(all=TRUE))
graphics.off()

#libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)

data_dir <- "S:/copper_rockfish_2023"
dir <- file.path(data_dir, "data", "rec_indices", "debwv_cpfv_onboard")
setwd(dir)
out.dir <- file.path(getwd(),"plots")
#load data
load("debwv_copper_data.R")
#####################################################################################
#Data filter dataframe 
data_filters <- data.frame(
  matrix(vector(), 12, 4,
         dimnames=list(c(), c("Filter", "Description",
                             "Samples", "Positive_Samples"))),
                          stringsAsFactors=F)
data_filters$Filter[1:2] = c("All")
filter.num = 1
###################################################################################

#Read in data
dim(subset(debwv, KEPT>0))

#Add to filter dataframe
data_filters$Samples[filter.num] = dim(debwv)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(debwv, KEPT>0))[1]
data_filters$Filter[filter.num] = "All"
data_filters$Description[filter.num] = 'None'
filter.num = filter.num + 1

#Remove numenc  = null
debwv <- subset(debwv, NUMENC!='NULL')

##########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(debwv)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(debwv, KEPT>0))[1]
data_filters$Filter[filter.num] = "No catch"
data_filters$Description[filter.num] = 'Remove no catch trips'
filter.num = filter.num + 1
#########################################################################################################

##############################Remove 1987 (only sampled Monterey Bay)
#Remove 1987 and also depth <80 fm
debwv  <- debwv %>%
  filter(YEAR>1987) %>%
  filter(DEPTH<80)

#########################################################################################################

#Add to filter dataframe
data_filters$Samples[filter.num] = dim(debwv)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(debwv, KEPT>0))[1]
data_filters$Filter[filter.num] = "Only sampled Monterey"
data_filters$Description[filter.num] = 'Remove 1987 and depths >80fm'
filter.num = filter.num + 1
#########################################################################################################


#mutate columns to numeric
debwv <- debwv %>% mutate_at(c('KEPT', 'DISCD', 'NUMENC','REEFID'), as.numeric)
dim(subset(debwv, KEPT>0))

#Read in SuperReef info - assignments for copper, but see how they match
reef_info <- read.csv('SuperReefs.csv',header=T)
reef_info_needed <- reef_info %>% 
                  dplyr::select(ReefID,COPPDebRe, Area)  %>%
                  rename(REEFID=ReefID,SuperReef = COPPDebRe) %>%
                  group_by(REEFID, SuperReef) %>%
                  summarise(tot_Area = sum(Area)) 
#Join super reefs to main data
debwv <- dplyr::left_join(debwv, reef_info_needed, by = 'REEFID')


#look at distributions of kept over superreefs
debwv %>% filter(KEPT>0) %>% group_by(SuperReef) %>% summarise(kept = sum(KEPT))


#assign a column for vermilion presence
debwv<- debwv %>%  mutate(spp_present = 
                        case_when(NUMENC>0 ~ T,
                                  TRUE ~ F))

#Add a column for wave - bimonthly sampling
debwv <- debwv %>%
  mutate(WAVE = case_when(MONTH %in% c(1,2) ~ 1,
                          MONTH %in% c(3,4) ~ 2,
                          MONTH %in% c(5,6) ~ 3,
                          MONTH %in% c(7,8) ~ 4,
                          MONTH %in% c(9,10) ~ 5,
                          MONTH %in% c(11,12) ~ 6)) %>%
  mutate_at(vars(WAVE), as.factor)

#Create catch per angler house
debwv <- debwv %>%
  mutate(CPUE = NUMENC/(AVG_OBSANG*(FISHTIME/70)))
summary(debwv$CPUE)

#make a copy of the original to reference
dat <- debwv
###################################################
#Look at district
dat %>% 
  group_by(DISTRICT) %>%
  count(spp_present)

with(dat, table(YEAR,DISTRICT))  
#District 5 and 6 not too much data


##############################FISHTIME
summary(dat$FISHTIME)
fishtime_quantile <- quantile(dat$FISHTIME, seq(0,1,.025))
fishtime_quantile

png(filename = paste0(out.dir,'/Histogram of time fished.png'), width = 6, height = 6, units = "in", res = 600)
hist(dat$FISHTIME, breaks=50)
dev.off()
#A lot of really small fish times, but I checked the data sheet
#logically anything less than ~7 minutes isn't a representative drift
#Not removing the upper quantile bc it can be a combo of a number of drifts
#Removes 5% of the data
dat <- dat %>%
      filter(FISHTIME >= 6,
      FISHTIME<=218) #, FISHTIME < fishtime_quantile[40])

#########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, KEPT>0))[1]
data_filters$Filter[filter.num] = "Time fished"
data_filters$Description[filter.num] = 'Remove upper and lower 2.5% of time fished; keep 6-218 minutes'
filter.num = filter.num + 1
#########################################################################################################

summary(dat$AVG_OBSANG)
OBSANG_quantile <- quantile(dat$AVG_OBSANG, seq(0,1,.025))
OBSANG_quantile

png(filename = paste0(out.dir,'/Histogram of observed anglers.png'), 
width = 6, height = 6, units = "in", res = 600)
hist(dat$AVG_OBSANG, breaks=50)
dev.off()

dat <- dat %>%
      filter(AVG_OBSANG >= 4,
      AVG_OBSANG <= 15) #, FISHTIME < fishtime_quantile[40])

#########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, KEPT>0))[1]
data_filters$Filter[filter.num] = "Observed anglers"
data_filters$Description[filter.num] = 'Remove upper and lower 2.5% of observed anglers; keep 4-15'
filter.num = filter.num + 1
#########################################################################################################




###############################DEPTH
#Look at depths and create depth bins
#BATHY DATA IN METERS, OBSERVER DATA IN FEET, MANAGEMENT IN FATHOMS 
#Convert feet to fathoms for the analyses


#Look at depth by SuperReef
#png(filename = paste0(out.dir,'/Histogram of depth (fm) by reef.png'), width = 6, height = 6, units = "in", res = 600)
ggplot(dat, aes(DEPTH, fill=SuperReef)) +
  geom_histogram() + xlab("Depth (fm)") +ylab("Number of drifts")
ggsave(file.path(out.dir,"depthfm_byreef.png"), width = 7, height = 7)

#Look at positive depth
dat %>% filter(KEPT>0) %>% do(data.frame(quantile(.$DEPTH, seq(0,1,.01))))
#Look at depths with no copper
dat  %>% filter(KEPT==0) %>% do(data.frame(quantile(.$DEPTH, seq(0,1,.01))))

#REMOVE anything deeper than  meters (~70 fathoms) retains 99% of positive drifts and 95% of all drifts
 dat <- dat %>%
   filter(DEPTH<=56,
          DEPTH >= 8) 
# ########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, KEPT>0))[1]
data_filters$Filter[filter.num] = "Depth"
data_filters$Description[filter.num] = 'Retain drifts between 8-56 fm'
filter.num = filter.num + 1
########################################################################################################
#percent groundfish filter
 quantile(dat$percent_groundfish, seq(0,1,.025), na.rm=T)

dat <- dat %>% filter(percent_groundfish>=.715)
# ########################################################################################################
#Add to filter dataframe
data_filters$Samples[filter.num] = dim(dat)[1]
data_filters$Positive_Samples[filter.num] = dim(subset(dat, KEPT>0))[1]
data_filters$Filter[filter.num] = "Target"
data_filters$Description[filter.num] = 'Retain trips with at least 71.5% groundfish catch (97.5% of trips)'
filter.num = filter.num + 1
########################################################################################################


#png(filename = paste0(out.dir,'/Histogram of depth fathoms after filter.png'), width = 6, height = 6, units = "in", res = 600)
hist(dat$DEPTH)
#dev.off()

ggplot(dat, aes(x=SuperReef, y = DEPTH, colour = SuperReef)) + 
geom_boxplot() +ylab("Depth (fm)")
ggsave(file = file.path(getwd(), "depth_by_reef.png"), width = 7, height = 7)




dat = dat %>%
      mutate(DEPTH_bin = cut(DEPTH,
                       breaks = c(10,20,30,40,50,60))) %>%
           mutate_at(vars(DEPTH_bin), as.factor)
           
           

#Look at distribution of data by depth bins
with(dat, table(spp_present,DEPTH_bin))

#dat = droplevels(dat)


###############################################################
###################################################
#Look at reefs and species presence
with(dat, table(spp_present, SuperReef))

#Look at reefs and species presence
with(dat, table(spp_present, DISTRICT))

#Find reefs that were fished, and encountered vermilion
pos_copp_reefs <- dat %>% filter(KEPT>0) %>% dplyr::select(SuperReef) %>% unique()
reefs_fished <- dat %>% dplyr::select(SuperReef) %>% unique()
#find reefs never fished
reef_fished_info <- reef_info_needed %>%
               mutate(reef_pos_copp = case_when(SuperReef %in% pos_copp_reefs$SuperReef ~ 'COPP_present')) %>%
               mutate(reef_fished = case_when(SuperReef %in% reefs_fished$SuperReef ~ 'Fished'))


#Count how many years a super reef was visited
reef_years <- dat %>%
              group_by(SuperReef) %>%
              summarise(n_years = n_distinct(YEAR))

#Count how many drifts on a super reef
reef_drifts <- dat %>%
              group_by(SuperReef) %>%
              count(name="n_drifts")

#join sample sizes to the same data frame
with(dat, table(YEAR, SuperReef))
with(dat %>% filter(KEPT>0), table(YEAR, SuperReef))
#x11();with(reef_sample_size,plot(n_years, n_drifts))

#############FILTER
 
#Look at the time spent on each reef
reef_time <- dat %>%
            group_by(SuperReef) %>%
            summarise(timefished = sum(FISHTIME),totalanghrs = sum(ANGHRS)) 

reef_areas <- reef_info %>% dplyr::select(ReefID, COPPDebRe, Area) %>%
              group_by(COPPDebRe) %>%
              summarise(total_area = sum(Area))
colnames(reef_areas)[1] <- 'SuperReef'


reef_sample_size <- inner_join(reef_years, reef_drifts)
reef_sample_size <- inner_join(reef_sample_size, reef_time)
reef_sample_size <- inner_join(reef_sample_size,reef_areas)
write.csv(reef_sample_size, file = file.path(getwd(),"reef_sample_size.csv"), 
          row.names = FALSE)



###################################################################
                        
#check for temporal coverage
with(dat, table(SuperReef))
with(dat, table(SuperReef,YEAR))
round(with(subset(dat,KEPT>0), table(SuperReef))/with(dat, table(SuperReef)),2)


#check for temporal coverage of positives
pos <- subset(dat, KEPT>0)
with(pos, table(SuperReef,YEAR))


###NEED TO MODIFY THIS BC now using superreef and not
#Combine some of the super reefs
dat$SuperReef = as.factor(dat$SuperReef)



 dat$MegaReef  <- dat$SuperReef %>% droplevels()
 dat = dat %>% mutate(MegaReef = case_when(
               SuperReef %in% c('1_OR_SF') ~ 'OR_SF',
               SuperReef %in% c('2_SF_MossLanding')~ 'SF_MossLanding',
               SuperReef %in% c('10_MossLanding_BigSur') ~ 'MossLanding_BigSur',
               SuperReef %in% c('13_SLOCnty_Morro', '15_Morro_Conception') ~ 'SLO_Conception'))
                              
                              
levels(dat$MegaReef) <- c("OR_SF" ,"SF_MossLanding", "MossLanding_BigSur", "SLO_Conception")                             

with(dat, table(MegaReef,YEAR))
with(dat, table(MegaReef))

with(subset(dat, KEPT>0),table(MegaReef))/with(dat, table(MegaReef))
percent_pos_depth <- round(with(subset(dat, KEPT>0),table(YEAR,MegaReef))/with(dat, table(YEAR,MegaReef)),2)
write.csv(percent_pos_depth, file = file.path(getwd(), "percent_pos_depth.csv"), 
          row.names = FALSE)
#plot CPUE by year again with combined reef
CPUE_reef_year <- dat %>%
  group_by(MegaReef,YEAR) %>%
  summarise(mean_CPUE = mean(CPUE))

ggplot(CPUE_reef_year, aes(x=YEAR, y = mean_CPUE, color = MegaReef, group=MegaReef)) +
  geom_point() + geom_line(linewidth = 1) + xlab("Year") + 
  ylab("Average CPUE") + scale_color_viridis_d()
ggsave(file = file.path(getwd(), "cpue_by_reef.png"), height = 7, width = 7)

#Look at depths again
round(with(subset(dat, NUMENC>0),table(YEAR,DEPTH_bin))/with(dat, table(YEAR,DEPTH_bin)),2)

#Join MegaReef to ReefInfo table
MegaReef_info <- dat %>% dplyr::select(SuperReef,MegaReef) %>% unique()
aa = left_join(reef_areas, reef_fished_info)
bb = left_join(aa, reef_sample_size)
COPP_reef_info = left_join(bb, MegaReef_info)




#Arithmetic index
CPUE_year <-  dat %>% group_by(YEAR) %>% summarise(Avg_CPUE = mean(CPUE)) %>% as.data.frame()
print(CPUE_year)

#Plot the average cpue by year and reef
png(filename = paste0(out.dir,'/Average CPUE by Year and Region.png'), width = 6, height = 4, 
    units = "in", pointsize = 10, res=300)
with(dat, interaction.plot(YEAR, MegaReef, CPUE,
                           col=1:7, lty=1, lwd=2, ylab="CPAH", ylim=c(0,.2), 
                           legend=F))
legend("topright", legend=c("V1", "V2",
                            "V3","V4"), lty=1, col=1:7)
dev.off()

levels(dat$MegaReef) = c('V1','V2','V3','V4')
#-------------------------------------------------------------------------------
#Airthmetic area-weighted index
CPUE_year_reef <- dat %>% group_by(MegaReef,YEAR) %>% summarise(Avg_CPUE = mean(CPUE)) %>% as.data.frame()

MegaReef_Area <- COPP_reef_info %>% 
                group_by(MegaReef) %>%  
                summarise(reef_area = sum(tot_Area)) %>%
                mutate(Percent_area = reef_area/sum(reef_area)) %>%
                filter(!is.na(MegaReef))

write.csv(MegaReef_Area,paste0(out.dir,'/COPP_megareefs.csv'))
CPUE_year_reef = left_join(CPUE_year_reef, MegaReef_Area)

MegaReef_Area <- reef_info
Arithmetic_weighted_index = CPUE_year_reef %>% 
  mutate(Weighted_val = Percent_area*Avg_CPUE) %>%
  group_by(YEAR) %>%
  mutate(Final_index = sum(Weighted_val)) %>%
  dplyr::select(YEAR, Final_index) %>% unique()

ggplot(Arithmetic_weighted_index, aes(YEAR, Final_index)) + geom_line()
ggsave(file = file.path(out.dir,'/Area weighted arithmetic mean.png'))


save(dat, data_filters, COPP_reef_info, file = file.path(dir,'COPP_filtered_data.RData'))

save.image(paste0(getwd(),'/Filtered_data_DebWV_onboard.RData'))




#################################################3
##additional looks at the data

pos <- subset(dat, NUMENC>0 )
with(pos, table(REEFID))
with(pos, table(YEAR))
with(dat, table(REEFID))
with(dat, table(YEAR, MegaReef))
summary(dat$percent_groundfish)

 quantile(dat$percent_groundfish, seq(0,1,.1), na.rm=T)

pos_reefs <- pos %>%
dplyr::select(REEFID) %>%
unique()
dat1 <- subset(dat, REEFID %in% pos_reefs$REEFID)
pos <- subset(dat1, NUMENC>0)
with(pos, table(YEAR))
with(dat1, table(YEAR))
with(dat1, table(YEAR, MegaReef))
summary(dat1$percent_groundfish)


ggplot(dat %>% filter(NUMENC>0), aes(x = CPUE, y = FISHTIME)) + 
geom_point()


summary(dat$ANGHRS)
summary(dat$FISHTIME)
summary(dat$AVG_OBSANG)
