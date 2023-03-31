#########################################################################
### Load and explore the ifiles
### Use the ifiles to re-create the PR1 interview level data for 2004-2015
### Also load in the 2015-2020 data from CDFW that we pieced back together
### from individual annual files
### Copper assessment 2023
### Melissa Monk
#########################################################################
rm(list = ls(all = TRUE))
graphics.off()
library(readxl)
library(dplyr)
library(tidyr)
library(here)
#-------------------------------------------------------------------------------
dir <-file.path(here(),"data","rec_indices","crfs_pr_dockside")
#setwd(dir)
setwd("S:/copper_rockfish_2023/data/rec_indices/crfs_pr_dockside")
speciesNODC = 8826010108
newCdfwSpecies = 'RFCOP'

#load the i1 and i3 files from 2004-2015
load("ifiles.RData")

#load a species code look up file -  use NODC
speciesCodes <- read.csv("species_lookup.csv")

#get the number of interviews from the i1 file
#look at survey field and filter
summary(as.factor(i1file$survey))
summary(as.factor(i1file$MODE_FX)) #already all 7
summary(as.factor(i1file$AREA_X))
summary(i1file$CNTRBTRS)
summary(as.factor(i1file$F_BY_P))
with(i1file, table(F_BY_P, YEAR))
#F_BY_P
#1 = yes, all sampler examined catch is from a single angler
#2 = no, more than one anglers contributed to the sampler examined catch (i.e., group catch)
#8 = No catch examined by the sampler

#the original filters below remove anything from 2012 to 2015
#pull just those years to figure out why
later_years<- i1file %>%
  filter(YEAR>2011)
summary(as.factor(later_years$F_BY_P))
summary(as.factor(later_years$survey))
with(later_years, table(F_BY_P, YEAR))
#F_BY_P not used after 2013
with(later_years, table(survey, YEAR))
#plenty of pr1
with(later_years, table(CNTRBTRS, YEAR))
#no issue there
with(later_years, table(NUM_TYP3, YEAR))
#num_typ3 used only until 2013

i1samples <- i1file %>%
  filter(survey =="CRFS-PR1") %>% 
  filter(CNTRBTRS>0) %>% # need at least 1 contributor
#  filter(!F_BY_P %in% c(8)) %>% #indicates no catch examined by the sampler
#  filter( !is.na(NUM_TYP3)) %>% #must have at least 1 type 3 record; this also removes is.na(F_BY_P)
  dplyr::select( ID_CODE,YEAR, district, prim1, prim2, GEARA, GEARB,#select columns to keep
                 CNTRBTRS, NUM_TYP3,
                 LEADER, PRT_CODE, MODE_FX, AREA, AREA_X,
                 DIST, CNTY, INTSITE, GEAR, 
                 PARTY, FIRST, WAVE,  F_BY_P, SALMON, ISLAND,
                  CRFS, survey, month, week)

#look at AREA_X and Island
with(i1samples, table(AREA_X, ISLAND))
#islands are all in areax=1
summary(as.factor(i1samples$AREA_X))
##3005 samples with NA for area
#    1      2      5   NA's 
#169563  19479   9272   3005 

##IF year < 2014 remove anything with FY_BY_P == 8
fbyp8 <- i1samples %>%
  filter(YEAR<2014,
         F_BY_P==8, 
         NUM_TYP3==0) %>%
  unique()

#remove the samples identified in the above table
i1samples <- i1samples %>%
  filter(!ID_CODE %in% fbyp8$ID_CODE)


#summary of unique interviews thus far
trip_summary <- i1samples %>%
  mutate_at(vars(district), as.factor) %>%
  dplyr::select(ID_CODE, YEAR, district, CRFS) %>%
  unique() %>%
  group_by(YEAR, district) %>%
  tally() %>%
  pivot_wider(names_from = district, values_from = n)

#get unique trips
trips <- i1samples %>%
  dplyr::select(ID_CODE) %>% #, YEAR, district, CRFS) %>%
  unique()  
#201,319 - indicates that isamples are now "trips"
#-------------------------------------------------------------------------------
#Collapse i3 records to id_codes in i1samples
#filter to species of interest
i3samples <- i3file %>%
  dplyr::select(ID_CODE, FSHINSP, SP_CODE) %>%
  filter(SP_CODE == speciesNODC) %>%
  filter(ID_CODE %in% i1samples$ID_CODE) %>%
  unique()
sum(i3samples$FSHINSP)
#### 34,395 total coppers observed

#join tables
pr_data <- left_join(i1samples, i3samples)

#suspect trips where two sets of copper rockfish available from the same
#group of anglers, one may be a typo? 
#remove these trips identified in aa
aa <- pr_data %>%
  group_by(ID_CODE) %>%
  tally() %>%
  filter(n>1)
#remove the two trips
#rename area_x and assign month
pr_data <- pr_data %>%
  filter(!ID_CODE %in% aa$ID_CODE) %>%
  mutate(area_x = case_when(
    AREA_X == 1 ~ "Nearshore (less than 3 mi)",
    AREA_X == 2 ~ "Offshore (greater than 3 mi)",
    AREA_X == 5 ~ "Bay/estuary/harbor")) %>%
  mutate(month = as.numeric(stringr::str_sub(ID_CODE,10,11)))

#get just the coppers
coppers <- pr_data %>%
  filter(FSHINSP>0)
#all coppers in area_x = 1

#-------------------------------------------------------------------------------
#####read newer PR data from 2015-2020
PR_catch_all <- read.csv("C:/Users/melissa.monk/Documents/CDFW data/PR_data_stitched_together/PR_Catch.csv")
PR_effort_all <- read.csv("C:/Users/melissa.monk/Documents/CDFW data/PR_data_stitched_together/PR_Effort.csv")
PR_header <- read.csv("C:/Users/melissa.monk/Documents/CDFW data/PR_data_stitched_together/PR_Header.csv")

# Merge ineeded header into to effort
PR_header_info <- PR_header %>% 
  dplyr::select(RefNum, ASSN, SurveyDate, 
                District, CNTY, Site, Port)

#add date columns
PR_header_info$Date2 <- anytime::anydate(PR_header_info$SurveyDate)
PR_header_info$YEAR <- as.numeric(format(PR_header_info$Date2, "%Y"))
PR_header_info$MONTH <- as.numeric(format(PR_header_info$Date2, "%m"))

#join effort and header info
PR_effort <- inner_join(PR_effort_all, PR_header_info)
summary(as.factor(PR_effort$YEAR))
#2015  2016  2017  2018  2019  2020 
#38461 33492 34023 32301 34993 28546 

# Turn columns into numeric
PR_effort <- PR_effort %>% 
  mutate_at(vars(AnglersTotal, AnglersUnlic), as.numeric)

#filter effort without anglers
#filter on waterarea
summary(PR_effort$AnglersTotal)
PR_effort <- PR_effort %>%
  filter(!is.na(AnglersTotal)) %>%
  mutate(ID = paste0(RefNum,"_",SampleNum)) 
summary(PR_effort$AnglersTotal)
#summary of the water area
summary(as.factor(PR_effort$X1stTargetWaterArea))

#Now subset the trips to just those where there is observed catch 
#Catch table
PR_catch <- PR_catch_all %>%
  mutate(ID = paste0(RefNum,"_",SampleNum)) %>%
  filter(Kept > 0)

# Remove any catch record that wasn't "observed"
catch_samples <- PR_catch %>%
  dplyr::select(ID) %>%
  unique() %>%
  mutate(ObsCatch = 1)

#Subset the PR_trips to only those with observed catch
PR_effort <- inner_join(PR_effort, catch_samples)

#PR trip summary
PR_trips <- PR_effort %>%
  group_by(YEAR, District) %>%
  tally() %>%
  pivot_wider(names_from = District, values_from=n)

PR_catch_target <- PR_catch %>%
  filter(Species == newCdfwSpecies,
         Kept>0)
#41,568 coppers kept

PR_data <- left_join(PR_effort, PR_catch_target, by = "ID")
#-------------------------------------------------------------------------------
#USE the i file data for 2004-2014
#USE the PR_raw data files for 2015-2019

#start with the pr_data 175,100 rows
i_data <- pr_data %>%
  filter(YEAR < 2015) %>%
  rename(ID = ID_CODE,
         year = YEAR,
         kept = FSHINSP,
         species = SP_CODE,
         geara = GEARA,
         gearb = GEARB,
         anglers = CNTRBTRS,
         mode = MODE_FX,
         county = CNTY,
         site = INTSITE,
         wave = WAVE) %>%
  filter(!is.na(prim1),
         !prim1 %in% c('NFOTH','NFCOM','NFPC6','NFSHL')) %>% #remove records with no primary1 indicator
  mutate_at(vars(prim1), as.numeric)
#primary species are NODC numbers

#rename columns #61,798 rows
raw_data <- PR_data %>%
  rename(anglers = AnglersTotal,
         prim1 = X1stTargetSpecies,
         prim2 = X2ndTargetSpecies,
         area_x = X1stTargetWaterArea,
         area = X2ndTargetWaterArea,
         geara = X1stTargetGear,
         gearb = X2ndTargetGear,
         kept = Kept,
         month = MONTH,
         year = YEAR,
         district = District,
         county = CNTY,
         site = Site,
         species = Species)
  #primary species are abbreviations   

#species codes for i_data 
ispeciesInfo <- speciesCodes %>%
  dplyr::select(NODC, CommonName) %>%
  rename(prim1 = NODC,
         prim1Common = CommonName) 
 ispeciesInfo2 <- speciesCodes %>%
  dplyr::select(NODC, CommonName) %>%
  rename(prim2 = NODC,
         prim2Common = CommonName)      
#species codes for raw_data
rawspeciesInfo <- speciesCodes %>%
  dplyr::select(SpeciesAbbrv, CommonName) %>%
  rename(prim1 = SpeciesAbbrv,
         prim1Common = CommonName) 
rawspeciesInfo2 <- speciesCodes %>%
  dplyr::select(SpeciesAbbrv, CommonName) %>%
  rename(prim2 = SpeciesAbbrv,
         prim2Common = CommonName) 

#I removed the alphabetic NODC codes from species lookup
i_data <- inner_join(i_data, ispeciesInfo)
raw_data <- inner_join(raw_data, rawspeciesInfo)
raw_data <- left_join(raw_data, rawspeciesInfo2)
#attempt to get the primary 2 species into common names
summary(as.factor(i_data$prim2))
prim2_iremove = c("CLAMS", "LOBST", "NFSHL")
i_data<- i_data %>%
mutate(prim2 = replace(prim2, prim2 %in% prim2_iremove, NA)) %>%
mutate_at(vars(prim2), as.numeric)
#add in prim2 commonnames
 i_data <- left_join(i_data, ispeciesInfo2)

#figure out which species abbreviations we don't have common names for
#SHINS
#SHOFF
#SQDMK - no nodc number so removed
#SCPRO
#CLMPO
#SHRMP
#SCPUS

#combine the two files
col_names = c('ID', 'year', 'month', 'kept', 'species', 'geara', 
              'gearb', 'anglers', 'area_x', 'county', 'site', 
              'district', 'prim1', 'prim2','prim1Common', 'prim2Common')
all_pr_data <- rbind(
  i_data[, col_names],
  raw_data[, col_names]
)
all_pr_data <- all_pr_data %>%
  mutate(kept = ifelse(is.na(kept),0, kept))

save(all_pr_data, file = "all_pr_data.RData")
