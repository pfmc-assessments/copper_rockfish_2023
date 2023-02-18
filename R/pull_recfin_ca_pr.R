##################################################################################################
# Contains SQL connections to read in the CA recreational PR data
# From RecFIN and save it to an rdata file
#	
# Written by Melissa Monk
# February 2023
##################################################################################################

library(odbc) 
library(ggplot2)
library(tidyr)
#Chantel dir
#dir <- "C:/Assessments/2023/copper_rockfish_2023/data/rec_bds"
#Melissa dir
dir <- "S:/copper_rockfish_2023/data/rec_indices"
setwd(dir)
dir.create(file.path(dir, "plots"))

cdfw_pr <- read.csv("cdfw_pr.csv")


# taking forever!!!
channel <- dbConnect(odbc::odbc(),dsn="PacFIN",uid="mmonk",pwd="mm2pf4db")
dbListTables(channel, schema = "RECFIN_MARTS")
dbListFields(channel, 'RECFIN_MARTS.COMPREHENSIVE_REC_SAMPLE')
startTime <- Sys.time()
# get the CDFW PR data
Rec_PR <- dbGetQuery(channel,
                     "SELECT SAMPLE_ID, ANGLER_ID, LOCATION_ID, CATCH_ID,
                     RECFIN_YEAR, RECFIN_MONTH, RECFIN_DAY, FISHERY_CODE,
                     STATE_NAME, AGENCY ,RECFIN_PORT_CODE, RECFIN_PORT_NAME,
                     RECFIN_DISTRICT_NAME,
                     RECFIN_SUBREGION_CODE, RECFIN_SUBREGION_NAME,
                     AGENCY_TRIP_TYPE_CODE, RECFIN_TRIP_TYPE_NAME,
                     AGENCY_WATER_AREA_CODE, AGENCY_WATER_AREA_NAME,
                     AGENCY_FISHED_AREA_NAME,
                     AGENCY_MODE_CODE, RECFIN_MODE_CODE, RECFIN_MODE_NAME,
                     NUMBER_HOURS_FISHED, NUMBER_OF_ANGLERS,
                     RECFIN_SPECIES_CODE, AGENCY_SPECIES_CODE, SPECIES_NAME,
                     PACFIN_SPECIES_CODE, NUMBER_RELEASED,
                     NUMBER_RELEASED_ALIVE, NUMBER_RELEASED_DEAD,
                     NUMBER_KEPT_OBSERVED, NUMBER_KEPT_UNOBSERVED, COUNTY
                     FROM RECFIN_MARTS.COMPREHENSIVE_REC_SAMPLE
                     WHERE RECFIN_MODE_CODE = 7 and
                     AGENCY = 'C' ")
endTime<- Sys.time()
