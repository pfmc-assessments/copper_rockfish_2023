##################################################################################################
# Contains SQL connections to read in the following recreational length data
#	Recreational Cooperative Research CPFV (started 2022) - coppers/quills only
# Deb WV lengths
# Don rec. port sampling
# Miller historical length
# Ally et al.
# Collins and Crooke
#                         for 2023 assessments
#	
# Written by Melissa Monk
#
##################################################################################################

library(here)
library(ggplot2)
library(dplyr)
library(DBI)
library(here)
library(glue)

#Chantel dir
#dir <- "C:/Assessments/2023/copper_rockfish_2023/data/rec_bds"
#Melissa dir
#dir <- "S:/copper_rockfish_2023/data/rec_bds"
dir <- file.path(here(), "data", "rec_bds")
setwd(dir)

#Set your species
sppAbbr <- "COPP"  #You will need to know your species' appreviation
cdfwSpp <- c(2308, 2347) #These are the 4 digit CDFW species codes, copper has 2 b/c whitebelly
nodcSpp <- c(8826010108, 8826010161)
#-------------------------------------------------------------------------------
# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "pinniger"

# SWFSC groundfish collections; 2022 recreational cooperative 
con_swc_gfish <- DBI::dbConnect(odbc::odbc(),
                      driver = db_driver,
                      server = swc_server,
                      database = "SWFSC_Groundfish_Collections",
                      Trusted_Connection = "yes"
)

# CDFW CRFS PCO lengths with trip type that includes retained and discarded fish
con_CDFW_PCO <- DBI::dbConnect(odbc::odbc(),
                               driver = db_driver,
                               server = swc_server,
                               database = "CDFW_CPFV_Onboard_2015_on",
                               Trusted_Connection = "yes"
)

# DebWV retained and discarded fish: 
# Chantel has access to this table
con_DebWV <- DBI::dbConnect(odbc::odbc(),
                               driver = db_driver,
                               server = swc_server,
                               database = "CDFW_CPFV_Onboard_1987_1998",
                               Trusted_Connection = "yes"
)

# Commercial port samples taking recreational retained lengths
con_rec_DonP <- DBI::dbConnect(odbc::odbc(),
                          driver = db_driver,
                          server = swc_server,
                          database = "California_Recreational_Fish_from_Commercial_Samplers",
                          Trusted_Connection = "yes"
)

# CA historical recreational data - includes Collins/Crooke, Ally, Miller
# Chantel can access these data
con_ca_rec_hist <- DBI::dbConnect(odbc::odbc(),
                           driver = db_driver,
                           server = swc_server,
                           database = "California_Historical_Recreational_Data",
                           Trusted_Connection = "yes"
)

#-------------------------------------------------------------------------------
#SQL queries
#every dataset needs the columns
# mode, area, lengthcm, year, dataset, sex, maturity

#Get the SQL queries for each of the databases
query_swc_coop <- glue::glue_sql(
  "SELECT SampleID, CaptureMonth, c.Vessel,
                    ForkLengthMM/10 as lengthcm, sex, maturity, stage, district,
                    TripType, DurationType, speciescode as species, DB_NAME() as db
                 FROM Cooperative_Recreational_CPFV_Collections c
	               INNER JOIN  Cooperative_Recreational_CPFV_Collections_Port_Lookup d
	               ON c.Vessel = d.Vessel
                 WHERE speciescode = {sppAbbr} and forklengthmm is not null", 
  .con = con_swc_gfish
)
swc_coop <- dbGetQuery(con_swc_gfish, query_swc_coop)
dbDisconnect(con_swc_gfish)

# add in the matching columns for the mode and year to the crfs data
swc_coop$mode <- 'cpfv'
swc_coop$year <- 2022
swc_coop <- swc_coop %>%
     mutate(area = 
           ifelse(district < 3, "south", "north"))
save(swc_coop, file = file.path(dir, "swc_rec_coop_bds.rdata"))

#-------------------------------------------------------------------------------
#CDFW DebWV onboard observer lengths; retained and discarded

query_DebWV <- glue::glue_sql(
  "SELECT LengthID, COMMON as Species, fate, FISH_TL as lengthcm_tl, 
  LENGTHS.TRIP_ID, DISTRICT as district, year(TRPDATE) as year, DB_NAME() as db 
  FROM LENGTHS
  INNER JOIN luSPECIES on LENGTHS.CDFGSP=luSPECIES.CDFGSP
  LEFT JOIN BOAT on BOAT.TRIP_ID=LENGTHS.TRIP_ID
  LEFT JOIN luPORT on BOAT.PORT=luPORT.PORT
  where luSPECIES.CDFGSP IN ({vals*}) and year(TRPDATE) is not null",
  vals = cdfwSpp,
  .con = con_DebWV
)

DebWV <- dbGetQuery(con_DebWV, query_DebWV)
dbDisconnect(con_DebWV)
DebWV$mode <- "cpfv"
DebWV$sex <- NA
DebWV$maturity <- NA
save(DebWV, file = file.path(dir, "DevWV_bds.rdata"))

#-------------------------------------------------------------------------------
#Don P commercial port samplers sampling recreational catch
query_rec_DonP <- glue::glue_sql(
  "SELECT rec_fish.SAMPLE_NO, FISH_NO, species, sex, FLENGTH as lengthcm,
  maturity, WEIGHT, CAL_PORT, YEAR(SAMPLE_DATE) as year,CDFG_BLOCK, 
  DB_NAME() as db
  FROM rec_fish
  LEFT JOIN rec_samples on rec_fish.SAMPLE_NO = rec_samples.SAMPLE_NO
  WHERE species = {sppAbbr}",
  .con = con_rec_DonP)
DonP <- dbGetQuery(con_rec_DonP, query_rec_DonP)
dbDisconnect(con_rec_DonP)
DonP$mode <- "cpfv"
DonP$area <- "north"
save(DonP, file = file.path(dir, "DonP_rec_bds.rdata"))


#-------------------------------------------------------------------------------
# CDFW historical rec length data: Ally
# Chantel can access
query_ally <- glue::glue_sql(
  "SELECT Autonumber, L_DATE_YY+1900 as year, 
  Complex_Name as complex, Landing_Name as landing, district,
  L_SPECODE as species,   L_KEPTREL as fate, 
  L_LENGTH1/10 as lengthcm_tl, L_TALLY_1 as length_tally, DB_NAME() as db
  FROM Ally_SoCal_CPFV_Lengths_84_to_89 j
  LEFT JOIN Ally_SoCal_CPFV_Port_Complex k 
  on j.L_COMPLEX=k.Complex and j.L_LANDING=k.Landing
  WHERE L_SPECODE in ({vals*})",
  vals = cdfwSpp,
  .con = con_ca_rec_hist)
ally_rec <- dbGetQuery(con_ca_rec_hist, query_ally)
ally_rec$mode <- "cpfv"
ally_rec$area <- "south"
ally_rec$sex <- NA
ally_rec$maturity <- NA
save(ally_rec, file = file.path(dir, "ally_rec_bds.rdata"))

#-------------------------------------------------------------------------------
# CDFW historical rec length data: Collins and Crooke
# Chantel can access
query_collins_70s <- glue::glue_sql(
  "SELECT a.tripID, SpCode as species, Length/10 as lengthcm_tl, 
  Catch as length_tally, date, year(date) as year, blockNo, district, 
  complex, ports, DB_NAME() as db
  FROM CollinsCrooke_SoCal_CPFV_1970s_Catch a
  INNER JOIN CollinsCrooke_SoCal_CPFV_1970s_Trips b ON a.TripID = b.TripID 
  LEFT JOIN CollinsCrooke_SoCal_CPFV_Port_Complex c ON b.Complex=c.ComplexID 
  WHERE SpCode in  ({vals*})",
  vals = cdfwSpp,
  .con = con_ca_rec_hist
)
collins_70s <- dbGetQuery(con_ca_rec_hist, query_collins_70s)

query_collins_80s <- glue::glue_sql(
  "SELECT  e.tripID, SpCode as species, Length/10 as lengthcm_tl, 
   Count as length_tally, date, year(date) as year, 
       blockNo, district, complex, ports, DB_NAME() as db
  FROM CollinsCrooke_SoCal_CPFV_1980s_Length e
  INNER JOIN CollinsCrooke_SoCal_CPFV_1980s_Trips f ON e.TripID = f.TripID
  LEFT JOIN CollinsCrooke_SoCal_CPFV_Port_Complex c ON f.Complex=c.ComplexID
  WHERE SpCode in  ({vals*})",
  vals = cdfwSpp,
  .con = con_ca_rec_hist
)
collins_80s <- dbGetQuery(con_ca_rec_hist, query_collins_80s)

collins_rec <- rbind(collins_70s, collins_80s)
collins_rec$mode <- "cpfv"
collins_rec$area <- "south"
collins_rec$sex <- NA
collins_rec$maturity <- NA
save(collins_rec, file = file.path(dir, "collinsCrooke_rec_bds.rdata"))

#-------------------------------------------------------------------------------
# CDFW historical rec length data: Miller
# Chantel can access
query_miller <- glue::glue_sql(
  "SELECT NODC as species, NUMBER as length_tally,  Length/10 as lengthcm_tl, g.COAST_DIST, district, County_Names as county, 
   MODE_short as mode, year, DB_NAME() as db
   FROM Miller_NorCal_Lengths_59_to_72 g
   INNER JOIN Miller_NorCal_MODE_Codes h ON g.MODE = h.MODE
   LEFT JOIN Miller_NorCal_COAST_DIST_Codes i ON g.COAST_DIST = i.COAST_DIST
   WHERE NODC in ({vals*})",
   vals = nodcSpp,
  .con = con_ca_rec_hist
)
miller_rec <- dbGetQuery(con_ca_rec_hist, query_miller)
miller_rec$sex <- NA
miller_rec$maturity <- NA
miller_rec <- miller_rec %>%
  mutate(area = 
           ifelse(district < 3, "south", "north"))
save(miller_rec, file = file.path(dir, "miller_rec_bds.rdata"))




#-------------------------------------------------------------------------------
# #CDFW PCO data (also in RecFIN, but pulling here as well for other purpose
# con_CDFW_PCO
# 
# 
# pco_bds2 <- dbSendQuery(con, "SELECT 
#       PCO_Biodata.RecFinSurveyEventID, PCO_BioData.SampleNum, 
#       Species, Length, DurationType, year, district
#   FROM [CDFW_CPFV_Onboard_2015_on].[dbo].[PCO_Biodata]
#   inner join PCO_Distinct_Trips on 
#   PCO_Distinct_Trips.RecFinSurveyEventID = PCO_Biodata.RecFinSurveyEventID
#   WHERE species = 'rfcop' and fate is null")
# pco_bds <- dbFetch(pco_bds2)
# dbClearResult(pco_bds2)
# pco_bds$mode <- 'cpfv'
# pco_bds$dataset <- 'pco'
# pco_bds <- pco_bds %>%
#   mutate(area = 
#            ifelse(district < 3, "south", "north")) %>%
#   mutate(TripType = ifelse(DurationType == "Overnight", "MultiDay", "SingleDay")) %>%
#   mutate(lengthcm = Length/10)
# 
# save(pco_bds, file = file.path(dir, "rec_pco2015_2019_bds.rdata"))




















