################################################################################
# Contains SQL connections to read the onboard observer data through 2019
#                         copper rockfish assessment
#	
# Melissa Monk 4/8/23
################################################################################

rm(list = ls(all = TRUE))
graphics.off()
library(here)
library(ggplot2)
library(dplyr)
library(DBI)
library(here)
library(glue)

speciesNODC = 8826010108

dir <- file.path(here(), "data", "rec_indices", "crfs_cpfv_onboard")
setwd(dir)

# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "pinniger"

# onboard connection
con_onboard <- DBI::dbConnect(odbc::odbc(),
                                driver = db_driver,
                                server = swc_server,
                                database = "Monk_index_manuscript",
                                Trusted_Connection = "yes"
)


#Get the SQL queries for onboard catch and effort information
query_onboard_catch <- glue::glue_sql(
  "SELECT * from DriftCatch", 
  .con = con_onboard
)
query_onboard_effort <- glue::glue_sql(
  "SELECT * from DriftEffort_forR", 
  .con = con_onboard
)

onboard_catch <- dbGetQuery(con_onboard, query_onboard_catch)
onboard_effort <- dbGetQuery(con_onboard, query_onboard_effort)



#Get the number of available samples by year
#looking to check on 2018 and 2019 in districts 5 and 6
samples_year_district <- onboard_effort %>%
  group_by(YEAR, DISTRICT) %>%
  tally() %>%
  tidyr::pivot_wider(names_from=DISTRICT, values_from = n)
View(samples_year_district)




#Combine and save data with target species
target_catch <- onboard_catch %>%
  filter(NODC == speciesNODC) %>%
  unite("ID", ASSN, LOCNUM, remove = FALSE) %>%
  dplyr::select(-RowID, -ASSN, -LOCNUM) 

effort <- onboard_effort %>%
  unite("ID", ASSN, LOCNUM, remove = FALSE) %>%
  dplyr::select(-RowID, -ASSN, -LOCNUM)

#join catch and effort
onboard_data <- left_join(effort, target_catch, by = c("ID"))

#why are there too many trips?
duplicate_drifts <- onboard_data %>%
  group_by(ID) %>%
  tally() %>%
  filter(n>1)
#there area a couple of trips with multiple records of the same species
#remove these

onboard_data <- onboard_data %>%
  filter(!ID %in% duplicate_drifts$ID) %>%
  mutate(gis90depthft = -START90mDEPTH * 3.281,
        gis2depthft = -START2mDEPTH * 3.281) %>%
  rename(year = YEAR,
         month = MONTH,
         county = CNTY,
         reef = REEFID_orig,
         reef.dist = ReefDist,
         district = DISTRICT,
         obsang = OBSANG,
         fishtime = FISHTIME,
         waterarea = WaterArea_NMFS,
         dbase = DBASE.x, 
         kept = KEPT,
         discd = DISCD,
         cdfw.block = CDFWBlockID,
         depth1ft = DEPTH1ft,
         depth2ft = DEPTH2ft) %>%
  mutate(kept = ifelse(is.na(kept), 0, kept),
         discd = ifelse(is.na(discd), 0, discd)) %>%
  mutate(number.fish = kept +  discd) %>%
  mutate_at(vars(month, county, district, reef, cdfw.block), as.factor) %>%
  mutate(effort = obsang * fishtime,
         cpue = number.fish/effort)

#save files for north and south of conception
save(onboard_data, file = file.path(dir, "onboard.RData"))

