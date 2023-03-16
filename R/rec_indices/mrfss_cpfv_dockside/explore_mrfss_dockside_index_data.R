################################################################################
# Contains SQL connections to read the MRFSS dockside data
#                         2023 assessments
#	
# Written by Melissa Monk
#
################################################################################

rm(list = ls(all = TRUE))
graphics.off()
library(here)
library(ggplot2)
library(dplyr)
library(DBI)
library(here)
library(glue)


dir <- file.path(here(), "data", "rec_indices", "mrfss_cpfv_dockside")
setwd(dir)

# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "pinniger"

# MRFSS connection
con_mrfss <- DBI::dbConnect(odbc::odbc(),
                                driver = db_driver,
                                server = swc_server,
                                database = "gf_assessment",
                                Trusted_Connection = "yes"
)


#Get the SQL queries for mrfss south
query_mrfss_south <- glue::glue_sql(
  "SELECT * from RecFIN_TripData_SoCA_Species", 
  .con = con_swc_gfish
)
#Get the SQL queries for each of the databases
query_mrfss_north <- glue::glue_sql(
  "SELECT * from RecFIN_TripData_NoCA_Species", 
  .con = con_swc_gfish
)
mrfss_south <- dbGetQuery(con_mrfss, query_mrfss_south)
mrfss_north <- dbGetQuery(con_mrfss, query_mrfss_north)

dbDisconnect(con_mrfss)
#--------------------------------------------
# We have to move trips from south to north
# from SLO 
#--------------------------------------------

##select only the non-species columns you need
mrfss_north = mrfss_north %>%
  filter(ST==6) %>%
  dplyr::select(-c(F1, SEQ, id_assign, id_sampler, id_date, TRPDATE, id_int
                   ,DATE1,SP_CODE_num,SP_CODE,PRIM1,PRIM2,AREA_X,AREA,                        
                   DIST,INTSITE,HRSF,CNTRBTRS,NUM_TYP3,NUM3,SUB_REG,FSHINSP,
                   LNGTH,WGT,ADD_HRS,T_LEN,OLD_WGT,WGT_FLAG,GEAR, old_len,
                   TRIP,COUNT,CATCH,NewTRIP,NewCOUNT,CATCHno10,NewTRIP_SP,
                   TRIP_SP,New_anglers,New_Hrsf,New_anglerXhrs,ANGLERS, ONBOARD,
                   MODE_FX, MODE_F, ST)) %>%
  rename(YEAR = Year) %>%
  filter(YEAR<2000) 

mrfss_south = mrfss_south %>%
  filter(ST == 6) %>%
  dplyr::select(-c(SEQ, id_assign, id_sampler, id_date, TRPDATE, id_int
                   ,DATE1,SP_CODE_num,SP_CODE,PRIM1,PRIM2,AREA_X,AREA,                        
                   DIST,INTSITE,HRSF,CNTRBTRS,NUM_TYP3,NUM3,SUB_REG,FSHINSP,
                   LNGTH,WGT,ADD_HRS,T_LEN,OLD_WGT,WGT_FLAG,GEAR, old_len,
                   TRIP,COUNT,CATCH,NewTRIP,NewCOUNT,CATCHno10,NewTRIP_SP,
                   TRIP_SP,New_anglers,New_Hrsf,New_anglerXhrs,ANGLERS, ONBOARD,
                   MODE_FX, MODE_F, ST, TempID)) %>%
  filter(YEAR<2000) 

#change spiney to spiny
mrfss_south = mrfss_south %>% rename('Spiny_Dogfish_Shark' = 'Spiney_Dogfish_Shark')
mrfss_north = mrfss_north %>% rename('Spiny_Dogfish_Shark' = 'Spiney_Dogfish_Shark')

#Make the data long to combine - then separate appropriate to north and south
mrfss_south_long <- mrfss_south %>%
  pivot_longer(cols = -c(id_code, YEAR, WAVE, CNTY, ANGLERxHRS))

mrfss_north_long <- mrfss_north %>%
  pivot_longer(cols = -c(id_code, YEAR, WAVE, CNTY, ANGLERxHRS))

mrfss_ca <- full_join(mrfss_south_long, mrfss_north_long)
#confirmed all rows are there

#assign counties and remove anything later than 1999
mrfss_ca = mrfss_ca %>% 
  mutate(area = 
           case_when(CNTY %in% c(1,13, 15, 23, 41,45,53,75, 77, 79,81,87,97) ~  'north',  #northern
                     CNTY %in% c(37,59,73,83,111) ~  'south')) #southern

#This moves the SLO trips to the north
#244 trips moved
mrfss_new_north <- mrfss_ca %>%
  filter(area == 'north') %>%
  pivot_wider(names_from = name, values_from = value)

mrfss_new_south <- mrfss_ca %>%
  filter(area == 'south') %>%
  pivot_wider(names_from = name, values_from = value)


save(mrfss_new_north, file = file.path(dir, "north", "mrfss_new_north.RData"))
save(mrfss_new_south, file = file.path(dir, "south", "mrfss_new_south.RData"))
