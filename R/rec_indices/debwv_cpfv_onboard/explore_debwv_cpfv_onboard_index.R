################################################################################
# Contains SQL connections to read Deb's onboard data
#                         2023 assessments
#	
# Written by Melissa Monk
# Note - only Monterey samples in 1987
################################################################################

rm(list = ls(all = TRUE))
graphics.off()
library(here)
library(ggplot2)
library(dplyr)
library(DBI)
library(here)
library(glue)


dir <- file.path(here(), "data", "rec_indices", "debwv_cpfv_onboard")
setwd(dir)

# connect to all of the databases with windows authentication
db_driver <- "SQL Server"
swc_server <- "pinniger"

# DebWV database connection
con_debwv <- DBI::dbConnect(odbc::odbc(),
                            driver = db_driver,
                            server = swc_server,
                            database = "CDFW_CPFV_Onboard_1987_1998",
                            Trusted_Connection = "yes"
)


#Get the SQL queries for deb's data - had to remove comments for R - they are in the SQL script
query_debwv <- glue::glue_sql(
  "SELECT LOCATION_TRUNC.TRIP_ID AS ASSN,			
		   LOCATION_TRUNC.FSHNG_STOP AS LOCNUM,		
		   CNTY,								
		   DISTRICT,								
		   MPA,										
		   ASSESS_AREA,								
		   SUM_OBSANG AS OBSANG,			
		   SUM_FISHTIME AS FISHTIME,				
		   ANGHRS,
		   BOAT.percent_groundfish,
		   YEAR(TRPDATE) AS YEAR,					
		   MONTH(TRPDATE) AS MONTH,					
     		   DEPTH =   CASE					
					 WHEN AVG_MINDEPTH IS NOT NULL AND AVG_MAXDEPTH IS NOT NULL				
					   THEN ((AVG_MINDEPTH+AVG_MAXDEPTH)/2)
					 WHEN AVG_MINDEPTH IS NULL AND AVG_MAXDEPTH IS NOT NULL
					   THEN   AVG_MAXDEPTH
					 WHEN AVG_MINDEPTH IS NOT NULL AND AVG_MAXDEPTH IS  NULL
			 			THEN AVG_MINDEPTH
					 WHEN AVG_MINDEPTH IS NULL AND AVG_MAXDEPTH IS NULL
			 			THEN ((luLOC_CODE.MINFMS*6)+(luLOC_CODE.MAXFMS*6))/2
			 			END,
			  DEPTHTYPE = CASE 
					 WHEN AVG_MINDEPTH IS NOT NULL AND AVG_MAXDEPTH IS NOT NULL 
					   THEN 'AVERAGE'
					 WHEN AVG_MINDEPTH IS NULL AND AVG_MAXDEPTH IS NOT NULL
					   THEN  'MAXDEPTH'
					 WHEN AVG_MINDEPTH IS NOT NULL AND AVG_MAXDEPTH IS  NULL
			 			THEN 'MINDEPTH'
					 WHEN AVG_MINDEPTH IS NULL AND AVG_MAXDEPTH IS NULL
			 			THEN 'LOC_CODE'
			 			END, 	
			 	
		   REEFID,							
		   SUM(case when CDFGSP = 2308 then NUMENC else 0 end) as NUMENC,  
		   SUM(case when CDFGSP = 2308 then KEPT else 0 end) as KEPT,     
           SUM(case when CDFGSP = 2308 then DISCD else 0 end) as DISCD     


	FROM LOCATION_TRUNC  
		   INNER JOIN BOAT		 ON LOCATION_TRUNC.Trip_ID = BOAT.Trip_ID
		   INNER JOIN LUPORT	 ON BOAT.PORT = LUPORT.PORT
		   INNER JOIN LULOC_CODE ON LULOC_CODE.LOC_CODE = LOCATION_TRUNC.LOC_CODE
		   INNER JOIN LUREEF	 ON LOCATION_TRUNC.LOC_CODE = LUREEF.LOC_CODE
		   LEFT  JOIN CATCHES	 ON LOCATION_TRUNC.Trip_ID = CATCHES.Trip_ID 
									and LOCATION_TRUNC.FSHNG_STOP = CATCHES.FSHNG_STOP
		   LEFT  JOIN LUSPECIES ON CATCHES.CDFGSP = LUSPECIES.CDFGSP

	WHERE 
			  FSHNGTYP_GFISH>0						
		  and SUM_FISHTIME is not NULL			
		  and SUM_OBSANG is not NULL			
		  and LOCATION_TRUNC.LOC_CODE is not NULL	
		  and ReefID is not NULL				
	 
			
    GROUP BY		
			  LOCATION_TRUNC.TRIP_ID,
			  LOCATION_TRUNC.FSHNG_STOP, 
			  SUM_OBSANG,  
			  CNTY,
			  AVG_MINDEPTH,
			  AVG_MAXDEPTH,
			  ASSESS_AREA, 
			  SUM_FISHTIME, 
			  TRPDATE, 
 			  BOAT.percent_groundfish,
 			  REEFID,
 			  DISTRICT,
 			  MPA,
 			  MINFMS,
			  MAXFMS, 
			  ANGHRS

   ORDER BY NUMENC desc ", 
  .con = con_debwv
)

debwv <- dbGetQuery(con_debwv, query_debwv)
dbDisconnect(con_debwv)

#Look at how many copper rockfish retained and discarded
debwv %>%
  group_by(YEAR) %>%
  summarise(kept = sum(KEPT),
            discard = sum(DISCD))
#very few discards so this will be a retained fish only index

#look at the percent of trips and groundfish
ggplot(debwv %>% filter(KEPT>0), aes(percent_groundfish, KEPT)) + 
  geom_point()

save(debwv, file = "debwv_copper_data.R")
