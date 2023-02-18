##################################################################################################
# Contains SQL connections to read in the following recreational length data
#	Recreational Cooperative Research CPFV (started 2022)
# Deb WV lengths
# Miller historical length
# Ally et al.
# Collins and Crooke
#                         for Copper Rockfish 2023
#	
# Written by Melissa Monk
#
##################################################################################################

library(here)
library(ggplot2)
library(DBI)

#Chantel dir
#dir <- "C:/Assessments/2023/copper_rockfish_2023/data/rec_bds"
#Melissa dir
# dir <- "S:/copper_rockfish_2023/data/rec_bds"
dir <- file.path(here(), "data", "rec_bds")
setwd(dir)
dir.create(file.path(dir, "plots"))


#load in cooperative recreational length comps
# connect to database with windows authentication
con <- DBI::dbConnect(odbc::odbc(),
                      driver = "SQL Server",
                      server = "pinniger",
                      database = "SWFSC_Groundfish_Collections",
                      Trusted_Connection = "yes"
)

coop2 <- dbSendQuery(con, "SELECT SampleID, CaptureMonth, 
                Cooperative_Recreational_CPFV_Collections.Vessel, ForkLengthMM, 
                Sex, Maturity,Stage, District, TripType, area
      FROM Cooperative_Recreational_CPFV_Collections
	      inner join Cooperative_Recreational_CPFV_Collections_Port_Lookup 
	  on Cooperative_Recreational_CPFV_Collections.Vessel =  Cooperative_Recreational_CPFV_Collections_Port_Lookup.Vessel
      where speciescode = 'copp' and forklengthmm is not null")
coop <- dbFetch(coop2)
dbClearResult(coop2)


load("crfss_bds_filtered.rdata")
crfs_cpfv <- subset(crfss_bds, mode == 'cpfv')


#add in the matching columns for the mode and year to the crfs data
coop$mode <- 'cpfv'
coop$year <- 2022
coop$lengthcm <- coop$ForkLengthMM/10

aggregate(lengthcm~mode+area, crfs_cpfv, quantile)
aggregate(lengthcm~mode+area+TripType, coop, quantile)


ggplot(crfs_cpfv, aes(lengthcm, fill = area, color = area)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_grid(area~.) 
ggsave(filename = file.path(dir, "plots", "crfs_length_dist_by_mode_area.png"),
       width = 10, height = 7)



save(rec_coop_bds, file = file.path(dir, "rec_bds", "rec_bds_filtered.rdata"))



