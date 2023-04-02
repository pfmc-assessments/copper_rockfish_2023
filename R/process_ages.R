##################################################################
#
#     Process growth ages for copper rockfish assessment 
#           
##################################################################

library(here)

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}
source(file.path(user_dir, "R", "get_caal.R"))
dir <- here("data", "ages", "formatted_age_files")

# Read in the growth ages
load(file.path(dir, "abrams_ages.rdata"))
load(file.path(dir, "ccfrp_ages.rdata"))
load(file.path(dir, "cdfw_ages.rdata"))
load(file.path(dir, "coop_ages.rdata"))
load(file.path(dir, "pearson_ages.rdata"))
load(file.path(dir, "crfs_ages.rdata"))

# Reset the directory to a lower level folder
dir <- here("data")

length_bins <- seq(10, 54, 2)
age_bins <- 1:50

# CCFRP ========================================================================
# There are 4 unsexed fish that will not be included
ccfrp_north <- get_caal(
  data = ccfrp_ages[ccfrp_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "ccfrp", 
  partition = 0)

write.csv(ccfrp_north, 
  file = file.path(dir, "ages", "forSS", "ccfrp_caal_north.csv"),
  row.names = FALSE)  

ccfrp_south <- get_caal(
  data = ccfrp_ages[ccfrp_ages$area == "south", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "ccfrp", 
  partition = 0)

write.csv(ccfrp_south, 
  file = file.path(dir, "ages", "forSS", "ccfrp_caal_south.csv"),
  row.names = FALSE) 

# COOP Ages ====================================================================
# Need to identify which area the ages are from by vessel

coop_ages$area <- "south"
coop_ages$area[coop_ages$vessel %in% c("Legacy", "Salty Lady", "Sea Wolf")] <- "north"
coop_ages$sex[!coop_ages$sex %in% c("F","M")] <- "U"

# Remove the "stage" column because the get_caal function fails to find the age column
coop_ages <- coop_ages[, -which(colnames(coop_ages) == "stage")]

# There are 23 unsexed fish in the north area and 6 in the south and 
# there are 42 carcass length fish (40 in the north and 2 in the south).

coop_north <- get_caal(
  data = coop_ages[coop_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "coop", 
  partition = 0)

write.csv(coop_north, 
          file = file.path(dir, "ages", "forSS", "coop_cpfv_caal_north.csv"),
          row.names = FALSE)  

coop_south <- get_caal(
  data = coop_ages[coop_ages$area == "south", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "coop", 
  partition = 0)

write.csv(coop_south, 
          file = file.path(dir, "ages", "forSS", "coop_cpfv_caal_south.csv"),
          row.names = FALSE) 

tmp <- coop_ages[, -which(colnames(coop_ages) == "length_cm")]

carcass_north <- get_caal(
  data = tmp[tmp$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "coop", 
  partition = 0)

write.csv(pearson_south, 
          file = file.path(dir, "ages", "forSS", "coop_cpfv_carcass_caal_north.csv"),
          row.names = FALSE) 

# Pearson Research =============================================================
# All fish are male or female
pearson_north <- get_caal(
  data = pearson_ages[pearson_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "pearson", 
  partition = 0)

write.csv(pearson_north, 
          file = file.path(dir, "ages", "forSS", "pearson_caal_north.csv"),
          row.names = FALSE)  

pearson_south <- get_caal(
  data = pearson_ages[pearson_ages$area == "south", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "ccfrp", 
  partition = 0)

write.csv(pearson_south, 
          file = file.path(dir, "ages", "forSS", "pearson_caal_south.csv"),
          row.names = FALSE) 


# Abrams Research =============================================================

# All fish are male or female

abrams_north <- get_caal(
  data = abrams_ages[abrams_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "abrams", 
  partition = 0)

write.csv(abrams_north, 
          file = file.path(dir, "ages", "forSS", "abrams_caal_north.csv"),
          row.names = FALSE)  

# CDFW Special Collections =====================================================

# One unsexed fish

cdfw_north <- get_caal(
  data = cdfw_ages[cdfw_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "cdfw", 
  partition = 0)

write.csv(cdfw_north, 
          file = file.path(dir, "ages", "forSS", "cdfw_special_collections_caal_north.csv"),
          row.names = FALSE)  

# CRFS =========================================================================
# There are some NA records
crfs_ages <- crfs_ages[!is.na(crfs_ages$age), ]

# All records are for PR north with 3 unsexed records 

pr_north <- get_caal(
  data = crfs_ages[crfs_ages$area == "north" & crfs_ages$mode == "PR", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "private", 
  partition = 0)

write.csv(pr_north, 
          file = file.path(dir, "ages", "forSS", "crfs_private_caal_north.csv"),
          row.names = FALSE)  



  