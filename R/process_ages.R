##################################################################
#
#     Process growth ages for copper rockfish assessment 
#           
##################################################################

library(ggplot2)
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
load(file.path(dir, "commercial_ages.rdata"))
load(file.path(dir, "historical_rec_ages.rdata"))
load(file.path(dir, "unknown_historical_ages.rdata"))

crfs_ages <- crfs_ages[!is.na(crfs_ages$age), ]
crfs_non_random <- crfs_ages[crfs_ages$year == 2021, ]
crfs_ages <- crfs_ages[crfs_ages$year != 2021, ]

# Reset the directory to a lower level folder
dir <- here("data")
load(file.path(dir, "wcgbt", "wcgbt_ages_with_area.rdata"))
wcgbt <- bio_orig[!is.na(bio_orig$Age), ]
colnames(wcgbt) <- tolower(colnames(wcgbt))
wcgbt$program <- "NWFSC WCGBT"

# Fix up program names for table
abrams_ages$program = "Abrams"
coop_ages$program <- "SWFSC/CPFV Coop."
cdfw_ages$program <- "CDFW"


length_bins <- seq(10, 54, 2)
age_bins <- 0:50

#===============================================================================
# Add all age sources that will be used in a growth fleet to a single df
# The CCFRP, CRFS and historical rec., and commercial ages will be link to fleets.
#===============================================================================

col_names <- c('program', 'year', 'area', 'sex', 'length_cm', 'age')
growth_ages <- rbind(
  abrams_ages[, col_names],
  pearson_ages[, col_names],
  cdfw_ages[, col_names],
  coop_ages[, col_names],
  wcgbt[, col_names],
  crfs_non_random [crfs_ages$year == 2021, col_names]
  #unknown_ages[, col_names] <- Leave these out for now since not sure if they link rec. fleet
)

growth_ages <- growth_ages[!is.na(growth_ages$length_cm), ]
# There are ages ranging between 0-50 years of age in this df 
#        F   M   U
#north 467 493  28
#south 728 725  59

ggplot(growth_ages) + geom_bar(aes(x = age, color = sex)) + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "growth_ages_by_area.png"), width = 7, height = 7)
ggplot(growth_ages) + geom_bar(aes(x = age, color = program)) + facet_grid(area~.)
ggsave(file = file.path(dir, "plots", "growth_ages_by_area_program.png"), width = 7, height = 7)

samples <- growth_ages %>%
  group_by(year, area, program) %>%
  reframe(
    n_ages = length(age)
  )
colnames(samples) <- c("Year", "area", "Source", "Ages")
write.csv(samples[samples$area == "north", colnames(samples) != "area"],
          file =file.path(dir,  "ages", "forSS", "north_growth_age_samples.csv"),
          row.names = FALSE)

write.csv(samples[samples$area == "south", colnames(samples) != "area"],
          file =file.path(dir,  "ages", "forSS", "north_growth_age_samples.csv"),
          row.names = FALSE)

growth_north <- get_caal(
  data = growth_ages[growth_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "growth", 
  partition = 0)

write.csv(growth_north, 
          file = file.path(dir, "ages", "forSS", "growth_caal_north.csv"),
          row.names = FALSE) 

growth_south <- get_caal(
  data = growth_ages[growth_ages$area == "south", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "growth", 
  partition = 0)

write.csv(growth_south, 
          file = file.path(dir, "ages", "forSS", "growth_caal_south.csv"),
          row.names = FALSE) 

# There are 40 carcass lengths in the north and 2 in the south
tmp <- coop_ages[, c("area", "year", "sex", "carcass_length_cm", "age")]
carcass_north <- get_caal(
  data = tmp[tmp$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "growth", 
  partition = 0)

write.csv(carcass_north, 
          file = file.path(dir, "ages", "forSS", "growth_carcass_caal_north.csv"),
          row.names = FALSE) 

#===============================================================================
# CCFRP 
#===============================================================================

ccfrp_north <- get_caal(
  data = ccfrp_ages[ccfrp_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "ccfrp", 
  partition = 0)

write.csv(ccfrp_north, 
  file = file.path(dir, "survey_indices", "ccfrp", "north", "forSS", "ccfrp_caal_north.csv"),
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
  file = file.path(dir, "survey_indices", "ccfrp", "north", "forSS", "ccfrp_caal_south.csv"),
  row.names = FALSE) 

#===============================================================================
# CRFS 
#===============================================================================
# There are some NA records

# All records are for PR north with 3 unsexed records 
samples <- crfs_ages %>%
  group_by(year, area, mode) %>%
  reframe(samples = length(age))
colnames(samples) <- c("Year", "area", "Fleet", "Ages")
write.csv(samples[samples$area == "north", colnames(samples) != "area"],
          file =file.path(dir,  "ages", "forSS", "crfs_north_age_samples.csv"),
          row.names = FALSE)

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

#===============================================================================
# Historical Recreational Samples
#===============================================================================



#===============================================================================
# Commercial 
#===============================================================================
commercial_ages <- commercial_ages[!is.na(commercial_ages$age), ]

samples <- commercial_ages %>%
  group_by(year, area) %>%
  reframe(
    Fleet = "Commercial Dead",
    samples = length(age))
colnames(samples) <- c("Year", "area", "Fleet", "Ages")
write.csv(samples[samples$area == "north", colnames(samples) != "area"],
          file =file.path(dir,  "ages", "forSS", "com_dead_north_age_samples.csv"),
          row.names = FALSE)
write.csv(samples[samples$area == "south", colnames(samples) != "area"],
          file =file.path(dir,  "ages", "forSS", "com_dead_south_age_samples.csv"),
          row.names = FALSE)

com_north <- get_caal(
  data = commercial_ages[commercial_ages$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = 1, 
  partition = 0)

write.csv(com_north, 
          file = file.path(dir, "ages", "forSS", "commercial_age_caal_north.csv"),
          row.names = FALSE)  

com_south <- get_caal(
  data = commercial_ages[commercial_ages$area == "south", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = 1, 
  partition = 0)

write.csv(com_south, 
          file = file.path(dir, "ages", "forSS", "commercial_age_caal_south.csv"),
          row.names = FALSE) 

#===============================================================================
# WCGBT 
#===============================================================================
wcgbt <- wcgbt[, !colnames(wcgbt) %in% c("age_years", "ageing_lab")]

wcgbt_north <- get_caal(
  data = wcgbt[wcgbt$area == "north", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "wcgbt", 
  partition = 0)

write.csv(wcgbt_north, 
          file = file.path(dir, "ages", "forSS", "by_source", "wcgbt_caal_north.csv"),
          row.names = FALSE)  

wcgbt_south <- get_caal(
  data = wcgbt[wcgbt$area == "south", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "wcgbt", 
  partition = 0)

write.csv(wcgbt_south, 
          file = file.path(dir, "ages", "forSS", "by_source", "wcgbt_caal_south.csv"),
          row.names = FALSE) 

#===============================================================================
# COOP Ages 
#===============================================================================

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
          file = file.path(dir, "ages", "forSS", "by_source", "coop_cpfv_caal_north.csv"),
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
          file = file.path(dir, "ages", "forSS", "by_source", "coop_cpfv_caal_south.csv"),
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
          file = file.path(dir, "ages", "forSS", "by_source", "coop_cpfv_carcass_caal_north.csv"),
          row.names = FALSE) 

#===============================================================================
# Pearson Research 
#===============================================================================
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
          file = file.path(dir, "ages", "forSS", "by_source", "pearson_caal_north.csv"),
          row.names = FALSE)  

pearson_south <- get_caal(
  data = pearson_ages[pearson_ages$area == "south", ], 
  len_bins = length_bins, 
  age_bins = age_bins, 
  month = 7, 
  ageing_error = 1,
  fleet = "pearson", 
  partition = 0)

write.csv(pearson_south, 
          file = file.path(dir, "ages", "forSS", "by_source", "pearson_caal_south.csv"),
          row.names = FALSE) 

#===============================================================================
# Abrams Research
#===============================================================================
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
          file = file.path(dir, "ages", "forSS", "by_source", "abrams_caal_north.csv"),
          row.names = FALSE)  

#===============================================================================
# CDFW Special Collections 
#===============================================================================
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
          file = file.path(dir, "ages", "forSS","by_source", "cdfw_special_collections_caal_north.csv"),
          row.names = FALSE)  





  