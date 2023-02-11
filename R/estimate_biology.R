###################################################################################
#
#              Copper rockfish 2023
#   Estimate biological parameters using survey and 
# 	 age and length read from various data sources.	
#   			   February 2023
#
#############################################################################################

library(nwfscSurvey)
library(ggplot2)

dir <- "C:/Assessments/2023/copper_rockfish_2023/data"

# Load in survey data
# NWFSC WCGBT survey data
load(file.path(dir, "wcgbt", "bio_copper rockfish_NWFSC.Combo_2022-11-27.rdata"))
wcgbt <- x
wcgbt$source <- "wcgbt"
wcgbt <- wcgbt[wcgbt$Latitude_dd < 42, ]
wcgbt$area <- 'north'
wcgbt[wcgbt$Latitude_dd < 34.47, 'area'] <- 'south'
colnames(wcgbt) <- tolower(colnames(wcgbt))
wcgbt$lat <- wcgbt$latitude_dd
wcgbt$lon <- wcgbt$longitude_dd

# NWFSC HKL survey data
hkl_all <- read.csv(file.path(dir, "nwfsc_hkl", "hookandline_2004_2021_draft_data.csv"))
hkl <- hkl_all[hkl_all$common_name == "Copper Rockfish", ]
hkl <- hkl[hkl$include_fish == 1, ]
hkl$source <- "hkl"
hkl$area <- 'south'
hkl$lat <- hkl$drop_latitude_degrees
hkl$lon <- hkl$drop_longitude_degrees


# CCFRP survey data


# Look at CRFSS as well for comparison with survey data
load(file.path(dir, "rec_bds", "crfss_bds_filtered.rdata"))
crfs <- crfss_bds
crfs$weight_kg <- crfs$AGENCY_WEIGHT
crfs$length_cm <- crfs$lengthcm
crfs$sex <- "U"
crfs$sex[crfs$RECFIN_SEX_NAME == "FEMALE"] <- "F" 
crfs$sex[crfs$RECFIN_SEX_NAME == "MALE"] <- "M" 

# Load in age reads from various sources
load(file.path(dir, "ages", "age_length_only_september_2021.Rdata"))
age_df <- df[df$Area %in% c("North_CA", "South_CA"), ]
colnames(age_df) <- tolower(colnames(age_df))
age_df$area[age_df$area == "North_CA"] <- 'north'
age_df$area[age_df$area == "South_CA"] <- 'south'

#=================================================
# Create a single data frame with all of the data
#=================================================
data_list <- list(wcgbt, hkl)
all_data <- NULL
for (a in 1:length(data_list)){
  tmp  <- data.frame(
  	year = data_list[[a]]$year,
  	lat = data_list[[a]]$lat,
  	lon = data_list[[a]]$lon,
  	area  = data_list[[a]]$area,
  	sex    = data_list[[a]]$sex,
  	length_cm = data_list[[a]]$length_cm,
  	weight_kg = data_list[[a]]$weight_kg,
  	age    = data_list[[a]]$age_years,
  	source = data_list[[a]]$source)
	
	all_data = rbind(all_data, tmp)			
}

#=================================================
# Estimate weight-length
#=================================================

ca_ests <- estimate_weight_length(
  data = all_data,
  col_length = "length_cm",
  col_weight = "weight_kg")
#  group median_intercept         SD            A        B
# female     9.555262e-06 0.10144934 9.604560e-06 3.188637
#   male     1.106592e-05 0.09748039 1.111862e-05 3.145235
#    all     1.438309e-05 0.11715348 1.448213e-05 3.074481
save(ca_ests, file = file.path(dir, "biology", "length_weight_ests.Rdata"))

ca_south_ests <- estimate_weight_length(
  data = all_data[all_data$area == 'south', ],
  col_length = "length_cm",
  col_weight = "weight_kg")
# group median_intercept         SD            A        B
# female     9.760803e-06 0.09873106 9.808492e-06 3.184427
#   male     1.115696e-05 0.09790639 1.121056e-05 3.144250
#    all     1.543890e-05 0.11628644 1.554364e-05 3.056488
# southern estimates are based on only 1686 fish

ca_north_ests <- estimate_weight_length(
  data = all_data[all_data$area == 'north', ],
  col_length = "length_cm",
  col_weight = "weight_kg")
#   group median_intercept         SD            A        B
# female     9.144959e-06 0.10825399 9.198701e-06 3.184286
#   male     9.360208e-06 0.08575163 9.394686e-06 3.181030
#    all     8.236316e-06 0.09822420 8.276144e-06 3.214452
# northern estimates are based on only 195 fish

crfs_ests <- estimate_weight_length(
  data = crfs,
  col_length = "length_cm",
  col_weight = "weight_kg")
#  group median_intercept        SD            A        B
# female     2.156236e-05 0.1041700 2.167967e-05 2.994721
#   male     3.317596e-10 0.0000000 3.317596e-10 5.872438
#    all     1.181218e-05 0.1535176 1.195220e-05 3.134932

length <- seq(0, 60, 1)
plot(length, crfs_ests$A[3] * length ^ crfs_ests$B[3], type = 'l',
	col = 'grey', lwd = 5)
points(crfs$length_cm, crfs$weight_kg, col = alpha('black', 0.2), pch = 1)
lines(length, crfs_ests$A[3] * length ^ crfs_ests$B[3], col = 'grey', lwd = 5)
lines(length, ca_ests$A[3] * length ^ ca_ests$B[3], col = 'grey', lty = 2, lwd = 2)
lines(length, ca_ests$A[1] * length ^ ca_ests$B[1], col = 'red', lty = 2, lwd = 2)
lines(length, ca_ests$A[2] * length ^ ca_ests$B[2], col = 'blue', lty = 2,lwd = 2)

ggplot(all_data, aes(x = length_cm, y = weight_kg, 
	color = source, pch = sex)) +
	geom_point() + 
	ylab("Weight (kg)") + xlab("Length (cm)") +
	scale_color_viridis_d(begin = 0, end = 0.5)

#========================================================
# Estimate age-at-length
#========================================================

age_df$Age <- age_df$age
age_df$Length_cm <- age_df$length_cm
age_df$Sex <- age_df$sex
length_age_ests_all <- est_growth(
  dat = age_df, 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

length_age_ests_north <- est_growth(
  dat = age_df[age_df$area == "north", ], 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

length_age_ests_south <- est_growth(
  dat = age_df[age_df$area == "south", ], 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

ages_all <- est_growth(
  dir = NULL, 
  dat = age_df, 
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  sdFactor = 2)

ages_south <- est_growth(
  dir = NULL, 
  dat = age_df[age_df$area == "south", ], 
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  sdFactor = 2)

ages_north <- est_growth(
  dir = NULL, 
  dat = age_df[age_df$area == "north", ], 
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  sdFactor = 2)


ggplot(age_df, aes(y = length_cm, x = age, color = source)) +
	geom_point() + 
	facet_grid(area~.) + 
	xlab("Age") + ylab("Length (cm)") +
	scale_fill_viridis_d()