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
library(here)

dir <- file.path(here(), "data")

pngfun <- function(dir, name, w = 7,h = 7, pt = 12){
  file <- file.path(dir, name)
  #cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}

# Load in survey data
# NWFSC WCGBT survey data
#load(file.path(dir, "wcgbt", "bio_copper rockfish_NWFSC.Combo_2023-02-11.rdata"))
load(file.path(dir, "wcgbt", "bio_2003-2004_w_ages.rdata"))
wcgbt <- bio_orig
wcgbt$source <- "NWFSC WCGBT"
wcgbt <- wcgbt[wcgbt$Latitude_dd < 42, ]
wcgbt$area <- 'north'
wcgbt[wcgbt$Latitude_dd < 34.47, 'area'] <- 'south'
colnames(wcgbt) <- tolower(colnames(wcgbt))
wcgbt$lat <- wcgbt$latitude_dd
wcgbt$lon <- wcgbt$longitude_dd

# NWFSC HKL survey data
load(file.path(dir, "nwfsc_hkl", "nwfsc_hkl_2004-2022.rdata"))
hkl <- hkl[hkl$common_name == "Copper Rockfish", ]
# hkl <- hkl[hkl$include_fish == 1, ]
hkl$source <- "NWFSC HKL"
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
age_df$weight_kg = NA
age_df$lat = NA
age_df$lon = NA
age_df$year = NA
colnames(age_df)[colnames(age_df)=='age'] <- 'age_years'
age_df = age_df[!age_df$source %in% c("NWFSC_HKL", "NWFSC_WCGBT"), ]


#=================================================
# Create a single data frame with all of the data
#=================================================
data_list <- list(wcgbt, hkl, age_df)
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
  	Source = data_list[[a]]$source)
	
	all_data = rbind(all_data, tmp)			
}

all_data <- all_data[!is.na(all_data$length_cm), ]


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

ggplot(all_data[all_data$source %in% c('wcgbt', 'hkl'), ], aes(x = length_cm, y = weight_kg, 
	color = sex, pch = sex)) +
	geom_point(shape = 16, size = 2, alpha = 0.5) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    strip.text.y = element_text(size = 14),
    panel.grid.minor = element_blank()) + 
	ylab("Weight (kg)") + xlab("Length (cm)") +
	scale_color_viridis_d(begin = 0, end = 1)
ggsave(filename = file.path(dir, "biology", "plots", "weigth_at_length.png"),
       width = 10, height = 8)


lens = 1:max(all_data$length_cm, na.rm = TRUE)
ymax = max(all_data$weight_kg, na.rm = TRUE)
xmax = max(all_data$length_cm, na.rm = TRUE)
line_col = c("red", 'blue', "grey")
sex_col = c(alpha(line_col[1:2], 0.2), alpha(line_col[3], 0.20))

pngfun(dir = file.path(dir, "biology", "plots"), 
  name = "Length_Weight_All.png", w = 7, h = 7, pt = 12)
par(mfrow = c(1, 1))
plot(all_data[all_data$sex == 'F', "length_cm"], 
  all_data[all_data$sex == 'F', "weight_kg"], las = 1,
  cex.lab = 1.5, cex.axis = 1.5, cex = 1.5,
     xlab = "Length (cm)", ylab = "Weight (kg)", main = "", 
     ylim = c(0, ymax), xlim = c(0, xmax), pch = 16, col = sex_col[1]) 
points(all_data[all_data$sex == 'F', "length_cm"], all_data[all_data$sex == 'F', "weight_kg"], pch = 16, col = sex_col[1])
points(all_data[all_data$sex == 'M', "length_cm"], all_data[all_data$sex == 'M', "weight_kg"], pch = 17, col = sex_col[2])
lines(lens, ca_ests[1, 4] * lens ^ ca_ests[1, 5], col = line_col[1], lwd = 3, lty = 2)
lines(lens, ca_ests[2, 4] * lens ^ ca_ests[2, 5], col = line_col[2], lwd = 3, lty = 3)
leg = c(paste0("F: a = ", signif(ca_ests[1, 3], digits = 3),  
                " b = ", round(ca_ests[1, 4], 2) ), 
    paste0("M: a = ", signif(ca_ests[2, 3], digits = 3),  
                " b = ", round(ca_ests[2, 4], 2) ))
legend("topleft", bty = 'n', legend = c("Female", "Male"), lty = c(2, 3), 
  pch = c(16, 17), col = c(line_col[1], line_col[2]), lwd = 3, cex = 1.25)
dev.off()

#========================================================
# Estimate age-at-length
#========================================================

age_df <- all_data[!is.na(all_data$age), ]
age_df <- age_df[age_df$Source != "Carcass Sampling", ]
age_df <- age_df[age_df$sex != "U", ]
age_df$Age <- age_df$age
age_df$Length_cm <- age_df$length_cm
age_df$Sex <- age_df$sex

ages_all <- est_growth(
  dir = NULL, 
  dat = age_df, 
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  sdFactor = 3)

remove <- which(ages_all[,'length_cm'] > ages_all[,'Lhat_high'] | ages_all[,'length_cm'] < ages_all[,'Lhat_low'])
ages_all[remove, ]

plot(ages_all[remove, 'Age'], ages_all[remove, "Length_cm"], xlim = c(0,40), ylim = c(0, 50)) 
clean_ages <- age_df[-remove, ]

length_age_ests_all <- est_growth(
  dat = clean_ages, #age_df, 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

save(length_age_ests_all, file = file.path(dir,"biology", 'length_at_age_ests_all.rdata'))

length_age_ests_north <- est_growth(
  dat = age_df[age_df$area == "north", ], 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))


save(length_age_ests_north, file = file.path(dir,"biology", 'length_at_age_ests_north.rdata'))


length_age_ests_south <- est_growth(
  dat = age_df[age_df$area == "south", ], 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

save(length_age_ests_south, file = file.path(dir,"biology", 'length_at_age_ests_south.rdata'))

ggplot(age_df, aes(y = length_cm, x = age, color = Source)) +
	geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 50) + ylim(1, 55) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
	facet_grid(area~.) + 
	xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "biology", "plots", "age_at_length.png"),
       width = 10, height = 8)


ggplot(age_df, aes(y = length_cm, x = age, color = sex)) +
  geom_point() + 
  theme_bw() + 
  xlim(1, 50) + ylim(1, 55) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  facet_grid(area~.) + 
  xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "biology", "plots", "age_at_length_by_sex.png"),
       width = 10, height = 8)


#========================================================
# Load in all ages available as of 4/2/2023
#========================================================
load(file.path(dir, "ages", "formatted_age_files", "all_copper_ages.rdata"))
all_ages$program[all_ages$program == "abrams"] <- "Abrams"
all_ages$program[all_ages$program == "Pearson Research'"] <- "Pearson"
all_ages$program[all_ages$program %in% c("CCFRP", "CPOP")] <- "CCFRP"
all_ages$program[all_ages$program %in% c("Commercial - EFI", "Commercial - Pilot Sampling", "Whole")] <- "CDFW"
all_ages$program[all_ages$program == "commercial"] <- "Commercial"
all_ages$program[all_ages$program %in% c("Recreation", "Unknown")] <- "CRFS"
all_ages$area[all_ages$area == "unknown"] <- "north" 


all_ages$Age <- all_ages$age
all_ages$Length_cm <- all_ages$length_cm
all_ages$Sex <- all_ages$sex
all_ages <- all_ages[!is.na(all_ages$age), ]

ages_all <- nwfscSurvey::est_growth(
  dir = NULL, 
  dat = all_ages, 
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  sdFactor = 3)

remove <- which(ages_all[,'length_cm'] > ages_all[,'Lhat_high'] | ages_all[,'length_cm'] < ages_all[,'Lhat_low'])
ages_all[remove, ]

HandyCode::pngfun(wd = file.path(dir,"ages", "formatted_age_files", "plots"), file = "bad_ages.png")
plot(ages_all[, 'Age'], ages_all[, "Length_cm"], xlim = c(0, 55), ylim = c(0, 60), col = 'grey') 
points(ages_all[remove, 'Age'], ages_all[remove, "Length_cm"], xlim = c(0,40), pch = 16, ylim = c(0, 50), col = 'red') 
dev.off()

sink(file = file.path(dir, "ages", "formatted_age_files", "ages_for_removal.txt"))
ages_all[remove, ]
sink()

clean_ages <- all_ages[-remove, ]

length_age_ests_all <- est_growth(
  dat = clean_ages, #age_df, 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

save(length_age_ests_all, file = file.path(dir, "biology", 'length_at_age_ests_all_4.2.2023.rdata'))

length_age_ests_north <- est_growth(
  dat = clean_ages[clean_ages$area == "north", ], 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

save(length_age_ests_north, file = file.path(dir,"biology", 'length_at_age_ests_north_4.2.2023.rdata'))


length_age_ests_south <- est_growth(
  dat = clean_ages[clean_ages$area == "south", ], 
  return_df = FALSE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10))

save(length_age_ests_south, file = file.path(dir,"biology", 'length_at_age_ests_south_4.2.2023.rdata'))

ggplot(all_ages, aes(y = Length_cm, x = Age, color = program)) +
  geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 50) + ylim(1, 55) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  facet_grid(area~.) + 
  xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "biology", "plots", "age_at_length_04022023.png"),
       width = 10, height = 12)

save(clean_ages, file = file.path(dir, "ages", "formatted_age_files", "cleaned_all_copper_ages.rdata"))
ggplot(age_df, aes(y = length_cm, x = age, color = sex)) +
  geom_point() + 
  theme_bw() + 
  xlim(1, 50) + ylim(1, 55) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  facet_grid(area~.) + 
  xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "biology", "plots", "age_at_length_by_sex.png"),
       width = 10, height = 8)


ggplot(all_ages, aes(y = length_cm, x = age, color = program)) +
  geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 50) + ylim(1, 55) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  facet_grid(area~.) + 
  xlab("Age") + ylab("Length (cm)") +
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "biology", "plots", "age_at_length_by_all_programs.png"),
       width = 10, height = 12)


tmp <- all_ages[all_ages$sex %in% c("F", "M") & all_ages$program %in% c("CCFRP", "NWFSC_WCGBT", "NWFSC_HKL", "Pearson", "CPFV_COOP_collections") & all_ages$area == "south", ]
ggplot(tmp, aes(y = Length_cm, x = Age, color = program, shape = program)) +
  geom_point(alpha = 0.1) + 
  theme_bw() + 
  geom_jitter() + 
  xlim(1, 50) + ylim(1, 55) +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        strip.text.y = element_text(size = 14),
        panel.grid.minor = element_blank()) + 
  xlab("Age") + ylab("Length (cm)") +
  facet_grid(sex~.) +
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "biology", "plots", "south_age_at_length_by_growth_source.png"),
       width = 10, height = 13)
