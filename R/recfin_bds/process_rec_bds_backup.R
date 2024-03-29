##################################################################################################
#
#	Process Recreational BDS Across Data Sources
# for use in the Copper Rockfish Assessent in 2023
# 					
#			Written by Chantel Wetzel
#
##################################################################################################

library(here)
library(nwfscSurvey)
# Note of the nwfscSurvey package: I did revisions to the unexpandeLf.fn 
# to work with our data. This function is available in the unexpand_comps
# branch on github.
library(ggplot2)
library(tidyverse)

# Here's the link to the paper: https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/fish-bull/echeverria.pdf
# The paper uses "pinched" tail length for total length, so it's an overestimate of total length. 
# (0.629+(1.01*LNGTH)) as total_length
# Check the MRFSS length, particularly those that overlap with Deb WV lengths

dir <- file.path(here(), "data", "rec_bds")
setwd(dir)
dir.create(file.path(dir, "plots"), showWarnings = FALSE)
dir.create(file.path(dir, "forSS"), showWarnings = FALSE)

# Load in data from various sources
# CRFS 2004-2022 - both areas
load("crfss_bds_filtered.rdata")
# MRFSS 1980-2003 - both areas
load("mrfss_bds_filtered.rdata")
# 1987-1998 - north
load("DevWV_bds.rdata")
# 1978 - 1984 - north
load("DonP_rec_bds.rdata")
# 1958 - 1966 - north
load("miller_rec_bds.rdata")
# 1984-1989- south
load("ally_rec_bds.rdata")
# 1975 - 1989 - south
load("collinsCrooke_rec_bds.rdata")


crfs <- crfss_bds
mrfs <- mrfss_bds
deb_wv <- DebWV
donp <- DonP

# Use only retained lengths in CRFS (only 280 released lengths from cpfv) 
crfs <- crfs[crfs$IS_RETAINED == "RETAINED", ]

# There is an overlap in data between Collins-Crooke and Ally in 1986-1989
collins_rec <- collins_rec[collins_rec$year < 1986, ]

# Add an area column for the Deb Wilson-Vanderberg data
deb_wv$area[deb_wv$district %in% 3:6] = "north"

# Add a source to all the data
crfs$program <- "crfs"
mrfs$program <- 'mrfss'
deb_wv$program <- 'deb_wilson-vandenberg'
miller_rec$program <- 'miller'
donp$program <- 'don_pearson'
ally_rec$program <- 'ally'
collins_rec$program <- 'collins-crooke'

# Add shoreside lengths to private mode
mrfs$mode[mrfs$mode == "shoreside"] <- 'private'
crfs$mode[crfs$mode == "shoreside"] <- 'private'

# Standardize a sex column
mrfs$sex <- "U"
crfs$sex <- crfs$RECFIN_SEX_CODE
crfs$sex[!crfs$sex %in% c("F", "M")] <- "U"
collins_rec$sex <- "U"
ally_rec$sex <- "U"
donp$sex[donp$sex == 2] <- "F"
donp$sex[donp$sex == 1] <- "M"
donp$sex[donp$sex == 9] <- "U"
miller_rec$sex <- "U"
deb_wv$sex <- "U"

# https://spo.nmfs.noaa.gov/sites/default/files/pdf-content/fish-bull/echeverria.pdf
# The paper uses "pinched" tail length for total length, so it's an overestimate of total length. 
# (0.629+(1.01*LNGTH)) as total_length

# Length column
# Note: These are total lengths not fork lengths
deb_wv$lengthcm <- deb_wv$lengthcm_tl / 10
miller_rec$lengthcm <- miller_rec$lengthcm_tl
collins_rec$lengthcm <- collins_rec$lengthcm_tl
ally_rec$lengthcm <- ally_rec$lengthcm_tl
donp$lengthcm <- donp$lengthcm / 10

#=============================================================================================
# Try to determine the number of trips in each data set
#=============================================================================================

mrfs$trip <- paste0(mrfs$year, mrfs$ID_CODE, mrfs$INTSITE, mrfs$AREA_X)
crfs$trip <- paste0(crfs$RECFIN_DATE, crfs$COUNTY_NUMBER, crfs$AGENCY_WATER_AREA_NAME)
deb_wv$trip <- paste0(deb_wv$year, deb_wv$TRIP_ID)
miller_rec$trip <- paste0(miller_rec$year, miller_rec$district, miller_rec$county)
donp$trip <- paste0(donp$SAMPLE_NO)
collins_rec$trip <- collins_rec$tripID
ally_rec$trip <- paste0(ally_rec$year, ally_rec$complex, ally_rec$landing, ally_rec$district)

#=============================================================================================
# CRFS mean length
#=============================================================================================

crfs_mean_length <- crfs %>%
  mutate(district = substr(RECFIN_PORT_NAME, 1, 5)) %>%
  group_by(RECFIN_YEAR, mode, RECFIN_PORT_NAME) %>%
  summarize(mean = mean(lengthcm))


ggplot(crfs_mean_length) +
 # geom_line(size = 1) + 
  geomtextpath::geom_textline(aes(
    x = RECFIN_YEAR, y = mean, 
    color = RECFIN_PORT_NAME, label = RECFIN_PORT_NAME),
  hjust = .7,
  size = 2,
  linewidth = 1.2) +
  facet_wrap(~mode) +
  theme(legend.position = "none") +
  xlab("Year") + ylab("Mean Length") +
  scale_color_viridis_d() 
 

#=============================================================================================
# MRFSS vs Deb's exploration
#=============================================================================================
 #mrfss assign counties to districts for only northern ca
   mrfs_north <- mrfs %>%
     filter(area == "north",
            mode=="cpfv",
            between(year, 1987,1998)) %>%
     mutate(district = case_when(CNTY %in% c(15, 23) ~ 6,
         CNTY %in% c(45) ~ 5,
              CNTY %in% c(53, 79, 87) ~ 3, 
            CNTY %in% c(1,13,41,75,77,81,97) ~ 4))
 col_names1 <- c('year', 'mode', 'area','district', 'program', 'lengthcm', 'trip')
 some_dat <- rbind(
     mrfs_north[, col_names1],
     deb_wv[, col_names1]
   )
 some_data <- rbind(
     mrfs_north[, col_names1],
     deb_wv[, col_names1]
   )
 mean_length <- some_data %>%
   group_by(program, district, year) %>%
   summarize(mean = mean(lengthcm),
             count = n())
 
 samples1 <- some_data %>%
   group_by(program, district, year) %>%
   summarize(count = n())
 
 samples2 <- some_data %>%
   group_by(program, year) %>%
   summarise(count_all = n())
 
 samples3 <- inner_join(samples1,samples2) %>%
   mutate(percent_in_district = count/count_all)

 #=============================================================================================
 # Plots for melissa's exploration
 #=============================================================================================
 
 #look at the percent of samples coming from each district by program over time
 ggplot(samples3, aes(x = year, y = percent_in_district, 
                      color = as.factor(district))) + 
   geom_point() +
   geom_path() +
   facet_wrap(~program) +
   scale_color_viridis_d() 
  
 
 
  ggplot(mean_length, aes(x = year, y = mean, color = district)) +
   geom_line(lwd = 0.8, adjust = 0.5) + 
   xlab("Year") + ylab("MeanLength") +
   facet_wrap(facets = c("program")) + 
 
  ggplot(some_data, aes(y = lengthcm, x = year, group = year)) +
     geom_boxplot() + 
     facet_wrap(facets = c("district"," program")) + 
     xlab("Year") + ylab("Length (cm)") 


 ggplot(some_data, aes(y = lengthcm, x = program, group = program)) +
     geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
     xlab("Length (cm)") + ylab("Density") +
     facet_wrap(facets = c("district")) + 
     scale_color_viridis_d()
 
 ggplot(some_data, aes(lengthcm, group = program)) +
     geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
     xlab("Length (cm)") + ylab("Density") +
     facet_wrap(facets = c("district")) + 
     scale_color_viridis_d()
 ggplot(some_data, aes(lengthcm, fill = program, color = program)) +
     geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
     xlab("Length (cm)") + ylab("Density") +
     facet_wrap(facets = c("district")) + 
     scale_color_viridis_d()
> #ggsave(filename = file.path(dir, "plots", "rec_south_length_boxplot_by_mode_program_year.png"),
   #       width = 10, height = 10)
 
    ggplot(some_data, aes(lengthcm, fill = district, color = district)) +
     geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
     xlab("Length (cm)") + ylab("Density") +
     facet_wrap(facets = c("program")) + 
     scale_color_viridis_d()




#=============================================================================================
# Put all the data into a single data frame
#=============================================================================================

col_names <- c('year', 'mode', 'area', 'program', 'lengthcm', 'sex', 'trip')
all_data <- rbind(
  mrfs[, col_names],
  crfs[, col_names], 
  deb_wv[, col_names],
  miller_rec[, col_names],
  donp[, col_names],
  collins_rec[, col_names],
  ally_rec[, col_names]
)

all_data <- all_data[!is.na(all_data$lengthcm), ]
# Let's remove 8 lengths that are clearly incorrect
remove <- which(all_data$lengthcm > 65)
all_data <- all_data[-remove, ]
# There area 23 lengths between 60-65 which seem suspect but 
# going to keep them.

#==============================================================================
# Plot the data quickly
#==============================================================================

ggplot(all_data[all_data$area == "south", ], aes(y = lengthcm, x = year, group = year)) +
  geom_boxplot() + 
  facet_wrap(facets = c("mode", "program")) + 
  xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "rec_south_length_boxplot_by_mode_program_year.png"),
       width = 10, height = 10)

ggplot(all_data[all_data$area == "north", ], aes(y = lengthcm, x = year, group = year)) +
  geom_boxplot() + 
  facet_wrap(facets = c("mode", "program")) + 
  xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "rec_north_length_boxplot_by_mode_program_year.png"),
       width = 10, height = 10)

ggplot(all_data, aes(lengthcm, fill = mode, color = mode)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("area", "program")) + 
  scale_color_viridis_d()

ggplot(all_data, aes(lengthcm, fill = program, color = program)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("area", "mode")) + 
  scale_color_viridis_d()

ggplot(all_data[all_data$area == 'south', ], aes(lengthcm, fill = mode, color = mode)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("program")) + 
  scale_color_viridis_d()

ggplot(all_data[all_data$area == 'north', ], aes(lengthcm, fill = mode, color = mode)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("program")) + 
  scale_color_viridis_d()

# Compare DWV and MRFSS data looking for overlap (e.g., 'mrfss' samples included in DWV data)
tmp = all_data[all_data$year %in% c(1987:1998) & all_data$area == 'north' & all_data$mode == 'cpfv', ]
tmp$length <- tmp$lengthcm
tmp$length[tmp$program == 'mrfss'] <- 0.629+(1.01 * tmp$length[tmp$program == 'mrfss'])
aggregate(length~program+year, tmp, quantile)

comp_mrfss_dwv <- tmp %>%
  group_by(year, program) %>%
  reframe(
    n = n(), 
    len_min = min(lengthcm),
    len_med = median(lengthcm),
    len_mean = mean(lengthcm),
    len_max = max(lengthcm)
  )
write.csv(comp_mrfss_dwv, 
    file = file.path(dir, "forSS", "north_mrfss_dwv_comparison.csv"),
    row.names = FALSE)
ggplot(tmp, aes(lengthcm, fill = program, color = program)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("year")) + 
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "plots", "rec_north_mrfss_dwv_comparison.png"),
       width = 10, height = 10)

# Compare Ally and MRFSS data looking for overlap 
tmp = all_data[all_data$year %in% c(1984:1989) & all_data$area == 'south' & all_data$mode == 'cpfv', ]
aggregate(lengthcm~program+year, tmp, quantile)
comp_mrfss_ally <- tmp %>%
  group_by(year, program) %>%
  summarise(
    n = n(), 
    len_min = min(lengthcm),
    len_med = median(lengthcm),
    len_mean = mean(lengthcm),
    len_max = max(lengthcm)
  )
write.csv(comp_mrfss_ally, 
    file = file.path(dir, "forSS", "south_mrfss_ally_comparison.csv"),
    row.names = FALSE)

ggplot(tmp, aes(lengthcm, fill = program, color = program)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("year")) + 
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "plots", "rec_south_mrfss_ally_comparison.png"),
       width = 10, height = 10)
#==============================================================================
#  
#==============================================================================


#==============================================================================
# Calculate sample size by year and area 
#==============================================================================

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

sample_size_cpfv <- all_data %>%
  dplyr::filter(mode == 'cpfv') %>%
  dplyr::group_by(area, year, program) %>%
  dplyr::summarise(
    ntrip = length(unique(trip)),
    n = length(lengthcm))

sample_size_private <- all_data %>%
  dplyr::filter(mode == 'private') %>%
  dplyr::group_by(area, year, program) %>%
  dplyr::summarise(
    ntrip = length(unique(trip)),
    n = length(lengthcm))

south <- dplyr::left_join(
  sample_size_cpfv[sample_size_cpfv$area == 'south', ],
  sample_size_private[sample_size_private$area == 'south', ], 
  by = c('year', 'program', 'area'))
colnames(south) <- c('Area', 'Year', 'Source', 'CPFV Trips', 'CPFV Samples', 'PR Trips', 'PR Samples')
south <- as.data.frame(south)
south[is.na(south)] <- "-"
south$Source <- toupper(south$Source)
south$Source <- gsub("_", " ", south$Source)
south$Area <- firstup(south$Area)

north <- dplyr::left_join(
  sample_size_cpfv[sample_size_cpfv$area == 'north', ],
  sample_size_private[sample_size_private$area == 'north', ], 
  by = c('year', 'program', 'area'))
colnames(north) <- c('Area', 'Year', 'Source', 'CPFV Trips', 'CPFV Samples', 'PR Trips', 'PR Samples')
north <- as.data.frame(north)
north[is.na(north)] <- "-"
north$Source <- toupper(north$Source)
north$Source <- gsub("_", " ", north$Source)
north$Area <- firstup(north$Area)

write.csv(south, file = file.path(dir, "forSS", "rec_south_sample_size_by_program.csv"), row.names = FALSE)
write.csv(north, file = file.path(dir, "forSS", "rec_north_sample_size_by_program.csv"), row.names = FALSE)

#==============================================================================
# Plot the data quickly
#==============================================================================

ggplot(all_data[all_data$area == "south", ], aes(y = lengthcm, x = year, group = year)) +
  geom_boxplot() + 
  facet_wrap(facets = c("mode", "program")) + 
  xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "rec_south_length_boxplot_by_mode_program_year.png"),
       width = 10, height = 10)

ggplot(all_data[all_data$area == "north", ], aes(y = lengthcm, x = year, group = year)) +
  geom_boxplot() + 
  facet_wrap(facets = c("mode", "program")) + 
  xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "rec_north_length_boxplot_by_mode_program_year.png"),
       width = 10, height = 10)

ggplot(all_data, aes(lengthcm, fill = mode, color = mode)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("area", "program")) + 
  scale_color_viridis_d()

ggplot(all_data, aes(lengthcm, fill = program, color = program)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("area", "mode")) + 
  scale_color_viridis_d()

ggplot(all_data[all_data$area == 'south', ], aes(lengthcm, fill = mode, color = mode)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("program")) + 
  scale_color_viridis_d()

ggplot(all_data[all_data$area == 'north', ], aes(lengthcm, fill = mode, color = mode)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("program")) + 
  scale_color_viridis_d()

# Compare DWV and MRFSS data looking for overlap (e.g., 'mrfss' samples included in DWV data)
tmp = all_data[all_data$year %in% c(1987:1998) & all_data$area == 'north' & all_data$mode == 'cpfv', ]
aggregate(lengthcm~program+year, tmp, quantile)
comp_mrfss_dwv <- tmp %>%
  group_by(year, program) %>%
  summarise(
    n = n(), 
    len_min = min(lengthcm),
    len_med = median(lengthcm),
    len_mean = mean(lengthcm),
    len_max = max(lengthcm)
  )
write.csv(comp_mrfss_dwv, 
    file = file.path(dir, "forSS", "north_mrfss_dwv_comparison.csv"),
    row.names = FALSE)

ggplot(tmp, aes(lengthcm, fill = program, color = program)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("year")) + 
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "plots", "rec_north_mrfss_dwv_comparison.png"),
       width = 10, height = 10)

# Compare Ally and MRFSS data looking for overlap 
tmp = all_data[all_data$year %in% c(1984:1989) & all_data$area == 'south' & all_data$mode == 'cpfv', ]
aggregate(lengthcm~program+year, tmp, quantile)
comp_mrfss_ally <- tmp %>%
  group_by(year, program) %>%
  summarise(
    n = n(), 
    len_min = min(lengthcm),
    len_med = median(lengthcm),
    len_mean = mean(lengthcm),
    len_max = max(lengthcm)
  )
write.csv(comp_mrfss_ally, 
    file = file.path(dir, "forSS", "south_mrfss_ally_comparison.csv"),
    row.names = FALSE)

ggplot(tmp, aes(lengthcm, fill = program, color = program)) + 
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("year")) + 
  scale_color_viridis_d()
ggsave(filename = file.path(dir, "plots", "rec_south_mrfss_ally_comparison.png"),
       width = 10, height = 10)

#==============================================================================
# Create un-weighted composition data for recreational data sources
#==============================================================================

# Add expected column names to work with nwfscSurvey package
# To Do: add revisions to nwfscSurvey package to dynamically check column names
length_bins <- c(seq(10, 54, 2))

# Should switch to using purr package function instead of a loop
for(a in unique(all_data$area)) {
  for(m in unique(all_data$mode)) {
    for(p in unique(all_data$program)) {
      df <- all_data[all_data$area == a & all_data$mode == m & all_data$program == p, ]
      if(dim(df)[1] > 0) {
        fleet <- ifelse(m == "cpfv", 3, 4)
        lfs <-  UnexpandedLFs.fn(
          datL = df, 
          lgthBins = length_bins,
          partition = 0, 
          fleet = fleet, 
          month = 7)
        
        if(!is.null(lfs$unsexed)){
          write.csv(lfs$unsexed, 
                    file = file.path(dir, "forSS", "data_by_program", paste0(a, "_", m, "_", p, "_sources_not_expanded_length_comp_sex_0.csv")),
                    row.names = FALSE) 
        } 
        if(!is.null(lfs$sexed)){
          write.csv(lfs$sexed, 
                    file = file.path(dir, "forSS",  "data_by_program", paste0(a, "_", m, "_", p, "_sources_not_expanded_length_comp_sex_3.csv")),
                    row.names = FALSE) 
        }
        lfs <- NULL
      } #if loop from dim(df)
    }
  }
}

for(a in unique(all_data$area)){
  for(m in unique(all_data$mode)) {
    df <- all_data[all_data$area == a & all_data$mode == m, ]
    if(dim(df)[1] > 0) {
      fleet <- ifelse(m == "cpfv", 3, 4)
      lfs <-  UnexpandedLFs.fn(
            datL = df, 
            lgthBins = length_bins,
            partition = 0, 
            fleet = fleet, 
            month = 7
      )
      
      if(!is.null(lfs$unsexed)) {
        write.csv(lfs$unsexed, 
            file = file.path(dir, "forSS", paste0(a, "_", m, "_all_sources_not_expanded_length_comp_sex_0.csv")),
            row.names = FALSE) 
      } 
      if(!is.null(lfs$sexed)) {
        write.csv(lfs$sexed, 
           file = file.path(dir, "forSS", paste0(a, "_", m, "_all_sources_not_expanded_length_comp_sex_3.csv")),
           row.names = FALSE) 
      }
      lfs <- NULL
    } # close if statement
  }
}




