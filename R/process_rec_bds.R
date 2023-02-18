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
library(ggplot2)
library(tidyverse)

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

# There is an overlaop in data between Collins-Crooke and Ally in 1986-1989
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


# Length column
# Note: These are total lengths not fork lengths
deb_wv$lengthcm <- deb_wv$lengthcm_tl / 10
miller_rec$lengthcm <- miller_rec$lengthcm_tl
collins_rec$lengthcm <- collins_rec$lengthcm_tl
ally_rec$lengthcm <- ally_rec$lengthcm_tl
donp$lengthcm <- donp$lengthcm / 10

# Put all the data into a single data frame
col_names <- c('year', 'mode', 'area', 'program', 'lengthcm', 'sex')
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
# Let's remove some lengths that are clearly incorrect
remove <- which(all_data$lengthcm > 65)
all_data <- all_data[-remove, ]
# There area 23 lengths between 60-65 which seem suspect but 
# going to keep them.

sample_size_cpfv <- all_data %>%
  dplyr::filter(mode == 'cpfv') %>%
  dplyr::group_by(area, year, program) %>%
  dplyr::tally()

sample_size_private <- all_data %>%
  dplyr::filter(mode == 'private') %>%
  dplyr::group_by(area, year, program) %>%
  dplyr::tally()

south <- dplyr::left_join(sample_size_cpfv[sample_size_cpfv$area == 'south', ],
                         sample_size_private[sample_size_private$area == 'south', ], by = c('year', 'program', 'area'))
colnames(south)[4:5] <- c('cpfv', 'private')
south[is.na(south)] <- 0
north <- dplyr::left_join(sample_size_cpfv[sample_size_cpfv$area == 'north', ],
                          sample_size_private[sample_size_private$area == 'north', ], by = c('year', 'program', 'area'))
colnames(north)[4:5] <- c('cpfv', 'private')
north[is.na(north)] <- 0
write.csv(south, file = file.path(dir, "forSS", "rec_south_sample_size_by_program.csv"), row.names = FALSE)
write.csv(north, file = file.path(dir, "forSS", "rec_north_sample_size_by_program.csv"), row.names = FALSE)


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

#==============================================================================
# Create un-weighted composition data for recreational data sources
#==============================================================================

# Add expected column names to work with nwfscSurvey package
# To Do: add revisions to nwfscSurvey package to dynamically check column names
all_data$Year <- all_data$year
all_data$Trawl_id <- 1:nrow(all_data)
all_data$Length_cm <- all_data$lengthcm
all_data$Sex <- all_data$sex
length_bins <- c(seq(8, 56, 2))

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
          find = grep(paste0("U-", length_bins[1]), colnames(lfs$unsexed)):grep(paste0("U-", max(length_bins)), colnames(lfs$unsexed))
          len_comps <- cbind(lfs$unsexed, lfs$unsexed[,find])
          write.csv(len_comps, 
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
        find = grep(paste0("U-", length_bins[1]), colnames(lfs$unsexed)):grep(paste0("U-", max(length_bins)), colnames(lfs$unsexed))
        out <- cbind(lfs$unsexed, lfs$unsexed[,find])
        write.csv(out, 
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



          