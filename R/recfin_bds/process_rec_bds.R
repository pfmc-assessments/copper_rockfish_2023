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
# crfs <- crfs[crfs$IS_RETAINED == "RETAINED", ]

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

mrfs$trip <- paste0(mrfs$year, mrfs$WAVE, mrfs$ID_CODE, mrfs$INTSITE, mrfs$AREA_X, mrfs$mode)
crfs$trip <- paste0(crfs$RECFIN_DATE, crfs$COUNTY_NUMBER, crfs$AGENCY_WATER_AREA_NAME, crfs$INTERVIEW_SITE, crfs$RECFIN_MODE_NAME)
deb_wv$trip <- paste0(deb_wv$year, deb_wv$TRIP_ID)
miller_rec$trip <- paste0(miller_rec$year, miller_rec$district, miller_rec$county, miller_rec$mode)
donp$trip <- paste0(donp$SAMPLE_NO)
collins_rec$trip <- collins_rec$tripID
ally_rec$trip <- paste0(ally_rec$year, ally_rec$complex, ally_rec$landing, ally_rec$district)

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
# remove <- which(all_data$lengthcm >= 60)

# Rename the lenth column to match the form expected by the unexpandedLF.fn
# colnames(all_data[colnames(all_data) == "lengthcm"]) <- "length_cm"

# Remove the MRFSS lengths from 1997-98 since they are the same as those in Deb's data
remove <- which(all_data$program == "mrfss" & all_data$year %in% 1997:1998 &
                all_data$mode == "cpfv" & all_data$area == "north")
all_data <- all_data[-remove, ]

save(all_data, file = file.path(dir, "all_rec_length_data.rdata"))

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
# Create un-weighted composition data for recreational data sources
#==============================================================================

# Add expected column names to work with nwfscSurvey package
# To Do: add revisions to nwfscSurvey package to dynamically check column names
all_data$age <- NA
length_bins <- c(seq(10, 54, 2))

all_data$sex_group <- "u"
all_data$sex_group[all_data$sex %in% c("M", "F")] <- 'b'

n <- all_data %>%
  dplyr::group_by(area, year, program, mode, sex_group) %>%
  dplyr::summarise(
    ntrip = length(unique(trip)))

# This creates the composition data by program just in case.
# Should switch to using purr package function instead of a loop
for(a in unique(all_data$area)) {
  for(m in unique(all_data$mode)) {
    for(p in unique(all_data$program)) {
      
      use_n <- n[n$area == a & n$mode == m & n$program == p, ]
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
          lfs$unsexed[,"InputN"] <- use_n[use_n$sex_group == "u", 'ntrip']
          write.csv(lfs$unsexed, 
                    file = file.path(dir, "forSS", "data_by_program", paste0(a, "_", m, "_", p, "_sources_not_expanded_length_comp_sex_0.csv")),
                    row.names = FALSE) 
        } 
        if(!is.null(lfs$sexed)){
          lfs$sexed[,"InputN"] <- use_n[use_n$sex_group == "b", 'ntrip']
          write.csv(lfs$sexed, 
                    file = file.path(dir, "forSS",  "data_by_program", paste0(a, "_", m, "_", p, "_sources_not_expanded_length_comp_sex_3.csv")),
                    row.names = FALSE) 
        }
        lfs <- NULL
      } #if loop from dim(df)
    }
  }
}


# Remove the MRFSS sample years that overlap with Deb's data
find_mrfss <- which(all_data$program == "mrfss" & all_data$year %in% 1987:1998 &
                    all_data$mode == "cpfv" & all_data$area == "north")
tmp <- all_data[-find_mrfss, ]
sub_mrfss <- all_data[find_mrfss, ]

n <- tmp %>%
  dplyr::group_by(area, year, mode, sex_group) %>%
  dplyr::summarise(
    ntrip = length(unique(trip)))

for(a in unique(tmp$area)){
  for(m in unique(tmp$mode)) {
    
    use_n <- n[n$area == a & n$mode == m, ]
    df <- tmp[tmp$area == a & tmp$mode == m, ]
    
    if(dim(df)[1] > 0) {
      fleet <- ifelse(m == "cpfv", 3, 4)
      lfs <-  UnexpandedLFs.fn(
            datL = df, 
            lgthBins = length_bins,
            partition = 0, 
            fleet = fleet, 
            month = 7)
      
      if(!is.null(lfs$unsexed)) {
        lfs$unsexed[,"InputN"] <- use_n[use_n$sex_group == "u", 'ntrip']
        write.csv(lfs$unsexed, 
            file = file.path(dir, "forSS", paste0(a, "_", m, "_all_sources_not_expanded_length_comp_sex_0.csv")),
            row.names = FALSE) 
      } 
      if(!is.null(lfs$sexed)) {
        lfs$sexed[,"InputN"] <- use_n[use_n$sex_group == "b", 'ntrip']
        write.csv(lfs$sexed, 
           file = file.path(dir, "forSS", paste0(a, "_", m, "_all_sources_not_expanded_length_comp_sex_3.csv")),
           row.names = FALSE) 
      }
      lfs <- NULL
    } # close if statement
  }
}

tmp <- sub_mrfss

n <- tmp %>%
  dplyr::group_by(area, year, mode, sex_group) %>%
  dplyr::summarise(
    ntrip = length(unique(trip)))

for(a in unique(tmp$area)){
  for(m in unique(tmp$mode)) {
    use_n <- n[n$area == a & n$mode == m, ]
    df <- tmp[tmp$area == a & tmp$mode == m, ]
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
        lfs$unsexed[,"InputN"] <- use_n[use_n$sex_group == "u", 'ntrip']
        write.csv(lfs$unsexed, 
                  file = file.path(dir, "forSS", paste0(a, "_", m, "_mrfss_1987-1998_not_expanded_length_comp_sex_0.csv")),
                  row.names = FALSE) 
      } 
      if(!is.null(lfs$sexed)) {
        lfs$sexed[,"InputN"] <- use_n[use_n$sex_group == "b", 'ntrip']
        write.csv(lfs$sexed, 
                  file = file.path(dir, "forSS", paste0(a, "_", m, "__mrfss_1987-1998_not_expanded_length_comp_sex_3.csv")),
                  row.names = FALSE) 
      }
      lfs <- NULL
    } # close if statement
  }
}

#==============================================================================
# Compare the early length to the length of aged fish in the historical ages
#==============================================================================
load(here("data", "ages", "formatted_age_files", "historical_rec_ages.rdata"))
load(here("data", "ages", "formatted_age_files", "unknown_historical_ages.rdata"))

age_df <- rbind(
  hist_rec_ages[, c('program', 'year', 'sex', 'length_cm', 'age')],
  unknown_ages [, c('program', 'year', 'sex', 'length_cm', 'age')]
)
age_df$lengthcm <- age_df$length_cm
age_df <- as.data.frame(age_df)
age_df$type = "age"
age_df$area = "unknown-area-ages"

df <- all_data[all_data$year %in% age_df$year, ]
df$type = "bds"

tmp <- rbind(
  df[, c('year', 'area', 'lengthcm', 'type')],
  age_df[, c('year', 'area', 'lengthcm', 'type')])

ggplot(tmp) +
  geom_density(aes(x = lengthcm, color = area)) +
  #geom_density(tmp[tmp$type == "bds", ], aes(x = lengthcm, color = area)) +
  facet_wrap('year')
ggsave(width = 10, height = 7,
  file = here("data", "ages", "formatted_age_files", "plots", "compare_hist_bds_len_and_len_of_aged_fish.png"))
# Sample size of ages by year
# 1975 1978 1981 1984 
#   84  209   63   91 



#==============================================================================
# Weight the CRFS lengths by district catch
#==============================================================================
catch <- read.csv(file = file.path(here(), "data", "rec_catch", "forSS", "crfs_catch_by_port_save.csv"))

weight <- catch %>%
  group_by(year, area, mode) %>%
  reframe(
    port = ports,
    percent = catch_mt/sum(catch_mt)
  )

crfs$ports <- sapply(strsplit(crfs$RECFIN_PORT_NAME, '\\s*[()]'), '[',1)
# create a subset df from the crfs data with the info I need
crfs_subset <- crfs[, c('year', 'area', 'mode', 'ports', 'sex', 'lengthcm')]
bins <- c(-999, length_bins, Inf)
crfs_subset$len_bins <- bins[findInterval(crfs_subset$lengthcm, bins, all.inside = TRUE)]

nlens_by_port <- crfs_subset[crfs_subset$year != 2004, ] %>%
  group_by(year, area, mode, ports, len_bins) %>%
  rename(port = ports) %>%
  reframe(n = length(len_bins))

lens_w_weights <- inner_join(nlens_by_port, weight, by = c('year', 'area', 'mode', 'port'))
lens_w_weights$n_weight <- lens_w_weights$n * lens_w_weights$percent

out <- lens_w_weights %>%
  group_by(year,area, mode, len_bins) %>%
  reframe(p = sum(n_weight))
write.csv(out, file = file.path(dir, "forSS", "weighted_crfs_lengths.csv"), row.names = FALSE)

samples_by_port <- crfs_subset[crfs_subset$year != 2004, ] %>%
  group_by(year, area, mode, ports) %>%
  rename(port = ports) %>%
  reframe(
    n = length(len_bins)) %>%
  group_by(year, area, mode) %>%
  mutate(
    length_percent = n / sum(n))

test <- inner_join(samples_by_port, weight, by = c('year', 'area', 'mode', 'port'))
colnames(test)[7] <- "catch_percent"
write.csv(test, file = file.path(dir, "forSS", "comparison_length_and_catch_percent_by_port.csv"),
      row.names = FALSE)

#==============================================================================
# Plot the weighted comps and compare to the unweighted
#==============================================================================
weighted_south <- read.csv(file = file.path(dir, "forSS", "weighted_crfs_lengths_south.csv"))
weighted_south_cpfv <- weighted_south[weighted_south$fleet == 3, 1:29]
weighted_south_pr <- weighted_south[weighted_south$fleet == 4, 1:29]
weighted_north <- read.csv(file = file.path(dir, "forSS", "weighted_crfs_lengths_north.csv"))
weighted_north_cpfv <- weighted_north[weighted_north$fleet == 3, 1:29]
weighted_north_pr <- weighted_north[weighted_north$fleet == 4, 1:29]

unweighted_north_cpfv <- read.csv(file = file.path(dir, "forSS", "north_cpfv_all_sources_not_expanded_length_comp_sex_0_rm_mrfss_overlap.csv"))
unweighted_north_pr <- read.csv(file = file.path(dir, "forSS", "north_private_all_sources_not_expanded_length_comp_sex_0_rm_mrfss_overlap.csv"))
unweighted_north_cpfv <- unweighted_north_cpfv[unweighted_north_cpfv$year > 2004, 1:29]
unweighted_north_pr <- unweighted_north_pr[unweighted_north_pr$year > 2004, 1:29]

unweighted_north_cpfv <- cbind(unweighted_north_cpfv[, 1:6], 100*unweighted_north_cpfv[,7:29]/apply(unweighted_north_cpfv[,7:29], 1, sum))
unweighted_north_pr <- cbind(unweighted_north_pr[, 1:6], 100*unweighted_north_pr[,7:29]/apply(unweighted_north_pr[,7:29], 1, sum))

unweighted_south_cpfv <- read.csv(file = file.path(dir, "forSS", "south_cpfv_all_sources_not_expanded_length_comp_sex_0_rm_mrfss_overlap.csv"))
unweighted_south_pr <- read.csv(file = file.path(dir, "forSS", "south_private_all_sources_not_expanded_length_comp_sex_0_rm_mrfss_overlap.csv"))
unweighted_south_cpfv <- unweighted_south_cpfv[unweighted_south_cpfv$year > 2004, 1:29]
unweighted_south_pr   <- unweighted_south_pr[unweighted_south_pr$year > 2004, 1:29]

unweighted_south_cpfv <- cbind(unweighted_south_cpfv[, 1:6], 100*unweighted_south_cpfv[,7:29]/apply(unweighted_south_cpfv[,7:29], 1, sum))
unweighted_south_pr   <- cbind(unweighted_south_pr[, 1:6], 100*unweighted_south_pr[,7:29]/apply(unweighted_south_pr[,7:29], 1, sum))

library(HandyCode)

pngfun(wd = file.path(dir, "plots"), file = "comparison_of_weighted_lengths_south_cpfv.png", w = 12, h = 12)
par(mfrow = c(4, 5), mar = c(1, 1,1,1), oma = c(2,2,2, 2))
for(y in unique(year)){
  plot(0, bty = 'n', ylim = c(0,25), xlim = c(10, 54), ylab = "Density", xlab = "Length (cm)")
  lines(length_bins, unweighted_south_cpfv[unweighted_south_cpfv$year == y, 7:29], lty = 1, lwd = 2)
  lines(length_bins, weighted_south_cpfv[weighted_south_cpfv$year == y, 7:29], lty = 2, lwd = 2, col = 'blue')
}
legend('topright', bty = 'n', col = c(1, 'blue'), legend = c("unweighted", "weighted"), lwd = 2, lty = c(1,2), 
       cex = 2)
dev.off()


pngfun(wd = file.path(dir, "plots"), file = "comparison_of_weighted_lengths_south_pr.png", w = 12, h = 12)
par(mfrow = c(4, 5), mar = c(1, 1,1,1), oma = c(2,2,2, 2))
for(y in unique(year)){
  plot(0, bty = 'n', ylim = c(0,25), xlim = c(10, 54), ylab = "Density", xlab = "Length (cm)")
  lines(length_bins, unweighted_south_pr[unweighted_south_pr$year == y, 7:29], lty = 1, lwd = 2)
  lines(length_bins, weighted_south_pr[weighted_south_pr$year == y, 7:29], lty = 2, lwd = 2, col = 'blue')
}
legend('topright', bty = 'n', col = c(1, 'blue'), legend = c("unweighted", "weighted"), lwd = 2, lty = c(1,2), 
       cex = 2)
dev.off()


pngfun(wd = file.path(dir, "plots"), file = "comparison_of_weighted_lengths_north_pr.png", w = 12, h = 12)
par(mfrow = c(4, 5), mar = c(1, 1,1,1), oma = c(2,2,2, 2))
for(y in unique(year)){
  plot(0, bty = 'n', ylim = c(0,25), xlim = c(10, 54), ylab = "Density", xlab = "Length (cm)")
  lines(length_bins, unweighted_north_pr[unweighted_north_pr$year == y, 7:29], lty = 1, lwd = 2)
  lines(length_bins, weighted_north_pr[weighted_north_pr$year == y, 7:29], lty = 2, lwd = 2, col = 'blue')
}
legend('topright', bty = 'n', col = c(1, 'blue'), legend = c("unweighted", "weighted"), lwd = 2, lty = c(1,2), 
       cex = 2)
dev.off()

pngfun(wd = file.path(dir, "plots"), file = "comparison_of_weighted_lengths_north_cpfv.png", w = 12, h = 12)
par(mfrow = c(4, 5), mar = c(1, 1,1,1), oma = c(2,2,2, 2))
for(y in unique(year)){
  plot(0, bty = 'n', ylim = c(0,25), xlim = c(10, 54), ylab = "Density", xlab = "Length (cm)")
  lines(length_bins, unweighted_north_cpfv[unweighted_north_cpfv$year == y, 7:29], lty = 1, lwd = 2)
  lines(length_bins, weighted_north_cpfv[weighted_north_cpfv$year == y, 7:29], lty = 2, lwd = 2, col = 'blue')
}
legend('topright', bty = 'n', col = c(1, 'blue'), legend = c("unweighted", "weighted"), lwd = 2, lty = c(1,2), 
       cex = 2)
dev.off()

ggplot(test[test$area == "south", ], alpha = 0.2) +
  geom_point(aes(x = year, y = -0.25, size = length_percent), color = 'blue') + 
  geom_point(aes(x = year, y = 0.25, size = catch_percent), color = 'orange') + 
  facet_wrap('port') +
  ylim(c(-0.5, 0.5)) +
  theme(axis.text = element_text(size = 0),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 0),
        strip.text.y = element_text(size = 14))

ggplot(crfs_subset, aes(x = lengthcm, color = as.factor(year))) +
  geom_density() +
  facet_wrap("ports")

plot_comps(
  data = weighted_south[weighted_south$fleet == 4, ],
  plot = 2,
  dir = dir,
  add_save_name = "south_pr_weighted"
)

plot_comps(
  data = weighted_south[weighted_south$fleet == 3, ],
  plot = 2,
  dir = dir,
  add_save_name = "south_cpfv_weighted"
)

plot_comps(
  data = unweighted_south_pr,
  plot = 2,
  dir = dir,
  add_save_name = "south_pr_unweighted"
)

plot_comps(
  data = unweighted_south_cpfv,
  plot = 2,
  dir = dir,
  add_save_name = "south_cpfv_unweighted"
)

data <- unweighted_south_pr
sex_type <- unique(data$sex) 
N <- data[, "InputN"]
year <- as.numeric(as.character(data$year))
sex <- unique(data$sex)
comps <- data[, -c(1:6)]

# Check to see if the unsexed or single sexed comps are 
# double printed
if (sum(grepl(".", colnames(comps), fixed = TRUE)) > 0 ) {
  comps <- comps[, !grepl(".", colnames(comps), fixed = TRUE)]
}
num <- ncol(comps) / ifelse(sex == 3, 2, 1)

# Determine if entries are proportions (e.g., sum to 1 or 100)
# and convert if needed
comps <- 100 * comps / apply(comps, 1, sum)

mod_comps <- cbind(year, comps)
df <- reshape2::melt(mod_comps, id = "year")
df$year <- factor(df$year, levels = unique(df$year))
df$sex  <- substr(df$variable, 1, 1)
df$sex  <- replace(df$sex, df$sex == "F", "FEMALE")
df$sex  <- replace(df$sex, df$sex == "M", "MALE")
df$sex  <- replace(df$sex, df$sex == "U", "UNSEXED")
df$sex  <- factor(df$sex, levels = unique(df$sex))
df$variable <- utils::type.convert(gsub('[FMU]', '', df$variable), as.is = TRUE)
df$n <- 0; a <- 1
for(y in year){
  df$n[df$year == y] <- N[a]
  a <- a + 1
}

ylabel <- "Length (cm)"
bub_step <- ifelse(max(df$value) < 50, 5, 10)
bub_range <- c(1, seq(bub_step, floor(max(df$value)), bub_step))
max_range <- 15
if(max(df$variable) - min(df$variable) > 40 ){
  y_axis <- seq(min(df$variable), max(df$variable), by = 10)
} else {
  y_axis <- seq(min(df$variable), max(df$variable), by = 5)
}

df2 <- df
df2$value <- df2$value / 100
df2[df2$sex == "MALE", 'value'] <- -1 * df2[df2$sex == "MALE", 'value']

p2 <- ggplot2::ggplot(df2, aes(x = variable, y = value)) +
  geom_line(aes(colour = sex), lwd = 1.1) +
  facet_wrap(facets = "year") +
  scale_fill_manual(values = c('FEMALE' = 'red', 'MALE' = 'blue', 'UNSEXED' = "darkseagreen")) +
  scale_color_manual(values = c('FEMALE' = 'darkred', 'MALE' = 'darkblue', 'UNSEXED' = "darkgreen")) +
  labs(x = ylabel, y = "Proportion") +
  geom_hline(yintercept = 0) +
  theme(legend.key = element_blank(), 
        axis.title.x = element_text (size = 12),
        axis.title.y = element_text (size = 12),
        axis.text.x = element_text(colour = "black", size = 12, angle = 90, vjust = 0.3, hjust = 1), 
        axis.text.y = element_text(colour = "black", size = 12), 
        legend.text = element_text(size = 10, colour ="black"), 
        legend.title = element_text(size = 12), 
        panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
        legend.position = "right")
print(p2) 

#=============================================================================================
# Look at weighting the CRFS lengths based on retained and released
#=============================================================================================

load(file.path(here(), "data", "rec_catch", "crfss_catch_filtered_march_2023.rdata"))
catch <- crfs
catch$catch_mt <- catch$TOTAL_MORTALITY_MT
catch$retained <- catch$RETAINED_MT
catch$discard <- catch$RELEASED_DEAD_MT

# Going to include the amount of fish recorded as released alive. There could be
# an argument otherwise. The real question is whether the released lengths come from
# dead fish only or a combination of fish that are determined to live or die. I assume
# it is both since the released alive is probably simply being estimated based on the
# discard mortality rate.
tmp <- catch %>%
  filter(mode == 'cpfv', RECFIN_WATER_AREA_NAME != "INLAND") %>%
  group_by(area, year) %>%
  reframe(
    retained = sum(retained),
    released = sum(RELEASED_ALIVE_MT) + sum(discard),
    total = retained + released,
  ) %>%
  mutate(
    per_rel = released / total,
    per_ret =  1 - per_rel
  )

bds <- crfss_bds
bds <- bds %>% filter(mode == 'cpfv')
table(bds$year, bds$IS_RETAINED, bds$area)

ggplot(tmp) +
  geom_point(aes(x = year, y = per_rel, color = area)) +
  geom_line(aes(x = year, y = per_rel, color = area))

length_bins <- c(seq(10, 54, 2))
tmp_len <- bds %>% 
  filter(year == 2022) %>%
  reframe(
    area = area,
    age = NA,
    year = year,
    length_cm = lengthcm,
    is_retained = IS_RETAINED,
    sex = "U",
    trip = paste0(RECFIN_DATE, COUNTY_NUMBER, AGENCY_WATER_AREA_NAME)
  )

n <- tmp_len %>%
  dplyr::group_by(area, year, sex, is_retained) %>%
  dplyr::summarise(
    ntrip = length(unique(trip)))

lfs_ret <-  UnexpandedLFs.fn(
  datL = tmp_len[tmp_len$area == "north" & tmp_len$is_retained == "RETAINED", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 3, 
  month = 7
)$unsexed

lfs_rel <-  UnexpandedLFs.fn(
  datL = tmp_len[tmp_len$area == "north" & tmp_len$is_retained == "RELEASED", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 3, 
  month = 7
)$unsexed

wght <- tmp %>% filter(year == 2022, area == "north")
comp <- wght$per_ret * lfs_ret[,7:ncol(lfs_ret)] + wght$per_rel * lfs_rel[,7:ncol(lfs_rel)]
comp <- 100* comp[,1:23] / sum(comp[,1:23])
out <- cbind(lfs_ret[, 1:6], comp, comp)
out$InputN <- floor(sum(n[n$area == "north" & n$is_retained == "RETAINED", "ntrip"] * wght$per_ret +
                  n[n$area == "north" & n$is_retained == "RELEASED", "ntrip"] * wght$per_rel))
write.csv(out, file = file.path(dir, "forSS", "weighted_release_retained_comp_north_cpfv_2022.csv"), row.names = FALSE)

plot(length_bins, 100 * lfs_ret[, 7:29] / sum(lfs_ret[, 7:29]), type = 'l', lwd = 2)
lines(length_bins, comp, lty = 2,  lwd = 2, col = 'blue')

lfs_ret <-  UnexpandedLFs.fn(
  datL = tmp_len[tmp_len$area == "south" & tmp_len$is_retained == "RETAINED", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 3, 
  month = 7
)$unsexed

lfs_rel <-  UnexpandedLFs.fn(
  datL = tmp_len[tmp_len$area == "south" & tmp_len$is_retained == "RELEASED", ], 
  lgthBins = length_bins,
  partition = 0, 
  fleet = 3, 
  month = 7
)$unsexed

wght <- tmp %>% filter(year == 2022, area == "south")
comp <- wght$per_ret * lfs_ret[,7:ncol(lfs_ret)] + wght$per_rel * lfs_rel[,7:ncol(lfs_rel)]
comp <- 100* comp[,1:23] / sum(comp[,1:23])
out <- cbind(lfs_ret[, 1:6], comp, comp)
out$InputN <- floor(sum(n[n$area == "south" & n$is_retained == "RETAINED", "ntrip"] * wght$per_ret +
                        n[n$area == "south" & n$is_retained == "RELEASED", "ntrip"] * wght$per_rel))
write.csv(out, file = file.path(dir, "forSS", "weighted_release_retained_comp_south_cpfv_2022.csv"), row.names = FALSE)

plot(length_bins, 100 * lfs_ret[, 7:29] / sum(lfs_ret[, 7:29]), type = 'l', lwd = 2)
lines(length_bins, comp, lty = 2,  lwd = 2, col = 'blue')
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
ggsave(file = file.path(dir, "plots", "crfs_mean_length_by_port.png"), width = 7, height = 7)


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
  facet_wrap(facets = c("program")) 

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
#ggsave(filename = file.path(dir, "plots", "rec_south_length_boxplot_by_mode_program_year.png"),
#       width = 10, height = 10)

ggplot(some_data, aes(x = lengthcm, group = district, color = district)) +
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
  xlab("Length (cm)") + ylab("Density") +
  facet_wrap(facets = c("program")) #+ 
#scale_color_viridis_d()


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


