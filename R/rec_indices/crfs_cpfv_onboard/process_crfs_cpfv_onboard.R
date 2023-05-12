################################################################################
### Process the CDFW CPFV onbard data for an index of abundance
### Depending on the species and the area you may need to modify the filters
### Copper assessment 2023
### Melissa Monk
################################################################################
rm(list = ls(all = TRUE))
graphics.off()
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)
library(ggridges)
# species and area identifiers - eventually put in function
pacfinSpecies <- "COPP"
speciesName <- "copper"
modelArea <- "north"
model <- "start2005"
# setwd to the north or the south
# set working directory
temp_dir <- "S:/copper_rockfish_2023"
dir <- file.path(temp_dir, "data", "rec_indices", "crfs_cpfv_onboard", modelArea)
setwd(dir)

# load data for processing
load(file.path(temp_dir, "data", "rec_indices", "crfs_cpfv_onboard", "onboard.RData"))

alphahull <- readxl::read_xlsx(file.path(
  temp_dir, "data", "rec_indices", "crfs_cpfv_onboard",
  "SouthernAlphaHulls", "AlphaHull_coppers.xlsx"
))

alphahull <- alphahull %>%
mutate(ID = paste0(ASSN,"_", LOCNUM)) %>%
dplyr::select(ID, AlphaHull)

# Data filter dataframe
filter.num <- 1
dataFilters <- data.frame(matrix(vector(), 10, 4,
  dimnames = list(c(), c(
    "Filter", "Description", "Samples",
    "Positive_Samples"
  ))
), stringsAsFactors = F)
#-------------------------------------------------------------------------------
onboard <- onboard_data %>%
  mutate(area = ifelse(district %in% c(1, 2), "south", "north")) %>%
  filter(area == modelArea) %>%
  filter(effort > 0)


# Get the number of available samples by year
samples_year_district <- onboard %>%
  group_by(year, district) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = district, values_from = n)
# View(samples_year_district)
write.csv(samples_year_district, "samples_year_district.csv")

# exploratory plots
ggplot(onboard, aes(x = as.factor(year), y = cpue)) +
  geom_boxplot() +
  xlab("Year") +
  ylab("CPUE")
ggsave(file.path(dir, "cpue_by_year.png"), height = 7, width = 7)

if (modelArea == "south") {
  onboard <- onboard %>%
    mutate(region = case_when(
      cdfw.block %in% c(876:879, 860:861, 842:843, 821:822, 801:802, 756:757, 737:740, 718:720, 681:682, 702:703) ~ "District 1 mainland",
      cdfw.block %in% c(651:658, 666, 667, 680, 683, 679, 701, 705) ~ "District 2 mainland",
      cdfw.block %in% c(888, 870, 867, 849:850, 829, 806:808, 813:815, 760, 768:769, 764:765, 761:762, 744:745, 724) ~ "Southern Channel Islands",
      cdfw.block %in% c(684:691, 707:713) ~ "Northern Channel Islands",
      TRUE ~ cdfw.block
    ))

#also add the alpha hulls
onboard <- left_join(onboard, alphahull, by = "ID")
}



#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("All data")
dataFilters$Description[filter.num] <- c("All data")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# depth by district or area fished
ggplot(
  onboard %>% filter(number.fish > 0, depth1ft < 250),
  aes(
    x = depth1ft / 6, y = as.factor(year),
    fill = as.factor(year)
  )
) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Depth (fm)") +
  ylab("Year") +
  # geom_density_ridges(onboard, aes(x = depth, y = year, colour = year)) +
  scale_fill_viridis_d()
ggsave(file.path(dir, "copper_depths_nofilter.png"))


# Proportion discarded by year
keep_discard <- onboard %>%
  group_by(year) %>%
  summarise(
    kept = sum(kept),
    discard = sum(discd)
  ) %>%
  mutate(`Proportion discarded` = discard / (kept + discard))
write.csv(keep_discard, file.path(dir, "keep_discard_prop.csv"), row.names = FALSE)





# remove 1999-2003 ----
if (model == "start2005") {
  onboard <- onboard %>%
    filter(year > 2003)

  #-------------------------------------------------------------------------------
  # Add to filter dataframe
  dataFilters$Filter[filter.num] <- c("Years")
  dataFilters$Description[filter.num] <- c("Start time series in 2005 due to sparse data")
  dataFilters$Samples[filter.num] <- onboard %>% tally()
  dataFilters$Positive_Samples[filter.num] <- onboard %>%
    filter(number.fish > 0) %>%
    tally()
  filter.num <- filter.num + 1
  #-------------------------------------------------------------------------------
}

# IF Depth isn't available and GIS depth is - add that in
# ONLY using starting depth which is depth1ft, and the gis depths are based on
# start locations
# remove any remaining drifts with no depth information
onboard <- onboard %>%
  mutate(depth = ifelse(is.na(depth1ft) & gis90depthft > 0, gis90depthft, depth1ft)) %>%
  filter(!is.na(depth))

# how many positive drifts is depth missing from
pos_data <- onboard %>% filter(number.fish > 0)
summary(pos_data$depth)
summary(as.factor(pos_data$LocationTableError))




# check fish time and observed anglers
summary(onboard$fishtime)
summary(onboard$obsang)

# remove any drifts with a location table error, missing fish time and missing
# number of anglers

onboard <- onboard %>%
  filter(is.na(LocationTableError)) %>%
  filter(!is.na(fishtime)) %>%
  filter(!is.na(obsang)) %>%
  filter(!is.na(reef)) %>%
  filter(fishtime > 0)

# missing reef info
summary(onboard$reef)

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Errors and Missing Data")
dataFilters$Description[filter.num] <- c("Remove drifts with missing data and identified errors")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# Keep ocean waters only
onboard <- onboard %>%
  filter(waterarea %in% c("N", "O"))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Area fished")
dataFilters$Description[filter.num] <- c("Remove drifts in bays and Mexico (if applicable)")
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------



# remove Jan-March in the north - no rockfishing these months
# remove Jan-Feb in the south - no rockfishing these months
if (modelArea == "north") {
  onboard <- onboard %>%
    filter(month %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12))
} else {
  onboard <- onboard %>%
    filter(month %in% c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
}

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Months fished")
if (modelArea == "north") {
  dataFilters$Description[filter.num] <- c("Remove Jan-March; recreational rockfish fishery closed")
} else {
  dataFilters$Description[filter.num] <- c("Remove Jan-Feb; recreational rockfish fishery closed")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# positive data
pos_data <- onboard %>%
  filter(number.fish > 0) %>%
  filter(depth < 500)
# look at depth distribution
ggplot(pos_data, aes(x = depth, fill = district)) +
  geom_histogram() +
  xlab("Depth (ft)") +
  ylab("Number of drifts with copper") +
  scale_fill_viridis_d()
ggsave(file = file.path(dir, "drifts_by_depth_district.png"), width = 7, height = 7)

depth_quantile2 <- quantile(onboard$depth, seq(0, 1, .01))
depth_quantile1 <- quantile(pos_data$depth, seq(0, 1, .01))
depth_quantile <- round(quantile(pos_data$depth, c(0.01, .99)), 0)
depth_quantile
# remove upper and lower 1%
onboard <- onboard %>%
  filter(depth <= 300,
         depth >= 50) %>%
 # filter(!region == "0") %>%
  droplevels()
# %in% (depth_quantile[1]:depth_quantile[2]))

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Depth")
if (modelArea == "north") {
  dataFilters$Description[filter.num] <- c("Remove upper and lower 1% of depth with observed coppers;
                                           Remaining drifts between 50 and 300 feet")
} else {
  dataFilters$Description[filter.num] <- c("Remove drifts in depths greater than 60 fathoms")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

# look at number of observed anglers
summary(onboard$obsang)
obsang_quantile <- quantile(pos_data$obsang, seq(0, 1, .025))
obsang_quantile

# remove upper and lower 2.5%
# CRFS manual suggests a max number of anglers to observe is 12
onboard <- onboard %>%
  filter(obsang %in% obsang_quantile[2]:obsang_quantile[40])

#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Observed anglers")
if (modelArea == "north") {
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% of observed anglers;
                                           Remaining drifts with 4-12 observed anglers")
} else {
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% of observed anglers;
                                           Remaining data: Observed anglers 4-14")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

summary(onboard$fishtime)
fishtime_quantile <- quantile(pos_data$fishtime, seq(0, 1, .025))
fishtime_quantile

# remove upper and lower 2.5%
# Drifts less than 5 minutes probably were not successful
onboard <- onboard %>%
  filter(
    fishtime > fishtime_quantile[2],
    fishtime < fishtime_quantile[40]
  )


#-------------------------------------------------------------------------------
# Add to filter dataframe
dataFilters$Filter[filter.num] <- c("Time fished")
if (modelArea == "north") {
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% time fished and
                                         time fished; Remaining drifts with 3-75 minutes time fished")
} else {
  dataFilters$Description[filter.num] <- c("Remove upper and lower 2.5% time fished and
                                         time fished; Remaining drifts with 5-102 minutes time fished")
}
dataFilters$Samples[filter.num] <- onboard %>% tally()
dataFilters$Positive_Samples[filter.num] <- onboard %>%
  filter(number.fish > 0) %>%
  tally()
filter.num <- filter.num + 1
#-------------------------------------------------------------------------------

if (modelArea == "north") {
  # subset automatically to 500m
  onboard <- onboard %>%
    filter(reef.dist < 500)

  # Look at distance from reef for the positives
  pos_data <- onboard %>% filter(number.fish > 0)

  summary(pos_data$reef.dist)

  ggplot(pos_data, aes(x = reef.dist, y = cpue, colour = district)) +
    geom_jitter(alpha = 0.5)

  # Cumulative distribution of distance from reef
  ggplot(pos_data, aes(reef.dist)) +
    stat_ecdf(geom = "step")


  reef.dist_quantile <- quantile(pos_data$reef.dist, seq(0, 1, .01))
  reef.dist_quantile
  # keep 95% of the data

  onboard <- onboard %>%
    filter(reef.dist < reef.dist_quantile[96]) %>%
    droplevels()

  onboard <- onboard %>%
    mutate(region = district)
  #-------------------------------------------------------------------------------
  # Add to filter dataframe
  dataFilters$Filter[filter.num] <- c("Distance from rocky substrate")
  dataFilters$Description[filter.num] <- c("After removing observations further
than 0.5km from rocky substrate, keep 95% of the data; drifts within 41.7 m of rocky substrate")
  dataFilters$Samples[filter.num] <- onboard %>% tally()
  dataFilters$Positive_Samples[filter.num] <- onboard %>%
    filter(number.fish > 0) %>%
    tally()
  filter.num <- filter.num + 1
  #-------------------------------------------------------------------------------
}

# #to filter by reef distance
# if (modelArea == "south") {
#   # Look at distance from reef for the positives and then by region
#   pos_data <- onboard %>%
#     filter(number.fish > 0) %>%
#     filter(reef.dist < 5000)
#   summary(pos_data$reef.dist)
#   reef.dist_quantile <- quantile(pos_data$reef.dist, seq(0, 1, .01))
#   reef.dist_quantile

#   # district 1
#   pos_data1 <- onboard %>%
#     filter(number.fish > 0) %>%
#     filter(reef.dist < 5000, region == "District 1 mainland")
#   reef.dist_quantile1 <- quantile(pos_data1$reef.dist, seq(0, 1, .01))
#   reef.dist_quantile1

#   with(pos_data, table(region))

#   ggplot(pos_data, aes(x = reef.dist, y = cpue, colour = region)) +
#     geom_jitter(alpha = 0.5, show.legend = FALSE) +
#     facet_wrap(~region) +
#     xlab("Distance to rocky subs")
#   ggsave(file.path(dir, "reef_distance.png"), height = 7, width = 7)
#   # Cumulative distribution of distance from reef
#   ggplot(pos_data, aes(reef.dist)) +
#     xlab("Distance to rocky substrate") +
#     stat_ecdf(geom = "step", color = "purple", show.legend = FALSE) +
#     facet_wrap(~region)
#   ggsave(file.path("ecdf_rocky_substrate.png"), width = 7, height = 7)


#   with(pos_data, table(cdfw.block))



#   onboard <- onboard %>%
#     filter(reef.dist < reef.dist_quantile[96]) %>%
#     droplevels()


#   #-------------------------------------------------------------------------------
#   # Add to filter dataframe
#   dataFilters$Filter[filter.num] <- c("Distance from rocky substrate")
#   dataFilters$Description[filter.num] <- c("Southern CA rocky substrate incomplete;
#                                          keep 95% of the data; drifts within 3,365 m of rocky substrate")
#   dataFilters$Samples[filter.num] <- onboard %>% tally()
#   dataFilters$Positive_Samples[filter.num] <- onboard %>%
#     filter(number.fish > 0) %>%
#     tally()
#   filter.num <- filter.num + 1
#   #-------------------------------------------------------------------------------

#   pos_data <- onboard %>% filter(number.fish > 0)
#   # look at reef distance
#   summary(pos_data$reef.dist)
#   quantile(pos_data$reef.dist, seq(0, 1, .01))
#   ggplot(pos_data, aes(x = reef.dist, y = cpue)) +
#     geom_jitter(alpha = 0.5, colour = "purple") +
#     xlab("Distance to rocky substrage (m)") +
#     ylab("CPUE") +
#     theme_bw() +
#     facet_wrap(~district)
#   ggsave(file.path(dir, "reef_dist_cpue.png"), height = 7, width = 7)

#   # primary target species only
#   cdfwblockTargets <- onboard %>%
#     group_by(cdfw.block) %>%
#     summarise(
#       driftsWithTarget = sum(number.fish > 0),
#       driftsWOTarget = sum(number.fish == 0)
#     ) %>%
#     mutate(
#       totaldrifts = driftsWithTarget + driftsWOTarget,
#       percentpos = driftsWithTarget / (driftsWithTarget + driftsWOTarget)
#     ) %>%
#     filter(percentpos > 0.05) %>%
#     filter(!cdfw.block == 0) %>%
#     filter(totaldrifts > 99)
#   # View(cdfwblockTargets)
#   # write.csv(driftTargets,
#   #          file.path(dir, "driftTargets.csv"),
#   #          row.names=FALSE)

#   onboard <- onboard %>%
#     filter(cdfw.block %in% cdfwblockTargets$cdfw.block) %>%
#     droplevels()

#   #-------------------------------------------------------------------------------
#   # Add to filter dataframe
#   dataFilters$Filter[filter.num] <- c("CDFW block")
#   dataFilters$Description[filter.num] <- c("Retain drifts in CDFW blocks with at least 100 drifts and
#                                          more than 5% of all drifts in that block
#                                          caught a copper rockfish")
#   dataFilters$Samples[filter.num] <- onboard %>% tally()
#   dataFilters$Positive_Samples[filter.num] <- onboard %>%
#     filter(number.fish > 0) %>%
#     tally()
#   filter.num <- filter.num + 1
#   #-------------------------------------------------------------------------------
# }

###To filter by alpha hull
#to filter by reef distance
if (modelArea == "south") {
  
  #remove the drifts outside an alpha hull

  onboard <- onboard %>%
  filter(!is.na(AlphaHull))

  pos_data <- onboard %>%
    filter(number.fish > 0)
 
  summary(as.factor(pos_data$AlphaHull))
  summary(as.factor(onboard$AlphaHull))

  # ggplot(pos_data, aes(x = reef.dist, y = cpue, colour = region)) +
  #   geom_jitter(alpha = 0.5, show.legend = FALSE) +
  #   facet_wrap(~region) +
  #   xlab("Distance to rocky subs")
  # ggsave(file.path(dir, "reef_distance.png"), height = 7, width = 7)
  # Cumulative distribution of distance from reef
  # ggplot(pos_data, aes(reef.dist)) +
  #   xlab("Distance to rocky substrate") +
  #   stat_ecdf(geom = "step", color = "purple", show.legend = FALSE) +
  #   facet_wrap(~region)
  # ggsave(file.path("ecdf_rocky_substrate.png"), width = 7, height = 7)


  # with(pos_data, table(cdfw.block))

  # pos_data <- onboard %>% filter(number.fish > 0)
  # # look at reef distance
  # summary(pos_data$reef.dist)
  # quantile(pos_data$reef.dist, seq(0, 1, .01))
  # ggplot(pos_data, aes(x = reef.dist, y = cpue)) +
  #   geom_jitter(alpha = 0.5, colour = "purple") +
  #   xlab("Distance to rocky substrage (m)") +
  #   ylab("CPUE") +
  #   theme_bw() +
  #   facet_wrap(~district)
  # ggsave(file.path(dir, "reef_dist_cpue.png"), height = 7, width = 7)

  # # primary target species only
  # cdfwblockTargets <- onboard %>%
  #   group_by(cdfw.block) %>%
  #   summarise(
  #     driftsWithTarget = sum(number.fish > 0),
  #     driftsWOTarget = sum(number.fish == 0)
  #   ) %>%
  #   mutate(
  #     totaldrifts = driftsWithTarget + driftsWOTarget,
  #     percentpos = driftsWithTarget / (driftsWithTarget + driftsWOTarget)
  #   ) %>%
  #   filter(percentpos > 0.05) %>%
  #   filter(!cdfw.block == 0) %>%
  #   filter(totaldrifts > 99)
  # # View(cdfwblockTargets)
  # # write.csv(driftTargets,
  # #          file.path(dir, "driftTargets.csv"),
  # #          row.names=FALSE)

  # onboard <- onboard %>%
  #   filter(cdfw.block %in% cdfwblockTargets$cdfw.block) %>%
  #   droplevels()

  #-------------------------------------------------------------------------------
  # Add to filter dataframe
  dataFilters$Filter[filter.num] <- c("Inferred habitat")
  dataFilters$Description[filter.num] <- c("Retain drifts within the alpha hulls from positive observations")
  dataFilters$Samples[filter.num] <- onboard %>% tally()
  dataFilters$Positive_Samples[filter.num] <- onboard %>%
    filter(number.fish > 0) %>%
    tally()
  filter.num <- filter.num + 1
  #-------------------------------------------------------------------------------
}



# depth by district or area fished
ggplot(
  onboard %>% filter(number.fish > 0),
  aes(
    x = depth / 6, y = as.factor(year),
    fill = as.factor(year)
  )
) +
  geom_density_ridges(show.legend = FALSE) +
  xlab("Depth (fm)") +
  ylab("Year") +
  # geom_density_ridges(onboard, aes(x = depth, y = year, colour = year)) +
  scale_fill_viridis_d()
ggsave(file.path(dir, "copper_depths_gisdepthadded.png"))


# look at years
summary(as.factor(onboard$year))


# look at district
summary(as.factor(onboard$district))
# will weight by district

# final tables and visualizations
# Get the number of available samples by year
samples_year_district <- onboard %>%
  group_by(year, district) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = district, values_from = n)
# View(samples_year_district)
write.csv(samples_year_district,
  file.path(dir, "samples_by_year_district.csv"),
  row.names = FALSE
)

# primary target species only
driftTargets <- onboard %>%
  group_by(district) %>%
  summarise(
    driftsWithTarget = sum(number.fish > 0),
    driftsWOTarget = sum(number.fish == 0)
  ) %>%
  mutate(
    totaldrifts = driftsWithTarget + driftsWOTarget,
    percentpos = driftsWithTarget / (driftsWithTarget + driftsWOTarget)
  )
write.csv(driftTargets,
  file.path(dir, "driftTargets.csv"),
  row.names = FALSE
)


# look at water area by district
# you can see in 2017 that they started fishing outside state waters
waterarea_year <- onboard %>%
  filter(number.fish > 0) %>%
  group_by(waterarea, district) %>%
  tally() %>%
  pivot_wider(names_from = waterarea, values_from = n) %>%
  mutate(percent_N = N / (N + O))
write.csv(waterarea_year, file.path(dir, "target_water_area.csv"))


# sample sizes by month
samples_month_year <- onboard %>%
  group_by(year, month) %>%
  tally() %>%
  tidyr::pivot_wider(names_from = month, values_from = n)
samples_month_year



cpue_by_district <- onboard %>%
  group_by(year, district) %>%
  summarise(average_cpue = mean(cpue))

# look average cpue by district
ggplot(cpue_by_district, aes(x = year, y = average_cpue, colour = district)) +
  geom_point(size = 3) +
  theme_bw() +
  geom_line(aes(x = year, y = average_cpue, colour = district)) +
  xlab("Year") +
  ylab("Average CPUE") +
  ylim(c(0, (max(cpue_by_district$average_cpue) * 1.1))) +
  scale_color_viridis_d()
ggsave(file = file.path(getwd(), "average_cpue_by_district.png"), width = 7, height = 7)

# look at 2015 in district 5
aa <- subset(onboard, year == 2015 & district == 5)
# one observation


# Assign blocks to areas
if (modelArea == "south") {
  # onboard <- onboard %>%
  #   mutate(region = case_when(
  #     cdfw.block %in% c(876:879, 860:861, 842:843, 821:822, 801:802, 756:757, 737:740, 718:720 , 681:682, 702:703) ~ "District 1 mainland",
  #     cdfw.block %in% c(651:658, 666, 667, 680, 683,  679, 701, 705) ~ "District 2 mainland",
  #     cdfw.block %in% c(888, 870, 867, 849:850, 829, 806:808, 813:815,760,  768:769, 764:765, 761:762, 744:745, 724) ~ "Southern Channel Islands",
  #     cdfw.block %in% c(684:691, 707:713) ~ "Northern Channel Islands",
  #     TRUE ~ cdfw.block
  #   ))
  #


  onboard <- onboard %>%
    mutate_at(vars(region), as.factor)
  summary(onboard$region)
  # mainland south 876:879 , 860:861, 842:843, 821:822, 801:802, 756: 757, 737:740, 718:720 , 681:682,
  # mainland north 651:658,
  #  southern channel islands 888, 870, 867, 849:850,829, 806:808, 813:815, 768:769, 764:765, 761:762, 744:745: 724
  #  norther channel islands 684:691, 707:713


  cpue_by_region <- onboard %>%
    group_by(year, region) %>%
    summarise(average_cpue = mean(cpue))

  # look average cpue by district
  ggplot(cpue_by_region, aes(x = year, y = average_cpue, colour = region)) +
    geom_point(size = 3) +
    theme_bw() +
    geom_line(aes(x = year, y = average_cpue, colour = region)) +
    xlab("Year") +
    ylab("Average CPUE") +
    ylim(c(0, (max(cpue_by_region$average_cpue) * 1.1))) +
    scale_color_viridis_d()
  ggsave(file = file.path(getwd(), "average_cpue_by_region.png"), width = 7, height = 7)
}


# save the datafile and filters for the run file
save(onboard, dataFilters,
  file = file.path(dir, "data_for_glm.RData")
)
