###################################################################################
#
#       Copper rockfish 2023
#        NWFSC WCGBT survey 
# data exploration and processing
#
#############################################################################################

devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
#library(nwfscSurvey)

dir_main <- "C:/Assessments/2023/copper_rockfish_2023/data/wcgbt"

#=====================================================================
# Pull all available data
#=====================================================================

# catch = pull_catch(
#     dir = dir, 
#     common_name = "copper rockfish",
#     survey = "NWFSC.Combo",
#     convert  = TRUE)
# bio = pull_bio(
#     dir = dir, 
#     common_name = "copper rockfish",
#     survey = "NWFSC.Combo",
#     convert = TRUE)
# bio_samps = pull_biological_samples(
#     dir = dir, 
#     common_name = "copper rockfish",
#     survey = "NWFSC.Combo")

load(file.path(dir_main, "catch_copper rockfish_NWFSC.Combo_2022-11-27.rdata"))
catch_orig <- x
load(file.path(dir_main, "bio_copper rockfish_NWFSC.Combo_2022-11-27.rdata"))
bio_orig <- x

PlotMap.fn(dat = catch_orig)
#=====================================================================
# Split the data by assessment area 
#=====================================================================
area = "south"
#area = "north"

if (area == "south"){
    catch = catch_orig[catch_orig$Latitude_dd < 34.47, ]
    bio = bio_orig[bio_orig$Latitude_dd < 34.47, ]
} else {
    catch = catch_orig[catch_orig$Latitude_dd >= 34.47 & catch_orig$Latitude_dd < 42, ]
    bio = bio_orig[bio_orig$Latitude_dd >= 34.47 & bio_orig$Latitude_dd < 42, ]
}

dir = file.path(dir_main, area)

#=====================================================================
# Process the data in the selected area 
#=====================================================================

PlotMap.fn(dir = dir, dat = catch)

# Observations range between 59 - 183 m 
# South - one  observation > 400 m, North - one observation at 359 m
# Peak observations between 75 - 105 m
# hist(catch[catch$cpue_kg_per_ha_der >0, "Depth_m"], breaks = 30)

# Existing SA file only allows splits at 75, 100, 125, 155, 183
strata = CreateStrataDF.fn(names=c("shallow", "mid"), 
                           depths.shallow = c( 55, 100),
                           depths.deep    = c(100, 183),
                           lats.south     = c(32.5, 32.5),
                           lats.north     = c(34.5, 34.5))

num_strata = CheckStrata.fn(dir = dir,  
							dat = catch, 
							strat.df = strata)

file.rename(file.path(dir, "forSS", "strata_observations.csv"),
            file.path(dir, "forSS", paste0(area, "_strata_observations.csv")))

# Calculate the design based index
biomass = Biomass.fn(dir = dir, 
               dat = catch,  
               strat.df = strata) 

file.rename(file.path(dir,  "forSS", "design_based_indices.csv"),
            file.path(dir,  "forSS", paste0(area, "_design_based_indices.csv")))

# Plot the biomass index
PlotBio.fn(dir = dir, 
    dat = biomass, 
    main = paste0("NWFSC WCGBTS - ", area))

PlotBioStrata.fn(dir = dir, 
    dat = biomass) 

#=====================================================================
# Visualize the data
#=====================================================================

plot_bio_patterns(
  dir = dir, 
  bio = bio, 
  col_name = "Length_cm")

temp <- catch[catch$Depth_m < 200, ] %>%
  dplyr::mutate(new = factor(
    cpue_kg_km2 <= 0,
    levels = c(FALSE, TRUE),
    labels = c("Present", "Absent")
  ))

# Plot depth bins (50 m) by presence/absence with default colors
plot_proportion(
  data = temp,
  column_factor = new,
  column_bin = Depth_m,
  width = 10,
  boundary = 0,
  bar_width = "equal"
)
ggplot2::ggsave(filename = file.path(dir, "plots", "proportion_by_depth.png"))

# In the south there are two observations > 150 m of one fish
# at 182.7 and another at 407.8. The last one is suspect. That 
# tow is 183 on 10-20-2008. The nearest previous tow with copper
# occured the day before in tow 179 at 77 meters.
plot_proportion(
  data = bio[bio$Depth_m < 150, ] %>%
    dplyr::mutate(Sex = codify_sex(Sex)),
  column_factor = Sex,
  column_bin = Depth_m,
  width = 10,
  boundary = 0,
  bar_width = "equal"
)
ggplot2::ggsave(filename = file.path(dir, "plots", "sex_by_depth.png"))



# Look at where copper are observed by location
plot(catch$Longitude_dd, catch$Latitude_dd, pch = 16, 
    col = 'grey', xlab = "Longitude", ylab = "Latitude")
pos = which(catch$cpue_kg_km2 > 0)
points(catch$Longitude_dd[pos], catch$Latitude_dd[pos], pch = 16, col = 'red')
legend('topright', bty = 'n', legend = c("All Tows", "Positive Tows"),
    col = c('grey', 'red'), pch = c(16, 16))
ggplot2::ggsave(filename = file.path(dir, "plots", paste0('postive_copper_tows_', area, '.png')))

plot_age_length_sampling(data = bio,
   xlim = c(0, 60),
   ylim = c(0, 0.25),
   dir = dir)

#=====================================================================
# Calculate length compositions
#=====================================================================
len_bin = seq(6, 60, 2)

# Calculate the effN
# Using the others group which is 2.43 unique samples / tow
# The shelf and slope rockfish in contrast have a value of 2.38

# Process the sexed and unsexed fish separately 
n = GetN.fn(
    dir = dir, 
    dat = bio, 
    type = "length", 
    species = "others")

file.rename(file.path(dir, "forSS", "length_SampleSize.csv"),
            file.path(dir, "forSS", paste0(area, "_all_length_sample_size.csv")))

# Process the sexed and unsexed fish separately 
n = GetN.fn(
    dir = dir, 
    dat = bio[bio$Sex %in% c("F", "M"), ], 
    type = "length", 
    species = "others")

file.rename(file.path(dir, "forSS", "length_SampleSize.csv"),
            file.path(dir, "forSS", paste0(area, "_sexed_length_sample_size.csv")))

sexed_length_comps <- SurveyLFs.fn(
    dir = dir, 
	datL = bio[bio$Sex %in% c("F", "M"), ],
    datTows = catch,  
    strat.df = strata,
    lgthBins = len_bin, 
    month = 7, fleet = NA, 
    sex = 3,
    nSamps = n)

plot_comps(dir = dir, 
    add_save_name = "sexed",
    data = sexed_length_comps)

file.rename(file.path(dir, "forSS", "Survey_Sex3_Bins_6_60_LengthComps.csv"),
            file.path(dir, "forSS", paste0(area, "_Survey_Sex3_Bins_6_60_LengthComps.csv")))
file.rename(file.path(dir, "forSS", "Survey_Sex3_Bins_-999_60_LengthComps.csv"),
            file.path(dir, "forSS", paste0(area, "_Survey_Sex3_Bins_-999_60_LengthComps.csv")))

# Check for greater than one observation per year
table(bio[bio$Sex == "U", "Year"])

n = GetN.fn(
    dir = dir, 
    dat = bio[bio$Sex == "U", ], 
    type = "length", 
    species = "others")

file.rename(file.path(dir, "forSS", "length_SampleSize.csv"),
            file.path(dir, "forSS", paste0(area, "_unsexed_length_sample_size.csv")))

unsexed_length_comps <- SurveyLFs.fn(
    dir = dir, 
    datL = bio[bio$Sex == "U", ],
    datTows = catch,  
    strat.df = strata,
    lgthBins = len_bin, 
    month = 7, fleet = NA, 
    sex = 0,
    nSamps = n)

plot_comps(dir = dir, 
    add_save_name = "unsexed",
    data = unsexed_length_comps)

PlotFreqData.fn(dir = dir,
	dat = sexed_length_comps, 
    inch = 0.10,
    main = paste0("NWFSC WCGBTS - ", area, " - Sexed Fish "))

PlotSexRatio.fn(dir = dir, 
    dat = bio, data.type = "length")

#=====================================================================
# Calculate age compositions
#=====================================================================
age_bin = 1:40

# There is only one unsexed fish that has been aged
n <- GetN.fn(
    dir = dir, 
    dat = bio, 
    type = "age", 
    species = "others")

# Marginals
age_comps <- SurveyAFs.fn(
    dir = dir, 
    datA = bio,
    datTows = catch,  
    strat.df = strata,
    ageBins = age_bin, 
    sex = 3, 
    month = 7, 
    fleet = NA, 
    nSamps = n)

plot_comps(dir = dir, 
    data = age_comps)

PlotFreqData.fn(
    dir = dir, 
    dat = age_comps, 
    main = paste0("NWFSC WCGBTS - ", area))

# Conditional-age-at-length
caal <- SurveyAgeAtLen.fn(
    dir = dir, 
    datAL = bio, 
    datTows = catch, 
    strat.df = strata, 
    lgthBins = len_bin, 
    ageBins = age_bin)

# Condense to a single year
caal = bio
caal$Year = 9999
caal_catch = catch
caal_catch$Year = 9999
SurveyAgeAtLen.fn(dir = dir, 
    datAL = caal, 
    datTows = caal_catch, 
    strat.df = strata, 
    lgthBins = len_bin, 
    ageBins = age_bin)

PlotVarLengthAtAge.fn(
    dir = dir, 
    dat = bio)
