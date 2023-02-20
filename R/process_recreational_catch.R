##############################################################################################################
#
# 	Data processing of recreational landings data for
#	  the 2023 copper rockfish assessment 
#			  by Chantel Wetzel 
#					
##############################################################################################################


main_dir = here::here() #"C:/Assessments/2023/copper_rockfish_2023/data/pacfin_catch"
dir <- file.path(main_dir, "data", "rec_catch")
dir.create(file.path(dir, "forSS"), showWarnings = FALSE)

# Load the data items
load(file.path(dir, "crfss_catch_filtered.rdata"))
load(file.path(dir, "mrfss_catch_filtered.rdata")) 
hist <- read.csv(file.path(dir, "2021.spp.rec.n.and.s.conception.csv"))

#===============================================================================================
# CRFS processing 
#===============================================================================================
crfs[crfs$mode == "shoreside", 'mode'] <- "private"

crfs$month <- crfs$RECFIN_MONTH
crfs$catch_mt <- crfs$TOTAL_MORTALITY_MT
crfs$wave[crfs$month %in% 1:2] <- 1
crfs$wave[crfs$month %in% 3:4] <- 2
crfs$wave[crfs$month %in% 5:6] <- 3
crfs$wave[crfs$month %in% 7:8] <- 4
crfs$wave[crfs$month %in% 9:10] <- 5
crfs$wave[crfs$month %in% 11:12] <- 6

crfs_wave <- aggregate(catch_mt ~ year + area + mode + wave, crfs, sum, drop = FALSE)
crfs_wave[is.na(crfs_wave)] <- 0

crfs_month <- aggregate(catch_mt ~ year + area + mode + month, crfs, sum, drop = FALSE)
crfs_month[is.na(crfs_month)] <- 0

#===============================================================================================
# MRFSS processing 
#===============================================================================================
mrfss$orig_catch_mt <- 0.001 * mrfss$WGT_AB1
mrfss$catch_mt <- mrfss$orig_catch_mt
mrfss$wave <- mrfss$WAVE

# Attempt to fill in missing values = this results in ~ 16 mt of catches being calculated
mrfss$ave_wgt_kg <- mrfss$WGT_AB1 / mrfss$TOT_CAT
mrfss$ave_wgt_mt <- 0.001 * mrfss$ave_wgt_kg
ave_wgt <- aggregate(ave_wgt_mt ~ year + area, mrfss, mean) 
ind <- which(is.na(mrfss$catch_mt))
for (i in ind){
	y <- mrfss[i, 'year']
	a <- mrfss[i, 'area']
	key <- which(ave_wgt$year == y & ave_wgt$area == a)
	mrfss$catch_mt[i] <- 
		mrfss$TOT_CAT[i] * ave_wgt[key, 'ave_wgt_mt']
}

# Remove inland records 19.7 mt, there is 0 catch in the ""
# mrfss <- mrfss[!mrfss$SOURCE_AREA_NAME %in% c("INLAND", ""), ]

# Add shoreside into the private fleet
mrfss[mrfss$mode == "shoreside", 'mode'] <- "private"

# Sum the catch by year, area, wave, and mode
mrfss_wave <- aggregate(catch_mt ~ year + area + wave + mode, mrfss, sum, drop = FALSE)
mrfss_wave[is.na(mrfss_wave)] <- 0

# Move catches in the south to the north for 1980 - 1989: 
# ratio of 0.317 based on Albin et al. 1993 
# See Allocate_MRFSS_1980-1989.xls to see how calculated.
factor <- 0.317
n <- which(mrfss_wave$year %in% c(1980:1989) & mrfss_wave$area == "north")
s <- which(mrfss_wave$year %in% c(1980:1989) & mrfss_wave$area == "south")
mrfss_wave$catch_mt[n] <- mrfss_wave[n, "catch_mt"] + mrfss_wave[s, "catch_mt"] * factor 
mrfss_wave$catch_mt[s] <- mrfss_wave[s, "catch_mt"] - mrfss_wave[s, "catch_mt"] * factor

#===============================================================================================
# Historical recreational catches 
#===============================================================================================
hist$cpfv <- hist$CPFV.tons
hist$private <- hist$skiff.shore.tons
hist[hist$area == "south_pt_concep", 'area'] <- 'south'
hist[hist$area == "north_pt_concep", 'area'] <- 'north'

hist_formatted <- data.frame(
	year = c(hist[hist$area == 'south', 'year'], hist[hist$area == 'south', 'year'],
		hist[hist$area == 'north', 'year'], hist[hist$area == 'north', 'year']),
	area = c(hist[hist$area == 'south', 'area'], hist[hist$area == 'south', 'area'],
		hist[hist$area == 'north', 'area'], hist[hist$area == 'north', 'area']),
	mode = c(rep('cpfv', sum(hist$area == 'south')), rep('private', sum(hist$area == 'south')),
		rep('cpfv', sum(hist$area == 'north')), rep('private', sum(hist$area == 'north'))),
	wave = 0, 
	catch_mt = c(hist[hist$area == 'south', 'cpfv'], hist[hist$area == 'south', 'private'],
		hist[hist$area == 'north', 'cpfv'], hist[hist$area == 'north', 'private'])
)

#===============================================================================================
# Combine everything together 
#===============================================================================================

all <- rbind(hist_formatted, mrfss_wave, crfs_wave)

landings_2021 <- aggregate(catch_mt ~ year + area, all, sum)
write.csv(landings_2021, file.path(dir, "forSS", "landings_in_2021_fleet_for_comparison.csv"))

#===============================================================================================
# Calculate the catch by wave or month to potentially use for composition expansion 
#===============================================================================================

# The data do not include revisions for specific years requested by CDFW (1981, 1990-1995)

mrfss_for_expansion <- aggregate(catch_mt ~ year + area + mode + wave, mrfss_wave, sum)
crfs_for_expansion <- aggregate(catch_mt ~ year + area + mode + month, crfs_month, sum)
mrfss_for_expansion$mode_wave <- paste0(mrfss_for_expansion$mode, "_", mrfss_for_expansion$wave)
crfs_for_expansion$mode_month <- paste0(crfs_for_expansion$mode, "_", crfs_for_expansion$month)

write.csv(mrfss_for_expansion[mrfss_for_expansion$area == "north", ],
	file = file.path(dir, "forSS", "north_mrfss_for_bds_expansion.csv"), row.names = FALSE)
write.csv(mrfss_for_expansion[mrfss_for_expansion$area == "south", ],
	file = file.path(dir, "forSS", "south_mrfss_for_bds_expansion.csv"), row.names = FALSE)
write.csv(crfs_for_expansion[crfs_for_expansion$area == "north", ],
	file = file.path(dir, "forSS", "north_crfs_for_bds_expansion.csv"), row.names = FALSE)
write.csv(crfs_for_expansion[crfs_for_expansion$area == "north", ],
	file = file.path(dir, "forSS", "south_crfs_for_bds_expansion.csv"), row.names = FALSE)

# =============================================================================================
# Format the catch for the model 
# =============================================================================================

mrfss_for_ss3 <- aggregate(catch_mt ~ year + area + mode, mrfss_wave, sum)
crfs_for_ss3  <- aggregate(catch_mt ~ year + area + mode, crfs_month, sum)

# =============================================================================================
# Fix the catch for specific years
# =============================================================================================

# North in 1993-1995 there are no cpfv removals. John Budrick proposed to fill these in by averaging
# the cpfv removals in 1987:1989 and 1996:1998 and ramping between these values. The other fleets
# (private-north, cpfv-south, private-south) can be averaged from removals between 1987:1989 and 
# 1993:1995.

average_early <- aggregate(catch_mt ~ mode + area, 
  mrfss_for_ss3[mrfss_for_ss3$year %in% 1987:1989,], mean)
average_late <- aggregate(catch_mt ~ mode + area, 
                                         mrfss_for_ss3[mrfss_for_ss3$year %in% 1993:1995,], mean)
mid_value <- (average_early$catch_mt + average_late$catch_mt) / 2

fill_values <- data.frame(
  year = c(rep(1990, 4), rep(1991, 4), rep(1992, 4)),
  mode = c(average_early$mode, average_early$mode, average_late$mode),
  area = c(average_early$area, average_early$area, average_late$area),
  catch_mt = c(average_early$catch_mt, mid_value, average_late$catch_mt)
)

# Fix the values in the north for the cpfv fleet
average_late <- mean(mrfss_for_ss3[mrfss_for_ss3$year %in% 1996:1998 & 
                     mrfss_for_ss3$area == "north" & mrfss_for_ss3$mode == "cpfv", "catch_mt"])

add <- c(fill_values[fill_values$year == 1990 & fill_values$area == "north" & 
  fill_values$mode == "cpfv", "catch_mt"] - average_late / rev(1:4), average_late)

fill_2004 <- 	as.data.frame(rbind(
  c(2004, 'north', 'cpfv', 6.529378),
  c(2004, 'north', 'private', 9.084398),
  c(2004, 'south', 'cpfv', 9.518152),
  c(2004, 'south', 'private', 4.171464)
))
colnames(fill_2004) <- c('year', 'area', 'mode', 'catch_mt')

# Adjust south catches for 1987 where there are no reported 
# removals in mrfss for waves 1-3 (first 6 months of the year)
wave_ave <- aggregate(catch_mt ~ mode, 
  mrfss[mrfss$area == 'south' & mrfss$year %in% c(1985:1986, 1988:1989) & mrfss$wave %in% 1:3, ], function(x) sum(x) /4 )
find <- which(mrfss_for_ss3$year == 1987 & mrfss_for_ss3$area == 'south')
mrfss_for_ss3[find[1], 'catch_mt'] <- 
  as.numeric(mrfss_for_ss3[find[1], 'catch_mt']) + wave_ave[wave_ave$mode == "cpfv", 'catch_mt']
mrfss_for_ss3[find[2], 'catch_mt'] <- 
  as.numeric(mrfss_for_ss3[find[2], 'catch_mt']) + wave_ave[wave_ave$mode == "private", 'catch_mt']

landings_to_add <- 
  as.data.frame(rbind(fill_values, fill_2004))

mrfss_for_ss3 <- rbind(
	mrfss_for_ss3, landings_to_add
)
mrfss_for_ss3 <- as.data.frame(mrfss_for_ss3)

# Now insert the cpfv catches to replace in the north for missing values - 
# Using a loop to make sure they get subbed into the correct year
a <- 1
for(y in 1991:1995){
  ind <- which(mrfss_for_ss3$year == y & mrfss_for_ss3$area == "north" & mrfss_for_ss3$mode == "cpfv")
  mrfss_for_ss3[ind, "catch_mt"] <- add[a]
  a <- a + 1
}
# Replace the private removals in the north for 1981 since they are so high
hist_year <- which(hist_formatted$year %in% 1979:1980 & hist_formatted$area == "north" & hist_formatted$mode == "private")
ave_years <- which(mrfss_for_ss3$year %in% 1982:1983 & mrfss_for_ss3$area == "north" & mrfss_for_ss3$mode == "private")
mrfss_for_ss3[mrfss_for_ss3$year == 1981 & mrfss_for_ss3$area == "north" & mrfss_for_ss3$mode == "private", "catch_mt"] <-
  mean(c(hist_formatted[hist_year, "catch_mt"], as.numeric(mrfss_for_ss3[ave_years, "catch_mt"])))    


all_for_model <- as.data.frame(rbind(hist_formatted[, -4], mrfss_for_ss3, crfs_for_ss3))

cpfv_n    <- which(all_for_model$mode == 'cpfv'    & all_for_model$area == 'north')
private_n <- which(all_for_model$mode == 'private' & all_for_model$area == 'north')
cpfv_s    <- which(all_for_model$mode == 'cpfv'    & all_for_model$area == 'south')
private_s <- which(all_for_model$mode == 'private' & all_for_model$area == 'south')

# year, season, fleet, catch, se
cpfv_north <- as.data.frame(cbind(
	all_for_model[cpfv_n, 'year'], 1, 3, round(as.numeric(all_for_model$catch_mt[cpfv_n]), 2), 0.01))
colnames(cpfv_north) <- c('year', 'seas', 'fleet', 'catch', 'se')
cpfv_north <- cpfv_north[sort(cpfv_north$year, decreasing = FALSE, index.return = TRUE)$ix, ]

private_north <- as.data.frame(cbind(
	all_for_model[private_n, 'year'], 1, 4, round(as.numeric(all_for_model$catch_mt[private_n]),2), 0.01))
colnames(private_north) <- c('year', 'seas', 'fleet', 'catch', 'se')
private_north <- private_north[sort(private_north$year, decreasing = FALSE, index.return = TRUE)$ix, ]

cpfv_south <- as.data.frame(cbind(
	all_for_model[cpfv_s, 'year'], 1, 3, round(as.numeric(all_for_model$catch_mt[cpfv_s]),2), 0.01))
colnames(cpfv_south) <- c('year', 'seas', 'fleet', 'catch', 'se')
cpfv_south <- cpfv_south[sort(cpfv_south$year, decreasing = FALSE, index.return = TRUE)$ix, ]

private_south <- as.data.frame(cbind(
	all_for_model[private_s, 'year'], 1, 4, round(as.numeric(all_for_model$catch_mt[private_s]),2), 0.01))
colnames(private_south) <- c('year', 'seas', 'fleet', 'catch', 'se')
private_south <- private_south[sort(private_south$year, decreasing = FALSE, index.return = TRUE)$ix, ]

write.csv(rbind(cpfv_south, private_south),
	file = file.path(dir, "forSS", "south_rec_landings_for_ss3.csv"), row.names = FALSE)

write.csv(rbind(cpfv_north, private_north),
	file = file.path(dir, "forSS", "north_rec_landings_for_ss3.csv"), row.names = FALSE)

# =============================================================================================
# Plot the data
# =============================================================================================
library(ggplot2)

all_for_model <- as.data.frame(all_for_model)
all_for_model$catch_mt <- as.numeric(all_for_model$catch_mt)
all_for_model$year <- as.numeric(all_for_model$year)

ggplot(all_for_model, aes(x = year, y = catch_mt, fill = mode)) +
  geom_bar(stat = 'identity') +
  facet_grid(area~.)+ 
  xlab("Year") + ylab("Catch (mt)") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) +
  scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(filename = file.path(dir, "plots", "all_rec_catch_mt_mode_2x1_fed.png"), 
       width = 13, height = 10, units = 'in')

mrfss$wave <- as.factor(mrfss$wave)
ggplot(mrfss, aes(x = year, y = catch_mt, fill = wave)) +
  geom_bar(position="stack", stat="identity") +
  facet_grid(area~.)+ 
  xlab("Year") + ylab("Catch (mt)") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text.y = element_text(size = 14)) +
  scale_fill_viridis_d()

