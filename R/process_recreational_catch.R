##############################################################################################################
#
# 	Data processing of recreational landings data for
#	  the 2023 copper rockfish assessment 
#			  by Chantel Wetzel 
#					
##############################################################################################################

dir <- "C:/Assessments/2023/copper_rockfish_2023/data/rec_catch"
dir.create(file.path(dir, "forSS"))

# Load the data items
load(file.path(dir, "crfss_catch_filtered.rdata"))
load(file.path(dir, "mrfss_catch_filtered.rdata")) 
hist <- read.csv(file.path(dir, "2021.spp.rec.n.and.s.conception.csv"))

# CRFS processing ===============================================
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

# MRFSS processing ==============================================
mrfss$orig_catch_mt <- 0.001 * mrfss$WGT_AB1
mrfss$catch_mt <- mrfss$orig_catch_mt
mrfss$wave <- mrfss$WAVE

# Attempt to fill in missing values
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

# Remove inland records
mrfss <- mrfss[!mrfss$SOURCE_AREA_NAME %in% c("INLAND", ""), ]

# Move catches in the south to the north for 1980 - 1989: 
# ratio of 0.317 based on Albin et al. 1993 
# See Allocate_MRFSS_1980-1989.xls to see how calculated.
mrfss_wave <- aggregate(catch_mt ~ year + area + mode + wave, mrfss, sum, drop = FALSE)
mrfss_wave[is.na(mrfss_wave)] <- 0
factor <- 0.317
n <- which(mrfss_wave$year %in% c(1980:1989) & mrfss_wave$area == "north")
s <- which(mrfss_wave$year %in% c(1980:1989) & mrfss_wave$area == "south")
mrfss_wave$catch_mt[n] <- mrfss_wave[n, "catch_mt"] + mrfss_wave[s, "catch_mt"] * factor 
mrfss_wave$catch_mt[s] <- mrfss_wave[s, "catch_mt"] - mrfss_wave[s, "catch_mt"] * factor

# Historical recreational catches ================================================
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

ggplot(hist_formatted, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) + 
	xlab("Year") + ylab("Landings (mt)") + 
	scale_fill_viridis_d(begin = 0, end = 0.5)
ggsave(filename = file.path(dir, "plots", "historical_rec_landings_mt_mode.png"), 
      width = 13, height = 10, units = 'in')

all <- rbind(hist_formatted, mrfss_wave, crfs_wave)

ggplot(all, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_wrap(facets = c("area")) + 
	xlab("Year") + ylab("Landings (mt)") + 
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "all_rec_landings_mt_mode_1x2.png"), 
      width = 13, height = 10, units = 'in')

ggplot(all, aes(x = year, y = catch_mt, fill = mode)) +
	geom_bar(stat = 'identity') +
	facet_grid(area~.)+ 
	xlab("Year") + ylab("Landings (mt)") + 
	theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
	scale_fill_viridis_d()
ggsave(filename = file.path(dir, "plots", "all_rec_landings_mt_mode_2x1.png"), 
      width = 13, height = 10, units = 'in')

total_by_mode <- aggregate(catch_mt ~ area + mode, all, sum)
#  area      mode     catch_mt
# north      cpfv 2396.0068392
# south      cpfv 1444.6931282
# north   private 4709.9102591
# south   private 1958.8411809
# north shoreside   19.9174686
# south shoreside    0.5121473

landings_2021 <- aggregate(catch_mt ~ year + area, all, sum)
write.csv(landings_2021, file.path(dir, "forSS", "landings_in_2021_fleet_for_comparison.csv"))

# What is the proportion of catch by each fleet and area
total_by_mode[total_by_mode$area == "north", 'catch_mt'] / 
	sum(total_by_mode[total_by_mode$area == "north", 'catch_mt'])
# North: cpfv = 34%, private = 66%, shoreside = 0.2%
total_by_mode[total_by_mode$area == "south", 'catch_mt'] / 
	sum(total_by_mode[total_by_mode$area == "south", 'catch_mt'])
# South: cpfv = 42%, private = 58%, shoreside = 0%

# Add shoreside into the private fleet
mrfss_wave[mrfss_wave$mode == "shoreside", 'mode'] <- "private"
crfs_month[crfs_month$mode == "shoreside", 'mode'] <- "private"

mrfss_for_expansion <- aggregate(catch_mt ~ year + area + mode + wave, mrfss_wave, sum)
crfs_for_expansion <- aggregate(catch_mt ~ year + area + mode + month, crfs_month, sum)
mrfss_for_expansion$mode_wave <- paste0(mrfss_for_expansion$mode, "_", mrfss_for_expansion$wave)
crfs_for_expansion$mode_month <- paste0(crfs_for_expansion$mode, "_", crfs_for_expansion$moth)

write.csv(mrfss_for_expansion[mrfss_for_expansion$area == "north", ],
	file = file.path(dir, "forSS", "north_mrfss_for_bds_expansion.csv"), row.names = FALSE)
write.csv(mrfss_for_expansion[mrfss_for_expansion$area == "south", ],
	file = file.path(dir, "forSS", "south_mrfss_for_bds_expansion.csv"), row.names = FALSE)
write.csv(crfs_for_expansion[crfs_for_expansion$area == "north", ],
	file = file.path(dir, "forSS", "north_crfs_for_bds_expansion.csv"), row.names = FALSE)
write.csv(crfs_for_expansion[crfs_for_expansion$area == "north", ],
	file = file.path(dir, "forSS", "south_crfs_for_bds_expansion.csv"), row.names = FALSE)

# ==============================================================
# Format the landings for the model 
# ==============================================================

mrfss_for_ss3 <- aggregate(catch_mt ~ year + area + mode, mrfss_wave, sum)
crfs_for_ss3  <- aggregate(catch_mt ~ year + area + mode, crfs_month, sum)

missing_years <- aggregate(catch_mt ~ mode + area, mrfss_for_ss3[mrfss_for_ss3$year %in% 1989:1993,], mean)
cpfv_n <- which(missing_years$mode == 'cpfv' & missing_years$area == 'north')
private_n <- which(missing_years$mode == 'private' & missing_years$area == 'north')
cpfv_s <- which(missing_years$mode == 'cpfv' & missing_years$area == 'south')
private_s <- which(missing_years$mode == 'private' & missing_years$area == 'south')

landings_to_add <- rbind(
	cbind(1990:1992, rep('north', 3), rep('cpfv', 3), missing_years[cpfv_n, 'catch_mt']),
	cbind(1990:1992, rep('north', 3), rep('private', 3), missing_years[private_n, 'catch_mt']),
	cbind(1990:1992, rep('south', 3), rep('cpfv', 3), missing_years[cpfv_s, 'catch_mt']),
	cbind(1990:1992, rep('south', 3), rep('private', 3), missing_years[private_s, 'catch_mt']),
	c(2004, 'north', 'cpfv', 0),
	c(2004, 'north', 'private', 0),
	c(2004, 'south', 'cpfv', 0),
	c(2004, 'south', 'private', 0)
)
colnames(landings_to_add) <- c('year', 'area', 'mode', 'catch_mt')
landings_to_add <- as.data.frame(landings_to_add)

mrfss_for_ss3 <- rbind(
	mrfss_for_ss3, landings_to_add
)

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
	all_for_model[private_n, 'year'], 1, 3, round(as.numeric(all_for_model$catch_mt[private_n]),2), 0.01))
colnames(private_north) <- c('year', 'seas', 'fleet', 'catch', 'se')
private_north <- private_north[sort(private_north$year, decreasing = FALSE, index.return = TRUE)$ix, ]

cpfv_south <- as.data.frame(cbind(
	all_for_model[cpfv_s, 'year'], 1, 3, round(as.numeric(all_for_model$catch_mt[cpfv_s]),2), 0.01))
colnames(cpfv_south) <- c('year', 'seas', 'fleet', 'catch', 'se')
cpfv_south <- cpfv_south[sort(cpfv_south$year, decreasing = FALSE, index.return = TRUE)$ix, ]

private_south <- as.data.frame(cbind(
	all_for_model[private_s, 'year'], 1, 3, round(as.numeric(all_for_model$catch_mt[private_s]),2), 0.01))
colnames(private_south) <- c('year', 'seas', 'fleet', 'catch', 'se')
private_south <- private_south[sort(private_south$year, decreasing = FALSE, index.return = TRUE)$ix, ]

write.csv(rbind(cpfv_south, private_south),
	file = file.path(dir, "forSS", "south_rec_landings_for_ss3.csv"), row.names = FALSE)

write.csv(rbind(cpfv_north, private_north),
	file = file.path(dir, "forSS", "north_rec_landings_for_ss3.csv"), row.names = FALSE)
