##############################################################################################################
#
# 	Data explorations & processing of GEMM discard data for
#	  		the 2023 copper rockfish assessment 
#			  		by Chantel Wetzel 
#					
##############################################################################################################

library(here)

dir = getwd()
gemm_dir <- file.path(dir, "data", "gemm")

load(file.path(gemm_dir, "gemm_Copper_rockfish.rdata"))

# Filter out part of the data
# ===================================================

remove <- grep("Recreational", gemm$sector, ignore.case = TRUE)
rec_data <- gemm[remove, ]
gemm <- gemm[-remove, ]

# GEMM data is split north and south of 4010 for commercial
# data.
remove <- grep("North", gemm$grouping, ignore.case = TRUE)
gemm <- gemm[-remove, ]

# Rename columns for simplicity
# ===================================================
gemm$landings <- gemm$total_landings_mt
gemm$catch <- gemm$total_discard_with_mort_rates_applied_and_landings_mt
gemm$discard <- gemm$total_discard_with_mort_rates_applied_mt
gemm$rate <- gemm$discard / (gemm$discard + gemm$landings)

# Look for overall shifts in discarding
# ===================================================
discard_by_year <- aggregate(discard ~ year, gemm, sum)
landings_by_year <- aggregate(landings ~ year, gemm, sum)
cbind(discard_by_year$year, discard_by_year[,2] / (discard_by_year[,2] + landings_by_year[,2]) )
# Last assessments only used 2002-2018 because there were regulation
# changes in 2019 but did not note which changes.


# Look at the discard rates by year and sector
# ===================================================
discard_sector <- aggregate(discard ~ year + sector, gemm, sum)
discard_sector[discard_sector$year %in% 2020:2021, ]
sum(discard_sector$discard)
# 8.858247 mt summed across all sectors and years

all_landings <- aggregate(landings ~ year + sector, gemm, sum)
sum(all_landings$landings)
# 128.4075 mt summed across all sectors and years

sum(discard_sector$discard) / (sum(discard_sector$discard) + sum(all_landings$landings))
# 0.065

sum(discard_sector[discard_sector$year < 2019, 'discard']) / 
	(sum(discard_sector[discard_sector$year < 2019, 'discard']) + 
		sum(all_landings[all_landings$year < 2019, 'landings']))
# 0.061


# Let's only look at some key sectors
# ====================================================================
keep <- which(gemm$sector %in% 
	c("LE Fixed Gear DTL - Hook & Line", "Nearshore", "CS - Hook & Line",
	  " OA Fixed Gear - Hook & Line", "OA Fixed Gear - Pot", "LE Fixed Gear DTL - Pot"))

sub_gemm <- gemm[keep, ]

all_discard <- aggregate(discard ~ year, sub_gemm, sum, drop = FALSE)
sum(all_discard$discard)
# 6.916137 mt summed across all sectors and years

all_landings <- aggregate(landings ~ year, sub_gemm, sum, drop = FALSE)
sum(all_landings$landings)
# 124.6796 mt summeda across all sectors and years

sum(all_discard$discard) / (sum(all_discard$discard) + sum(all_landings$landings))
# 0.05255594

rate_by_year <- cbind(all_discard$year, all_discard$discard / (all_discard$discard + all_landings$landings))
colnames(rate_by_year) <- c("year", "rate")
mean(rate_by_year[, 'rate'])
# all years = 0.051
rate_by_year[,'rate'] <- round(rate_by_year[,'rate'],2)

mean(rate_by_year[rate_by_year[,"year"] < 2019, 'rate'])
# 0.057

median(rate_by_year[rate_by_year[,"year"] < 2019, 'rate'])
# 0.027

gemm_data <- cbind(rate_by_year, all_landings$landings, all_discard$discard)
colnames(gemm_data) <- c("year", "rate", "landings", "discard")
save(gemm_data, file = file.path(dir, "gemm_processed_south_4010.Rdata"))

# Let's look at the average and variance by year
# =============================================================
table(gemm$year)
ave_discard_rate_all <- aggregate(rate ~ year, gemm, mean)
sd_discard_rate_all <- aggregate(rate ~ year, gemm, sd)
sd_high <- ave_discard_rate_all$rate + 1.96 * sd_discard_rate_all$rate
sd_low <- ave_discard_rate_all$rate - 1.96 * sd_discard_rate_all$rate
all_data <- as.data.frame(cbind(ave_discard_rate_all, sd_low, sd_high))
all_data[all_data < 0 ] <- 0

table(sub_gemm$year)
sub_ave_discard_rate <- aggregate(rate ~ year, sub_gemm, mean)



cex.lab = 1.1; main_name = "All Sectors"
plot(0, type = "n",
      xlim = range(all_data$year),
      ylim = c(0, 1),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = 1.2)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Discard Rate", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = main_name,
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = "",
    font = 2, cex = cex.lab, line = -1.75)
  graphics::arrows(x0 = all_data$year, y0 = all_data$sd_low, x1 = all_data$year, y1 = all_data$sd_high, 
    angle = 90, code = 3, length = 0.01, col = "darkgrey")
  graphics::points(all_data$year, all_data$rate, pch = 17, bg = 1, cex = 1.6)
  graphics::lines( all_data$year,  all_data$rate, col = 1, cex = 1, lwd = 2)


sub_data <- sub_gemm
  cex.lab = 1.5
  plot(0, type = "n",
      xlim = range(sub_data$year),
      ylim = c(0, 1),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = 1.2)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Discard Rate", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = "Select Sectors",
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = "",
    font = 2, cex = cex.lab, line = -1.75)
  graphics::points(sub_data$year, sub_data$rate, pch = 17, bg = 1, cex = 1.6)
  graphics::lines( sub_data$year, sub_data$rate, col = 1, cex = 1, lwd = 2)