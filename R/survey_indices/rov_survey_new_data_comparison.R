# ROV Data Exploration
# May 20, 2023

library(here)
library(ggplot2)
library(cowplot)

dir <- here("data", "survey_indices", "rov")

load(here("data", "survey_indices", "rov", "rov_south_data_used_for_index_creation.rdata"))
old <- rov_south
  
load(here("data", "survey_indices", "rov", "rov_south_data_used_for_index_creation_05192023.rdata"))
new <- rov_south

# Put everything in a single df for ease of grid plotting later
data <- data.frame(
  line = c(new$FirstOfLineID, old$LineID),
  year = c(new$year, old$year),
  usable_area = c(new$usable_area, old$usable_area),
  depth = c(new$depth, old$depth),
  prop_soft = c(new$prop_soft, old$prop_soft),
  prop_hard = c(new$prop_hard, old$prop_hard),
  designation = c(new$designation, old$designation),
  mpa_group = c(new$mpa_group, old$mpa_group),
  n = c(new$n, old$n),
  source = c(rep("Corrected Data", nrow(new)), rep("Old Data", nrow(old)))
)

ind <- sort(old$LineID,index.return = TRUE)$ix
tmp_old <- old[ind, ]
ind <- sort(new$FirstOfLineID,index.return = TRUE)$ix
tmp_new <- new[ind, ]

HandyCode::pngfun(wd = file.path(dir, "plots"), "south_prop_soft_data_comparision.png", w = 18, h = 12)
par(mfrow = c(2,2))
plot(tmp_old$prop_soft[tmp_old$year == 2015], tmp_new$prop_soft[tmp_new$year == 2015], xlab = "Old Data", ylab = "Corrected Data", main = paste("Prop. Soft", 2015))
plot(tmp_old$prop_soft[tmp_old$year == 2019], tmp_new$prop_soft[tmp_new$year == 2019], xlab = "Old Data", ylab = "Corrected Data", main = paste("Prop. Soft", 2019))
plot(tmp_old$prop_soft[tmp_old$year == 2020], tmp_new$prop_soft[tmp_new$year == 2020], xlab = "Old Data", ylab = "Corrected Data", main = paste("Prop. Soft", 2020))
plot(tmp_old$prop_soft[tmp_old$year == 2021], tmp_new$prop_soft[tmp_new$year == 2021], xlab = "Old Data", ylab = "Corrected Data", main = paste("Prop. Soft", 2021))
dev.off()

HandyCode::pngfun(wd = file.path(dir, "plots"), "south_depth_data_comparision.png", w = 18, h = 12)
par(mfrow = c(2,2))
plot(tmp_old$depth[tmp_old$year == 2015], tmp_new$depth[tmp_new$year == 2015], xlab = "Old Data", ylab = "Corrected Data", main = paste("Depth", 2015))
plot(tmp_old$depth[tmp_old$year == 2019], tmp_new$depth[tmp_new$year == 2019], xlab = "Old Data", ylab = "Corrected Data", main = paste("Depth", 2019))
plot(tmp_old$depth[tmp_old$year == 2020], tmp_new$depth[tmp_new$year == 2020], xlab = "Old Data", ylab = "Corrected Data", main = paste("Depth", 2020))
plot(tmp_old$depth[tmp_old$year == 2021], tmp_new$depth[tmp_new$year == 2021], xlab = "Old Data", ylab = "Corrected Data", main = paste("Depth", 2021))
dev.off()

HandyCode::pngfun(wd = file.path(dir, "plots"), "south_usable_area_data_comparision.png", w = 18, h = 12)
par(mfrow = c(2,2))
plot(tmp_old$usable_area[tmp_old$year == 2015], tmp_new$usable_area[tmp_new$year == 2015],  xlab = "Old Data", ylab = "Corrected Data", main = paste("Usable Area", 2015))
plot(tmp_old$usable_area[tmp_old$year == 2019], tmp_new$usable_area[tmp_new$year == 2019],  xlab = "Old Data", ylab = "Corrected Data", main = paste("Usable Area", 2015))
plot(tmp_old$usable_area[tmp_old$year == 2020], tmp_new$usable_area[tmp_new$year == 2020],  xlab = "Old Data", ylab = "Corrected Data", main = paste("Usable Area", 2015))
plot(tmp_old$usable_area[tmp_old$year == 2021], tmp_new$usable_area[tmp_new$year == 2021],  xlab = "Old Data", ylab = "Corrected Data", main = paste("Usable Area", 2015))
dev.off()

# filter down to only 2019 data to explore this further
#old <- old[old$year == 2019, ]
#new <- new[new$year == 2019, ]


ggplot(data = data, aes(x = prop_soft)) +
  geom_histogram(position = "stack", col = 'black', fill = 'blue') +
  facet_grid(c("year", "source")) 

ggplot(data = data, aes(x = prop_hard)) +
  geom_histogram(position = "stack", col = 'black', fill = 'green') +
  facet_grid(c("year", "source")) 

ggplot(data = data, aes(x = usable_area)) +
  geom_histogram(position = "stack", col = 'black', fill = 'green') +
  facet_grid(c("year", "source")) 

ggplot(data = data, aes(x = n)) +
  geom_histogram(position = "stack", col = 'black', fill = 'green') +
  facet_grid(c("year", "source")) 

ggplot(data = data, aes(x = designation)) +
  geom_histogram(position = "stack", stat = "count", col = 'black', fill = 'green') +
  facet_grid(c("year", "source")) 

ggplot(data = data, aes(x = mpa_group)) +
  geom_histogram(position = "stack", stat = "count", col = 'black', fill = 'green') +
  facet_grid(c("year", "source")) 


ggplot(data = data, aes(x = depth)) +
  geom_histogram(position = "stack", col = 'black', fill = 'blue') +
  facet_grid(c("year", "source")) 


p1 <- ggplot(data = new, aes(x = usable_area)) +
  geom_histogram(position = "stack", col = 'black', fill = 'blue') +
  facet_grid("year") + 
  ylim(c(0, 35))

p2 <- ggplot(data = old, aes(x = usable_area)) +
  geom_histogram(position = "stack", col = 'black', fill = 'green') +
  facet_grid("year") + 
  ylim(c(0, 35))

ggplot(data = old, aes(x = prop_soft), alpha = 0.2, col = 'green') +
  geom_histogram(position = "stack") +
  geom_histogram(data = new, aes(x = prop_soft), col = 'blue', alpha = 0.2) +
  facet_wrap("year")

ggplot(data = old, aes(x = prop_soft)) +
  geom_density(col = 'yellow', size = 1) +
  geom_density(data = new, aes(x = prop_soft), col = 'blue', alpha = 0.2, size = 1) +
  facet_wrap("year")

ggplot(data = old, aes(x = depth)) +
  geom_density(col = 'yellow', size = 1, alpha = 2) +
  geom_density(data = new, aes(x = depth), col = 'blue', alpha = 0.2, size = 1) +
  facet_wrap("year")

ggplot(data = old, aes(x = usable_area)) +
  geom_density(col = 'yellow', size = 1, alpha = 2) +
  geom_density(data = new, aes(x = usable_area), col = 'blue', alpha = 0.2) +
  facet_wrap("year")

ggplot(data = old, aes(x = n)) +
  geom_histogram(position = "stack", alpha = 0.2, fill = 'green', col = 'black') +
  geom_histogram(data = new, aes(x = n), fill = 'blue', col = 'purple', alpha = 0.2) +
  facet_wrap("year")

table(old$year, old$designation)
table(new$year, new$designation)

#================================================================================
# North data
#===============================================================================
load(here("data", "survey_indices", "rov", "rov_north_data_used_for_index_creation.rdata"))
old <- rov_north

load(here("data", "survey_indices", "rov", "rov_north_data_used_for_index_creation_05192023.rdata"))
new <- rov_north

# Put everything in a single df for ease of grid plotting later
data <- data.frame(
  line = c(new$FirstOfLineID, old$LineID),
  year = c(new$year, old$year),
  usable_area = c(new$usable_area, old$usable_area),
  depth = c(new$depth, old$depth),
  prop_soft = c(new$prop_soft, old$prop_soft),
  prop_hard = c(new$prop_hard, old$prop_hard),
  designation = c(new$designation, old$designation),
  mpa_group = c(new$mpa_group, old$mpa_group),
  n = c(new$n, old$n),
  source = c(rep("Corrected Data", nrow(new)), rep("Old Data", nrow(old)))
)

# filter down to the same transects for comparisons
keep <- unique(old$LineID)
new_subset <- new[new$FirstOfLineID %in% keep, ]

ind <- sort(old$LineID,index.return = TRUE)$ix
tmp_old <- old[ind, ]
ind <- sort(new_subset$FirstOfLineID,index.return = TRUE)$ix
tmp_new <- new_subset[ind, ]

par(mfrow = c(2,2))
plot(tmp_old$prop_soft[tmp_old$year == 2014], tmp_new$prop_soft[tmp_new$year == 2014], xlab = "Old Data", ylab = "Corrected Data", main = 2015)
plot(tmp_old$prop_soft[tmp_old$year == 2015], tmp_new$prop_soft[tmp_new$year == 2015], xlab = "Old Data", ylab = "Corrected Data", main = 2015)
#plot(tmp_old$prop_soft[tmp_old$year == 2016], tmp_new$prop_soft[tmp_new$year == 2016], xlab = "Old Data", ylab = "Corrected Data", main = 2015)
plot(tmp_old$prop_soft[tmp_old$year == 2019], tmp_new$prop_soft[tmp_new$year == 2019], xlab = "Old Data", ylab = "Corrected Data", main = 2019)
#plot(tmp_old$prop_soft[tmp_old$year == 2020], tmp_new$prop_soft[tmp_new$year == 2020], xlab = "Old Data", ylab = "Corrected Data", main = 2020)
#plot(tmp_old$prop_soft[tmp_old$year == 2021], tmp_new$prop_soft[tmp_new$year == 2021], xlab = "Old Data", ylab = "Corrected Data", main = 2021)

par(mfrow = c(2,2))
plot(tmp_old$depth[tmp_old$year == 2014], tmp_new$depth[tmp_new$year == 2014])
plot(tmp_old$depth[tmp_old$year == 2015], tmp_new$depth[tmp_new$year == 2015])
plot(tmp_old$depth[tmp_old$year == 2019], tmp_new$depth[tmp_new$year == 2019])
#plot(tmp_old$depth[tmp_old$year == 2020], tmp_new$depth[tmp_new$year == 2020])
#plot(tmp_old$depth[tmp_old$year == 2021], tmp_new$depth[tmp_new$year == 2021])



par(mfrow = c(2,2))
plot(tmp_old$usable_area[tmp_old$year == 2015], tmp_new$usable_area[tmp_new$year == 2015])
plot(tmp_old$usable_area[tmp_old$year == 2019], tmp_new$usable_area[tmp_new$year == 2019])
plot(tmp_old$usable_area[tmp_old$year == 2020], tmp_new$usable_area[tmp_new$year == 2020])
plot(tmp_old$usable_area[tmp_old$year == 2021], tmp_new$usable_area[tmp_new$year == 2021])


#===============================================================================
# Plot Indices Comparisons
#===============================================================================
area <- "north"
#load(here("data", "survey_indices", "rov", "delta_gamma_south_designation_depth_year_soft_73_27", "index.rdata"))
load(here("data", "survey_indices", "rov", "glm_negbin_north_designation_depth", "index.rdata"))
old_index <- index
#load(here("data", "survey_indices", "rov", "delta_gamma_south_designation_depth_year_soft_73_27_05192023", "index.rdata"))
load(here("data", "survey_indices", "rov", "glm_negbin_north_designation_depth_80_20_05192023", "index.rdata"))
data <- index

years =   as.numeric(as.character(data$year))
sdmtmb_est <- data[,'est']
hi_sdmtmb <- data[, "upr"]
lo_sdmtmb <- data[, "lwr"]

sdmtmb_est_old <- old_index[,'est']
hi_sdmtmb_old  <- old_index[, "upr"]
lo_sdmtmb_old  <- old_index[, "lwr"]


out_file = here("data", "survey_indices", "rov", "plots", paste0(area, "_index_comparison.png"))
grDevices::png(filename = out_file,
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)

cex.axis = 1.25
cex.lab = 1.20
ymax = max(hi_sdmtmb) + 0.10 * max(hi_sdmtmb)
x <- 0.04

plot(0, type = "n",
     xlim = range(years),
     ylim = c(0, ymax),
     xlab = "", ylab = "", yaxs = "i",
     main = "", cex.axis = cex.axis)

graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
graphics::mtext(side = 2, "Relative Index", cex = cex.lab, line = 2.5)

graphics::arrows(x0 = years + x, y0 = lo_sdmtmb, x1 = years + x, y1 = hi_sdmtmb, 
                 angle = 90, code = 3, length = 0.01, col = "blue",
                 lty = 2)
graphics::points(years + x, sdmtmb_est, pch = 16, bg = 1, cex = 1.6, col = 'blue')
graphics::lines(years + x,  sdmtmb_est, cex = 1, col = 'blue', lty = 2)

graphics::arrows(x0 = years + 0.1, y0 = lo_sdmtmb_old, x1 = years + 0.1, y1 = hi_sdmtmb_old, 
                 angle = 90, code = 3, length = 0.01, col = "green",
                 lty = 2)
graphics::points(years + 0.1, sdmtmb_est_old, pch = 16, bg = 1, cex = 1.6, col = 'green')
graphics::lines(years + 0.1,  sdmtmb_est_old, cex = 1, col = 'green', lty = 2)
legend("topleft", bty = 'n', col = c("blue", "green"), lty = 2, pch = 16,
       legend = c("New Index", "Old Index"))

dev.off()

years =   as.numeric(as.character(data$year))
sdmtmb_est <- data[,'est'] / mean(data[,'est'])
hi_sdmtmb <- data[, "upr"] / mean(data[, "upr"])
lo_sdmtmb <- data[, "lwr"] / mean(data[, "lwr"])

sdmtmb_est_old <- old_index[,'est'] / mean( old_index[,'est'])
hi_sdmtmb_old  <- old_index[, "upr"]/ mean( old_index[,'upr'])
lo_sdmtmb_old  <- old_index[, "lwr"]/ mean( old_index[,'lwr'])


out_file = here("data", "survey_indices", "rov", "plots", paste0(area, "_index_comparison_standardized.png"))
grDevices::png(filename = out_file,
               width = 10, height = 7, units = "in", res = 300, pointsize = 12)

cex.axis = 1.25
cex.lab = 1.20
ymax = max(hi_sdmtmb) + 0.25 * max(hi_sdmtmb)
x <- 0.04

plot(0, type = "n",
     xlim = range(years),
     ylim = c(0, ymax),
     xlab = "", ylab = "", yaxs = "i",
     main = "", cex.axis = cex.axis)

graphics::mtext(side = 1, "Year", cex = cex.lab, line = 3)
graphics::mtext(side = 2, "Relative Index", cex = cex.lab, line = 2.5)

graphics::arrows(x0 = years + x, y0 = lo_sdmtmb, x1 = years + x, y1 = hi_sdmtmb, 
                 angle = 90, code = 3, length = 0.01, col = "blue",
                 lty = 2)
graphics::points(years + x, sdmtmb_est, pch = 16, bg = 1, cex = 1.6, col = 'blue')
graphics::lines(years + x,  sdmtmb_est, cex = 1, col = 'blue', lty = 2)

graphics::arrows(x0 = years + 0.1, y0 = lo_sdmtmb_old, x1 = years + 0.1, y1 = hi_sdmtmb_old, 
                 angle = 90, code = 3, length = 0.01, col = "green",
                 lty = 2)
graphics::points(years + 0.1, sdmtmb_est_old, pch = 1, bg = 1, cex = 1.6, col = 'green')
graphics::lines(years + 0.1,  sdmtmb_est_old, cex = 1, col = 'green', lty = 2)

legend("topleft", bty = 'n', col = c("blue", "green"), lty = 2, pch = c(16, 1),
       legend = c("New Index", "Old Index"))

dev.off()
