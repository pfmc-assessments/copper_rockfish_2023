# ROV Data Exploration
# May 20, 2023

library(here)
library(ggplot2)

load(here("data", "survey_indices", "rov", "rov_south_data_used_for_index_creation.rdata"))
old <- rov_south 
  
load(here("data", "survey_indices", "rov", "rov_south_data_used_for_index_creation_05192023.rdata"))
new <- rov_south

ggplot(data = new, aes(x = prop_soft)) +
  geom_histogram(position = "stack") +
  facet_wrap("year")

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
