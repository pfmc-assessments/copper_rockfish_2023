# Sampling data simulation
# Analysis based on "sample bias cpfv duration_sca.xlsx"
# from Julia Coates.

# The percent of total single-day trips south of point conception
# 2001 - 2020: 91%
# 2001 - 2013: 97%
# 2014 - 2020: 81%
#
# The percentage of composition sampling for the same area are:
# 2012 - 2018: 98% (the % trips from these years are 85%)
# 2019: 86% (the % trips from this year are 80%)
library(viridis)
dir <- "C:/Assessments/2023/copper_rockfish_2023/data/rec_bds"

# Simulate the high percentage from recent years
# if there were 1000 total length samples with sampling proportions
# by length of trip:
one_day <- rnorm(810, 31.9, 6.1)
multi_day <- rnorm(190, 35.15, 5.6)

rep_sample <- c(one_day, multi_day)
bias_sample <- c(rnorm(980, 31.9, 6.1), rnorm(20, 35.15, 5.6))

# Create a df of the samples
df <- data.frame(
	length = c(one_day, multi_day),
	trip_type = c(rep("single-day", length(one_day)),
				  rep( "multi-day", length(multi_day)))
)

df2 <- data.frame(
	length = c(rep_sample, bias_sample),
	sample_type = c(rep("unbiased", length(rep_sample)),
				  rep( "biased", length(bias_sample)))
)

aggregate(length~trip_type, df, function(x) round(quantile(x, c(0, 0.10, 0.25, 0.5, 0.75, 0.90, 1)), 1))
#  trip_type length.0% length.10% length.25% length.50% length.75% length.90% length.100%
#  multi-day      19.5       29.0       31.3       34.7       38.8       42.0        48.9
# single-day      15.7       24.6       27.8       31.8       36.0       40.0        52.6
#   combined      15.7       25.0       28.5       32.7       36.6       40.6        52.6 

ggplot(df, aes(length, fill = trip_type, color = trip_type)) +
	geom_histogram(binwidth = 1, boundary = 0, position="stack") +
	scale_fill_viridis_d() +
	scale_x_continuous(name = "Length (cm)") +
	scale_y_continuous(name = "Count") +
	geom_vline(xintercept = mean(df[df$trip_type == "single-day", "length"]), 
		color = 'green', lwd = 2) + 
	geom_vline(xintercept = mean(df[df$trip_type == "multi-day", "length"]), 
		color = 'darkblue', lwd = 2) + 
	geom_vline(xintercept = mean(df[, "length"]), 
		color = 'black', lwd = 2) 

ggplot(df, aes(length, fill = trip_type, color = trip_type)) +
  geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) +
  scale_fill_viridis_d() +
  scale_x_discrete(name = "Length (cm)")

aggregate(length~sample_type, df2, function(x) round(quantile(x, c(0, 0.10, 0.25, 0.5, 0.75, 0.90, 1)), 1))
# sample_type length.0% length.10% length.25% length.50% length.75% length.90% length.100%
#      biased      13.9       24.5       28.1       32.2       36.3       40.0        49.6
#    unbiased      15.7       25.0       28.5       32.7       36.6       40.6        52.6
color <- viridis::viridis(2)

ggplot(df2, aes(length, fill = sample_type)) +
  geom_density(alpha = 0.5, lwd = 0.8, color = NA) +
  scale_fill_viridis_d() +
  geom_vline(xintercept = quantile(df2[df2$sample_type == "biased", "length"], c(0.1, 0.5, 0.9)),
	col = color[1], lwd = 2, lty = 2) + 
  geom_vline(xintercept = quantile(df2[df2$sample_type == "unbiased", "length"], c(0.1, 0.5, 0.9)),
	col = color[2], lwd = 2, lty = 2)  

ggsave(filename = file.path(dir, 'plots', "cpfv_trip_sampling_bias_unbiased.png"), 
      width = 13, height = 10, units = 'in')
