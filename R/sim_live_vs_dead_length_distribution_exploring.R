# Sampling data simulation to examine the impact
# of the commercial fishery landings of live vs.
# dead fish across time and by area. Fish landed 
# live are often of a target size that may vary 
# from the average size of fish landed dead.
#
# The proportion of landing between live and dead fish
# varies north and south for copper rockfish. Based on the 
# available data in PacFIN, since 1993/94 there is a higher
# proportion of fish landed live in the north and in recent 
# years the proportion of fish landed live in the north has
# continued to increase
# North (1993-2022) Averge % Landed Dead = 41% (59% live)
# South (1994-2022) Averge % Landed Dead = 53% (47% live)
# North (2010-2022) Averge % Landed Dead = 29% (71% live)
# South (2010-2022) Averge % Landed Dead = 43% (57% live)
#
# The percent of length samples by landed condition type in
# PacFIN does not align with the landings proportions due to 
# the difficulties of obtaining live fish length measurements.
# The percent of length measurements coming from dead fish 
# across all years (1994-2021) in the north are 67% (where 41%
# of landings are from dead fish) and in the south are 36% 
# (where 53% landed dead). Since 2010 the percent of length 
# samples from the north from dead fish are 63% (29% landed
# dead) and in the south of 54% (43% landed dead). 
# North (1994-2021) Ave. Length of Dead = 40.8 (sd = 5.8) n = 84
# North (1994-2021) Ave. Length of Live = 35.2 (sd = 4.9) n = 42
# South (1994-2021) Ave. Length of Dead = 39.9 (sd = 5.6) n = 46
# South (1994-2021) Ave. Length of Live = 36.1 (sd = 4.2) n = 81
# North (2010-2021) Ave. Length of Dead = 40.6 (sd = 7.7) n = 38
# North (2010-2021) Ave. Length of Live = 33.4 (sd = 4.2) n = 22
# South (2010-2021) Ave. Length of Dead = 36.2 (sd = 4.1) n = 54
# South (2010-2021) Ave. Length of Live = 34.1 (sd = 5.3) n = 63

# For each area and for each period of time, the difference between
# the size of fish landed live vs. dead is greater than 2 cm which
# would result in difference in the composition data between lumping
# or splitting the data.

library(viridis)
library(ggplot2)
dir <- "C:/Assessments/2023/copper_rockfish_2023/data/pacfin_bds"

# Set up some scenarios with the most inefficient approach possible.
# Start with the scenario that 67% of lengths are coming from live
# fish based on the north average across all year
mult <- 1
dead_all_n <- rnorm(67 * mult, 40.8, 5.8)
live_all_n <- rnorm(33 * mult, 35.2, 4.9)
# Sampling based on the north landings %
dead_all_n_true <- rnorm(41 * mult, 40.8, 5.8)
live_all_n_true <- rnorm(59 * mult, 35.2, 4.9)
# Sampling based on the 2010+ north averages
dead_recent_n <- rnorm(67 * mult, 40.6, 7.7)
live_recent_n <- rnorm(33 * mult, 33.4, 4.2)
# Sampling based on the 2010+ north landing %
dead_recent_n_true <- rnorm(29 * mult, 40.6, 7.7)
live_recent_n_true <- rnorm(71 * mult, 33.4, 4.2)
# Sampling based on the all years south averages
dead_all_s <- rnorm(36 * mult, 39.9, 5.6)
live_all_s <- rnorm(73 * mult, 36.1, 4.2)
# Sampling based on the all years south averages
dead_all_s_true <- rnorm(53 * mult, 39.9, 5.6)
live_all_s_true <- rnorm(74 * mult, 36.1, 4.2)
# Sampling based on the 2010+ south averages
dead_recent_s <- rnorm(54 * mult, 36.2, 4.1)
live_recent_s <- rnorm(46 * mult, 34.1, 5.3)
# Sampling based on the 2010+ north landing %
dead_recent_s_true <- rnorm(43 * mult, 36.2, 4.1)
live_recent_s_true <- rnorm(57 * mult, 34.1, 5.3)

# Create a df of the samples
df <- data.frame(
    length = c(dead_all_n, live_all_n, dead_all_n_true, live_all_n_true,
        dead_recent_n, live_recent_n, dead_recent_n_true, live_recent_n_true,
        dead_all_s, live_all_n, dead_all_s_true, live_all_s_true,
        dead_recent_s, live_recent_s, dead_recent_s_true, live_recent_s_true),
    condition = c(rep("dead", length(dead_all_n)), rep("live", length(live_all_n)), 
        rep("dead", length(dead_all_n_true)), rep("live", length(live_all_n_true)),
        rep("dead", length(dead_recent_n)), rep("live", length(live_recent_n)), 
        rep("dead", length(dead_recent_n_true)), rep("live", length(live_recent_n_true)),
        rep("dead", length(dead_all_s)), rep("live", length(live_all_n)), 
        rep("dead", length(dead_all_s_true)), rep("live", length(live_all_s_true)),
        rep("dead", length(dead_recent_s)), rep("live", length(live_recent_s)), 
        rep("dead", length(dead_recent_s_true)), rep("live",length(live_recent_s_true))),
    scenario = c(rep("north_all_years", sum(length(dead_all_n) + length(live_all_n))), 
        rep("north_all_years_perfect_sampling", sum(length(dead_all_n_true) + length(live_all_n_true))),
        rep("north_2010_2021", sum(length(dead_recent_n) + length(live_recent_n))), 
        rep("north_2010_2021_perfect_sampling", sum(length(dead_recent_n_true) + length(live_recent_n_true))),
        rep("south_all_years", sum(length(dead_all_s) + length(live_all_n))), 
        rep("south_all_years_perfect_sampling", sum(length(dead_all_s_true) + length(live_all_s_true))),
        rep("south_2010_2021", sum(length(dead_recent_s) + length(live_recent_s))), 
        rep("south_2010_2021_perfect_sampling", sum(length(dead_recent_s_true) + length(live_recent_s_true))))
)

means <- aggregate(length~scenario + condition, df, mean)
comb_means <- aggregate(length~scenario, df, mean)

ggplot(df, aes(length, fill = condition, color = condition)) +
    geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) +
    scale_fill_viridis_d() +
    geom_vline(data = means, aes(xintercept = length, colour = condition), lwd = 2) +
    geom_vline(data = comb_means, aes(xintercept = length), lty = 2, lwd = 1) + 
    facet_wrap("scenario", ncol = 2) + 
    scale_x_continuous(name = "Length (cm)") +
    scale_y_continuous(name = "Density")
ggsave(filename = file.path(dir, 'plots', "live_dead_length_dist_by_area_time.png"), 
      width = 13, height = 10, units = 'in') 