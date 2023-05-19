###############################################################################
# title: Age Data Comparsion for Copper Rockfish
# author: Chantel Wetzel (adapted from code by Kelli Johnson)
###############################################################################

library(here)
library(ggplot2)
library(grid)
library(dplyr)

# Load in age data from all sources
dir <- here("data", "ages")

load(here("data", "ages", "formatted_age_files", "abrams_ages.rdata"))
load(here("data", "ages", "formatted_age_files", "cdfw_ages.rdata"))
load(here("data", "ages", "formatted_age_files", "coop_ages.rdata"))
load(here("data", "ages", "formatted_age_files", "pearson_ages.rdata"))
load(here("data", "pacfin_bds", "commercial_ages.rdata"))
load(here("data", "rec_bds", "crfs_ages.rdata"))
load(here("data", "rec_bds", "historical_rec_ages.rdata"))
load(here("data", "wcgbt",  "wcgbt_ages_with_area.rdata"))
load(here("data", "survey_indices", "nwfsc_hkl", "nwfsc_hkl_2004-2022.rdata"))
load(here("data", "survey_indices", "ccfrp", "ccfrp_ages.rdata"))

hkl$program <- "NWFSC HKL"
hkl$area <- "south"
hkl <- hkl[hkl$common_name == "Copper Rockfish", ]
bio_orig$program <- "NWFSC WCGBT"
colnames(bio_orig) <- tolower(colnames(bio_orig))

col_names <- c('program', 'year', 'area', 'sex', 'length_cm', 'age')
all_ages <- rbind(
  ccfrp_ages[, col_names],
  hkl[, col_names], 
  bio_orig[, col_names],
  abrams_ages[, col_names],
  pearson_ages[, col_names],
  cdfw_ages[, col_names],
  crfs_ages[, col_names],
  coop_ages[, col_names],
  commercial_ages[, col_names],
  hist_rec_ages[, col_names]
)

all_ages <- all_ages[!is.na(all_ages$age), ]

all_ages$program[all_ages$program == "CPOP"] <- "CCFRP"
all_ages$program[all_ages$program %in% c("Commercial - EFI", "Commercial - Pilot Sampling", "Whole")] <- "Research Ages"
all_ages$program[all_ages$program == "abrams"] <- "Research Ages"
all_ages$program[all_ages$program == "Pearson Research"] <- "Research Ages"
all_ages$program[all_ages$program == "commercial"] <- "Commercial"
all_ages$program[all_ages$program %in% c("CRFS", "recreational")] <- "Recreational"

# Use this line for the fishery age data plots
#remove <- which(all_ages$area == "south" & all_ages$program == "Research Ages")
#all_ages <- all_ages[-remove, ]

save(all_ages, file = file.path(dir, "formatted_age_files", "all_copper_ages_05162023.rdata"))

samples_by_area <- all_ages %>%
  group_by(area, program) %>%
  reframe(
    n = length(age)
  )

quantile_by_area <- all_ages %>%
  group_by(area) %>%
  reframe(
    min_age = min(age),
    max_age = max(age),
    quant_50 = quantile(age, 0.50),
    quant_90 = quantile(age, 0.90),
    quant_95 = quantile(age, 0.95),
    quant_99 = quantile(age, 0.99)
  )

write.csv(samples_by_area, file = file.path(dir, "forSS", "age_sample_sizes_source_area.csv"), row.names = FALSE)
write.csv(quantile_by_area, file = file.path(dir, "forSS", "age_quantiles_by_area.csv"), row.names = FALSE)


#===============================================================================
# Create figure by area
#===============================================================================

alpha <- 0.6
width <- 1
xlab <- "Age"

all_ages$Type <- factor(all_ages$program)

a <- ggplot(data = all_ages[all_ages$area == "north", ], aes(x = age)) +
  # facet_grid(Source ~ .) +
  geom_density(aes(col = Type, fill = Type), alpha = alpha, position = "identity") +
  xlab("Age") +
  ylab("Count") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

aa <- ggplot(data = all_ages, aes(x = age)) +
  geom_histogram(aes(y = ..count.., col = Type, fill = Type),
    binwidth = width, position = "stack") +
  xlab("Age") +
  ylab("Count") +
  facet_grid(~factor(area, levels = c("south", "north"))) +
  scale_fill_viridis_d() +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(size = 20))
ggsave(filename = file.path(dir, "plots", "age_histogram.png"),
       width = 24, height = 12)

b <- ggplot(data = all_ages[all_ages$area == "south", ], aes(x = age)) +
  facet_wrap(facets = "Type", ncol = 1) +
  geom_density(aes(col = Type, fill = Type), position = "identity") +
  xlab(xlab) +
  ylab("Density") +
  scale_fill_viridis_d() +
  scale_x_continuous(limits = range(all_ages$age), expand = c(0, 0)) +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(size = 25), legend.position = "none", strip.background = element_blank())
ggsave(filename = file.path(dir, "plots", "age_density_south.png"),
       width = 11, height = 11)

pngfun <- function(wd, file,w=7,h=7,pt=12){
  file <- file.path(wd, file)
  cat('writing PNG to',file,'\n')
  png(filename=file,
      width=w,height=h,
      units='in',res=300,pointsize=pt)
}

pngfun(wd = file.path(dir, "plots"), "copper_north_ages_by_source_presentation.png", w = 18, h = 12)
subvp <- grid::viewport(width = 0.6, height = 0.78, x = 0.68, y = 0.59)
aa
print(b, vp = subvp)
dev.off()

# South

a <- ggplot(data = all_ages[all_ages$area == "south", ], aes(x = age)) +
  geom_density(aes(col = Type, fill = Type), alpha = alpha, position = "identity") +
  xlab("Age") +
  ylab("Count") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

aa <- ggplot(data = all_ages[all_ages$area == "south", ], aes(x = age)) +
  geom_histogram(aes(y = ..count.., col = Type, fill = Type),
                 alpha = alpha, binwidth = width, position = "stack") +
  xlab("Age") +
  ylab("Count") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

b <- ggplot(data = all_ages[all_ages$area == "south", ], aes(x = age)) +
  facet_wrap(facets = "Type", ncol = 1) +
  geom_density(aes(col = Type, fill = Type), alpha = alpha, position = "identity") +
  xlab(xlab) +
  ylab("Density") +
  scale_color_viridis_d() +
  scale_x_continuous(limits = range(all_ages$age), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank())


pngfun(wd = file.path(dir, "plots"), "copper_south_ages_by_source_presentation.png", w = 18, h = 12)
subvp <- grid::viewport(width = 0.6, height = 0.78, x = 0.68, y = 0.59)
aa
print(b, vp = subvp)
dev.off()

#===============================================================================
# Fishery Ages
#===============================================================================

alpha <- 0.6
width <- 1
xlab <- "Age"

col_names <- c('Fleet', 'year', 'area', 'sex', 'length_cm', 'age')
crfs_ages$Fleet <- crfs_ages$mode
crfs_ages$Fleet[crfs_ages$Fleet == "PC"] <- "CPFV"
hist_rec_ages$Fleet <- "CPFV"
commercial_ages$Fleet <- "Commercial Dead"
coop_ages$Fleet <- "CPFV"
all_ages <- rbind(
  crfs_ages[, col_names],
  coop_ages[, col_names],
  commercial_ages[, col_names],
  hist_rec_ages[, col_names]
)

all_ages <- all_ages[!is.na(all_ages$age), ]
remove <- which(all_ages$Fleet == "CPFV" & all_ages$area == "south" & all_ages$year == 2022)
all_ages <- all_ages[-remove, ]

ggplot(data = all_ages, aes(x = year)) +
  geom_histogram(aes(col = Fleet, fill = Fleet), binwidth = 1, stat = "count", position = "stack") +
  xlab("Year") +
  ylab("Count") +
  facet_grid(~factor(area, levels = c("south", "north"))) + 
  scale_fill_viridis_d() +
  theme_bw(base_size = 25) +
  theme(legend.position = c(0.1, 0.85), 
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 20))
ggsave(filename = file.path(dir, "plots", "fishey_age_histogram.png"),
       width = 24, height = 12)


a <- ggplot(data = all_ages[all_ages$area == "south", ], aes(x = age)) +
  geom_density(aes(col = Fleet, fill = Fleet), alpha = alpha, position = "identity") +
  xlab("Age") +
  ylab("Count") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

aa <- ggplot(data = all_ages[all_ages$area == "south", ], aes(x = age)) +
  geom_histogram(aes(y = ..count.., col = Fleet, fill = Fleet),
                 alpha = alpha, binwidth = width, position = "stack") +
  xlab("Age") +
  ylab("Count") +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

b <- ggplot(data = all_ages[all_ages$area == "south", ], aes(x = age)) +
  facet_wrap(facets = "Fleet", ncol = 1) +
  geom_density(aes(col = Fleet, fill = Fleet), alpha = alpha, position = "identity") +
  xlab(xlab) +
  ylab("Density") +
  scale_fill_viridis_d() +
  scale_x_continuous(limits = range(all_ages$age), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank())


HandyCode::pngfun(wd = file.path(dir, "plots"), "copper_south_fishery_ages_by_source_presentation.png", w = 18, h = 12)
subvp <- grid::viewport(width = 0.6, height = 0.78, x = 0.68, y = 0.59)
aa
print(b, vp = subvp)
dev.off()


#===============================================================================
# Plot Available Ages by Area
#===============================================================================

load(file.path(dir, "2021_ages", "age_length_only_september_2021.Rdata"))
df <- df[df$Area%in% c("Oregon", "Washington"), ]

tmp <- data.frame(
  Region = c(all_ages$area, df$Area),
  length_cm = c(all_ages$length_cm, df$Length_cm),
  age = c(all_ages$age, df$Age)
)
tmp$Region[tmp$Region == "north"] <- "CA: North of Point Conception"
tmp$Region[tmp$Region == "south"] <- "CA: South of Point Conception"

ggplot(data = tmp, aes(x = age)) +
  geom_histogram(aes(y = ..count.., col = Region, fill = Region),
                 binwidth = 1, position = "stack") +
  xlab("Age") +
  ylab("Count") +
  #facet_grid(~factor(area, levels = c("south", "north"))) +
  scale_fill_viridis_d() +
  theme_bw(base_size = 20) +
  theme(legend.position = c(0.6, 0.85),
        axis.text = element_text(size = 20))
ggsave(filename = file.path(dir, "plots", "west_coast_age_histogram.png"),
       width = 12, height = 12)


ggplot(data = tmp, aes(x = age)) +
  geom_density(aes(col = Region, fill = Region), position = "identity") +
  xlab("Age") +
  ylab("Density") +
  facet_wrap(factor(Region)~.) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")
#      axis.text = element_text(size = 20))
ggsave(filename = file.path(dir, "plots", "west_coast_age_density.png"),
       width = 12, height = 12)

ggplot(tmp) + 
  geom_point(aes(y = length_cm, x = age, color = Region), alpha = 0.50, size = 3) +
  scale_color_viridis_d() +
  theme_bw(base_size = 20) +
  theme(legend.position = c(0.5, 0.15),
        axis.text = element_text(size = 20)) +
  ylim(c(0, 60)) + xlim(c(0, 55)) + 
  xlab("Age") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "west_coast_length_age.png"),
       width = 12, height = 12)


quantile_by_area <- tmp %>%
  group_by(Region) %>%
  reframe(
    n = length(age),
    min_age = min(age),
    max_age = max(age),
    quant_50 = quantile(age, 0.50),
    quant_90 = quantile(age, 0.90),
    quant_95 = quantile(age, 0.95),
    quant_99 = quantile(age, 0.99),
    quant_999 = quantile(age, 0.999)
  )

write.csv(quantile_by_area, file = file.path(dir, "forSS", "west_coast_age_quantiles_by_area.csv"), row.names = FALSE)

