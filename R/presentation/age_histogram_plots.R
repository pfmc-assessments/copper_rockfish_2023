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
all_ages$program[all_ages$program %in% c("Commercial - EFI", "Commercial - Pilot Sampling", "Whole")] <- "CDFW Special Collections"
all_ages$program[all_ages$program == "abrams"] <- "Abrams Research"
all_ages$program[all_ages$program == "commercial"] <- "Commercial"
all_ages$program[all_ages$program %in% c("CRFS", "recreational")] <- "Recreational"


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

aa <- ggplot(data = all_ages[all_ages$area == "north", ], aes(x = age)) +
  geom_histogram(aes(y = ..count.., col = Type, fill = Type),
    alpha = alpha, binwidth = width, position = "stack") +
  xlab("Age") +
  ylab("Count") +
  scale_color_viridis_d() +
  theme_bw() +
  theme(legend.position = "none")

b <- ggplot(data = all_ages[all_ages$area == "north", ], aes(x = age)) +
  facet_wrap(facets = "Type", ncol = 1) +
  geom_density(aes(col = Type, fill = Type), alpha = alpha, position = "identity") +
  xlab(xlab) +
  ylab("Density") +
  scale_color_viridis_d() +
  scale_x_continuous(limits = range(all_ages$age), expand = c(0, 0)) +
  theme_bw() +
  theme(legend.position = "none", strip.background = element_blank())


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