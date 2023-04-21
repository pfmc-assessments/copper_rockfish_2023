library(here)
library(dplyr)
library(ggplot2)

dir <- here("data")

load(file.path(dir, "ages", "formatted_age_files", "coop_ages.rdata"))
load(file.path(dir, "ages", "formatted_age_files", "abrams_ages.rdata"))
load(file.path(dir, "ages", "formatted_age_files", "pearson_ages.rdata"))
load(file.path(dir, "rec_bds", "crfss_bds_filtered.rdata"))
load(file.path(dir, "ages", "formatted_age_files", "cdfw_ages.rdata"))
load(file.path(dir, "wcgbt", "bio_copper rockfish_NWFSC.Combo_2023-04-03.rdata"))
wcgbt <- x
wcgbt$area <- "south"
wcgbt$area[wcgbt$Latitude_dd > 34.5] <- "north"

cpfv <- crfss_bds %>% filter(year == 2022 & mode == "cpfv")


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
  
data <- data.frame(
  source = c(rep("CRFS-CPFV", nrow(cpfv)), 
             rep("Coop.-CPFV", nrow(coop_ages)),
             rep("Pearson", nrow(pearson_ages)),
             rep("Abrams", nrow(abrams_ages)),
             rep("WCGBT", nrow(wcgbt)),
             rep("CDFW", nrow(cdfw_ages))),
  area = c(cpfv$area, coop_ages$area, pearson_ages$area, abrams_ages$area, wcgbt$area, cdfw_ages$area),
  length = c(cpfv$lengthcm, coop_ages$length_cm, pearson_ages$length_cm, 
             abrams_ages$length_cm, wcgbt$Length_cm, cdfw_ages$length_cm),
  age = c(rep(NA, nrow(cpfv)), coop_ages$age, pearson_ages$age, abrams_ages$age, wcgbt$Age, cdfw_ages$age)
)

data$area <- firstup(data$area)

ggplot(data[data$source %in% c("CRFS-CPFV", "Coop.-CPFV"), ], 
       aes(x = length, color = source)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d(begin = 0, end = 0.5) + 
  xlab("Length (cm)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 2)) +
  facet_grid(area~.)
ggsave(file.path(dir, "ages", "plots", "coop_crfs_length_comparison.png"),
       height = 7, width = 10)

tmp <- data[data$source %in% c("Pearson", "Abrams", "WCGBT", "Coop.-CPFV") &
            data$area == "South", ]
ggplot(tmp, 
       aes(x = length, color = source)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d() + 
  xlab("Length (cm)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(file.path(dir, "ages", "plots", "south_growth_length_comparison.png"),
       height = 7, width = 7)

ggplot(tmp, 
       aes(x = age, color = source)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d() + 
  xlab("Age") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(file.path(dir, "ages", "plots", "south_growth_age_comparison.png"),
       height = 7, width = 7)


tmp <- data[data$source %in% c("Pearson", "Abrams", "WCGBT", "CDFW") &
              data$area == "North", ]
ggplot(tmp, 
       aes(x = length, color = source)) + 
  geom_density(linewidth = 2) + 
  scale_color_viridis_d() + 
  xlab("Length (cm)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(file.path(dir, "ages", "plots", "north_growth_length_comparison.png"),
       height = 7, width = 7)

ggplot(tmp, 
       aes(x = age, color = source)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d() + 
  xlab("Age") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(file.path(dir, "ages", "plots", "north_growth_age_comparison.png"),
       height = 7, width = 7)


ggplot(data, 
       aes(x = age, color = source)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d() + 
  xlab("Age") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_grid(area~.)
ggsave(file.path(dir, "ages", "plots", "north_south_age_comparison.png"),
       height = 7, width = 10)

ggplot(data, 
       aes(x = age, color = area)) + 
  geom_density(size = 2) + 
  scale_color_viridis_d(begin=0, end = 0.5) + 
  xlab("Age") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


load(file.path(dir, "rec_bds", "crfs_ages.rdata"))
load(file.path(dir, "rec_bds", "historical_rec_ages.rdata"))
load(file.path(dir, "rec_bds", "all_rec_length_data.rdata"))

table(all_data$year)
rec <- all_data[all_data$year %in% c(1975, 1978, 1981, 1984) & all_data$mode == "cpfv",]

table(hist_rec_ages$area, hist_rec_ages$mode, hist_rec_ages$year)
ggplot(hist_rec_ages) +
  geom_density(aes(x = length_cm, color = area)) + 
  facet_wrap("year")

ggplot(rec) +
  geom_density(aes(x = lengthcm, color = area)) + 
  facet_wrap("year")

data <- data.frame(
  year = c(rec$year, hist_rec_ages$year),
  source = c(rep("CPFV-Length", nrow(rec)), 
             rep("CPFV-Ages", nrow(hist_rec_ages))),
  area = c(rec$area, hist_rec_ages$area),
  length = c(rec$lengthcm, hist_rec_ages$length_cm),
  age = c(rep(NA, nrow(rec)), hist_rec_ages$age)
)

data$area <- firstup(data$area)

tmp <- data[data$area == "North", ]

ggplot(tmp, 
       aes(x = length, color = source)) + 
  geom_density(linewidth = 2) + 
  scale_color_viridis_d() + 
  xlab("Length (cm)") + ylab("Density") + 
  facet_wrap("year") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(file.path(dir, "ages", "plots", "north_hist_cpfv_length_comparison.png"),
       height = 7, width = 7)

# Note below that it is plotting 84 lengths from ages vs 346 lengths
tmp <- data[data$area == "South" & data$year == 1975, ]
ggplot(tmp, 
       aes(x = length, color = source)) + 
  geom_density(linewidth = 2) + 
  scale_color_viridis_d() + 
  xlab("Length (cm)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(file.path(dir, "ages", "plots", "south_hist_cpfv_length_comparison.png"),
       height = 7, width = 7)


age <- crfs_ages[crfs_ages$mode == "PR" & crfs_ages$area == "north", ]
rec <- all_data[all_data$year == 2022 & all_data$mode == "private" & all_data$area == "north",]

data <- data.frame(
  year = c(rec$year,age$year),
  source = c(rep("PR-Length", nrow(rec)), 
             rep("PR-Ages", nrow(age))),
  area = c(rec$area, age$area),
  length = c(rec$lengthcm, age$length_cm),
  age = c(rep(NA, nrow(rec)), age$age)
)

ggplot(data, 
       aes(x = length, color = source)) + 
  geom_density(linewidth = 2) + 
  scale_color_viridis_d() + 
  xlab("Length (cm)") + ylab("Density") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

ggsave(file.path(dir, "ages", "plots", "north_crfs_pr_length_comparison.png"),
       height = 7, width = 7)


# Load in the Pearson ages and plot their locations
pearson <- read.csv(file.path(dir, "ages", "formatted_age_files", "copper_don_pearson_research_ages_2001-2007.csv"))
pearson$lat <- pearson$START_LAT / 100
pearson$lon <- -1 * pearson$START_LONG / 100
pearson$n <- 1


pearson_site <- pearson %>%
  group_by(lat, lon) %>%
  reframe(
    Count = sum(n)
  )

tmp <- pearson_site[pearson_site$lat > 34.5, ]

ggplot() +
  geom_point(data = tmp, aes(x = lon, y = lat, size = Count), color = 'blue', alpha = 0.5) +
  scale_size_binned("Count", breaks = c(1, 2, 5, 10,  20, 35)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(-121.0, -124.5), c(34.5, 42)) 
ggsave(file = file.path(dir, "ages", "plots", "pearson_age_locations_north.png"),
       width = 12, height = 15)

tmp <- pearson_site[pearson_site$lat <= 34.5, ]

ggplot() +
  geom_point(data = tmp, aes(x = lon, y = lat, size = Count), color = 'blue', alpha = 0.5) +
  scale_size_binned("Count", breaks = c(2, 3, 5, 8, 16)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "right") +
  xlab("Longitude") + ylab("Latitude") +
  draw_theme() +
  draw_projection() +
  draw_land() +
  draw_USEEZ(c(-117, -122), c(31.9, 34.5)) 
ggsave(file = file.path(dir, "ages", "plots", "pearson_age_locations_south.png"),
       width = 12, height = 15)