library(here)
library(dplyr)
library(ggplot2)

dir <- here("data")

load(file.path(dir, "ages", "formatted_age_files", "coop_ages.rdata"))
load(file.path(dir, "ages", "formatted_age_files", "abrams_ages.rdata"))
load(file.path(dir, "ages", "formatted_age_files", "pearson_ages.rdata"))
load(file.path(dir, "rec_bds", "crfss_bds_filtered.rdata"))
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
             rep("WCGBT", nrow(wcgbt))),
  area = c(cpfv$area, coop_ages$area, pearson_ages$area, abrams_ages$area, wcgbt$area),
  length = c(cpfv$lengthcm, coop_ages$length_cm, pearson_ages$length_cm, 
             abrams_ages$length_cm, wcgbt$Length_cm),
  age = c(rep(NA, nrow(cpfv)), coop_ages$age, pearson_ages$age, abrams_ages$age, wcgbt$Age)
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


tmp <- data[data$source %in% c("Pearson", "Abrams", "WCGBT", "Coop.-CPFV") &
              data$area == "North", ]
ggplot(tmp, 
       aes(x = length, color = source)) + 
  geom_density(size = 2) + 
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

