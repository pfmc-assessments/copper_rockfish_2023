
library(ggplot2)
library(ggridges)
library(tidyr)
library(dplyr)
library(viridis)
library(here)
load( file.path(here(), "data", "ages", "formatted_age_files", "all_copper_ages.rdata"))
load(file.path(here(), "data", "rec_bds", "crfss_bds_filtered.rdata"))
dir <- here("data", "rec_bds", "plots")

# Add month to the CRFS data
crfss_bds$convert_date <- crfss_bds$RECFIN_DATE
crfss_bds <- crfss_bds %>%
  tidyr::separate(convert_date, sep="/", into = c("month", "day", "year"))
crfss_bds$RECFIN_MONTH <- crfss_bds$month

ages <- all_ages %>% 
  filter(!program == "unknown") %>%
  filter(!area == "unknown") 

south_coop_ages <- ages %>%
  filter(program=="CPFV_COOP_collections") %>%
  filter(!is.na(length_cm)) %>%
  droplevels


ggplot(ages, 
       aes(x = length_cm, fill = as.factor(program), 
                     colour = as.factor(program))) +
         geom_density(alpha = 0.5) +
         facet_wrap(~area)

crfss_pr <- crfss_bds %>%
  mutate(length_cm  = AGENCY_LENGTH/10) %>%
  filter(!mode %in% ("cpfv")) %>%
  mutate_at(vars(RECFIN_YEAR), as.factor)

crfss_pc <- crfss_bds %>%
  mutate(length_cm  = AGENCY_LENGTH/10) %>%
  filter(mode == "cpfv") %>%
  mutate_at(vars(RECFIN_YEAR), as.factor)

crfss_annual <- crfss_pc %>%
  group_by(RECFIN_YEAR, area) %>%
  tally() %>%
  pivot_wider(names_from = area, values_from = n)

crfss_pr_annual <- crfss_pr %>%
  group_by(RECFIN_YEAR, area) %>%
  tally() %>%
  pivot_wider(names_from = area, values_from = n)


ggplot(crfss_pc, aes(length_cm, y = RECFIN_YEAR, fill = RECFIN_YEAR)) +
  geom_density_ridges(alpha = 0.6) +
  #theme_ridges() +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme(legend.position="none") +
  xlab("Length (cm)") + ylab("Year") +
  facet_wrap(~area)
ggsave(filename = file.path(dir, "crfss_cpfv_ggridges_year_area.png"),
       width = 10, height = 10)

ggplot(crfss_pr, aes(length_cm, y = RECFIN_YEAR, fill = RECFIN_YEAR)) +
  geom_density_ridges(alpha = .6) +
  #theme_ridges() +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme(legend.position="none") +
  xlab("Length (cm)") + ylab("Year") +
  facet_wrap(~area)
ggsave(filename = file.path(dir, "crfss_pr_ggridges_year_area.png"),
       width = 10, height = 10)

south  <- crfss_pc %>%
  filter(mode == "cpfv",
         area == "south") 

south_2022 <- south %>%
  filter(RECFIN_YEAR == 2022)

# south 2022 summaries ----
south22_summary <- south_2022 %>%
  dplyr::select(RECFIN_YEAR, RECFIN_DATE, COUNTY_NUMBER, INTERVIEW_SITE,
                RECFIN_PORT_NAME) %>%
  unique() %>%
  group_by(COUNTY_NUMBER, INTERVIEW_SITE, RECFIN_PORT_NAME) %>%
  tally()

south22_nfish <- south_2022 %>%
  dplyr::select(COUNTY_NUMBER, INTERVIEW_SITE,
                                 RECFIN_PORT_NAME) %>%
  group_by(COUNTY_NUMBER, INTERVIEW_SITE, RECFIN_PORT_NAME) %>%
  tally()

south22_nfish1 <- south_2022 %>%
  dplyr::select(
                RECFIN_PORT_NAME) %>%
  group_by(RECFIN_PORT_NAME) %>%
  tally()





north <- crfss_bds %>%
  filter(mode == "cpfv",
         area == "north")

aa <- south %>%
  group_by(RECFIN_YEAR, RECFIN_MONTH, IS_RETAINED) %>%
  tally() %>%
  pivot_wider(names_from = IS_RETAINED, values_from = n)

aaa <- south %>%
  group_by(RECFIN_YEAR) %>%
  tally()

bb <- north %>%
  group_by(RECFIN_YEAR, IS_RETAINED) %>%
  tally() %>%
  pivot_wider(names_from = IS_RETAINED, values_from = n)


ggplot(south %>% filter(RECFIN_YEAR %in% c(2018, 2019,2020,2021,2022)), aes(length_cm, colour = as.factor(IS_RETAINED), 
               fill = as.factor(IS_RETAINED))) +
         geom_density(alpha = .5) +
         facet_wrap(~RECFIN_YEAR)


ggplot(north %>% filter(RECFIN_YEAR>2018), aes(lengthcm, colour = as.factor(IS_RETAINED), 
                  fill = as.factor(IS_RETAINED))) +
  geom_density(alpha = .5) +
  facet_wrap(~RECFIN_YEAR)


cc <- crfss_bds %>%
  filter(mode == "private",
         area == "south") %>%
  group_by(RECFIN_YEAR, IS_RETAINED) %>%
  tally() %>%
  pivot_wider(names_from = IS_RETAINED, values_from = n)


crfs_2022 = crfss_pc %>%
  filter(area == "south",
         mode == "cpfv",
         RECFIN_YEAR==2022)

crfs_2022_district <- crfs_2022 %>%
  group_by(RECFIN_PORT_NAME) %>%
  tally()

ggplot(crfs_2022, aes(length_cm)) +
  geom_density(fill = "blue", alpha = .5) +
  geom_density(data = south_coop_ages, aes(length_cm, fill = "purple"),
               alpha = .5)


coop_blocks <- read.csv(file.path(here(), "data", "rec_bds", "coop_data.csv"))
coop_block_summary <- coop_blocks %>%
  group_by(CDFWBlockID) %>%
  tally()

coop_blocks <- coop_blocks %>%
  filter(!is.na(ForkLengthMM)) %>%
  mutate(length_cm = as.numeric(ForkLengthMM)/10) %>%
  filter(area=="south") %>%
 # filter(CDFWBlockID %in% c(711,688,813)) %>%
  mutate_at(vars(CDFWBlockID), as.factor)

crfs_2022$port <- sapply(strsplit(crfs_2022$RECFIN_PORT_NAME, '\\s*[()]'), '[',1)

ggplot(coop_blocks, aes(length_cm)) +
    geom_density(aes(fill = CDFWBlockID), alpha = .5) +
geom_density(data = crfs_2022,
             aes(length_cm, fill = as.factor(port)), alpha= .5) +
  scale_fill_viridis_d() + 
  xlab("Length (cm)") + ylab("Density")
ggsave(filename = file.path(dir, "crfs_coop_cpfv_len_by_block_2022.png"),
       width = 10, height = 10)

#just SB landing

ggplot(coop_blocks %>% filter(Vessel %in% c("Coral Sea", "Stardust"), CDFWBlockID %in% c(687,688)), aes(length_cm)) +
  geom_density(aes(fill = CDFWBlockID),
               alpha = .5) +
  geom_density(data = crfs_2022, #%>% filter(COUNTY_NUMBER==83, INTERVIEW_SITE==400),
               aes(length_cm, fill = as.factor(port)), alpha= .5) +
  scale_fill_viridis_d() + 
xlab("Length (cm)") + ylab("Density")
ggsave(filename = file.path(dir, "sb_only_crfs_coop_cpfv_len_by_block_2022.png"),
       width = 10, height = 10)


ggplot(coop_blocks, aes(length_cm)) +
  geom_density() +
  geom_density(data = crfs_2022, aes(length_cm)) +
  scale_fill_viridis_d()

ggplot(coop_blocks, aes(length_cm)) +
  geom_density(aes(fill = CDFWBlockID), alpha = 0.5) +
  scale_fill_viridis_d() + 
  facet_grid(DurationType~.) +
  xlab("Length (cm)") + ylab("Density")
ggsave(filename = file.path(dir, "coop_cpfv_len_by_block_trip_duration_2022.png"),
       width = 10, height = 10)

