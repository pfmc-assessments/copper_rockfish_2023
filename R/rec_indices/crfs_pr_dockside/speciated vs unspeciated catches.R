###############################################################################
# Calculate the number of speciated and rockfish gen across year
# to show the covid impact
################################################################################

setwd("S:/copper_rockfish_2023/data/rec_indices/crfs_pr_dockside")
#setwd(glue::glue(here(),"/data/rec_indices/crfs_pr_dockside/"))
out.dir <- file.path(getwd(), "bag_change_visuals") 

load("pr_data_for_covid_speciation.RData")



PR_summary <- PR_data %>%
  filter(grepl("RF", Species)) %>%
#mutate(model_area = case_when(District < 3 ~ 'SCA',
#                              District > 2 ~ 'NCA')) %>%
  mutate(species_category = case_when(Species %in% c("RFGEN") ~ 'Rockfish_gen',
                                      T ~'Speciated')) %>%
  group_by(YEAR, species_category) %>%
  summarise(sum_kept = sum(Kept+UnObs)) %>%
  pivot_wider(names_from = species_category, values_from = sum_kept) %>%
  mutate(Percent.RFGEN = round(Rockfish_gen / (Rockfish_gen + Speciated), 3))

write.csv(PR_summary,"PR_obs_unobs.csv")

