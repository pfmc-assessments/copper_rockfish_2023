
PR_summary <- catch_new %>%
mutate(model_area = case_when(District < 3 ~ 'SCA',
                              District > 2 ~ 'NCA')) %>%
  mutate(species_category = case_when(Species %in% c("RFGEN","NOXXX") ~ 'Unknown',
                                      T ~'Speciated')) %>%
  group_by(model_area, YEAR, species_category) %>%
  summarise(sum_kept = sum(Kept),
            sum_unobs = sum(UnObs))

write.csv(PR_summary,"PR_obs_unobs.csv")

