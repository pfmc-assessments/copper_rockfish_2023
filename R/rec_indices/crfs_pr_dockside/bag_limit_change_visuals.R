################################################################################
### Look at the CDFW PR data over time to see if the 1-fish bag limit affected
### angler behavior
### Copper assessment 2023
### Melissa Monk
################################################################################
rm(list = ls(all = TRUE))
graphics.off()
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(glue)
library(ggridges)
#species and area identifiers - eventually put in function
pacfinSpecies <- 'COPP'
speciesName <- "copper"

#setwd to the north or the south
#set working directory
dir <- file.path(here(), "data", "rec_indices", "crfs_pr_dockside", "bag_change_visuals")
#setwd(glue::glue(here(),"/data/rec_indices/crfs_pr_dockside/"))
setwd(dir)
#load data for processing
load("pr_data_for_bag_visuals.RData")

raw_data <- raw_data %>%
mutate(area = ifelse(district > 2, "north", "south")) 
# bag limit exploration---------------------------------------------------------
#look at total sample sizes by year to see if there is a post-covid difference
# cdfwpr_samplesize <- raw_data %>%
#   mutate(area = ifelse(district > 2, "north", "south")) %>%
#   group_by(area, year) %>%
#   tally() %>%
#   pivot_wider(names_from = area, values_from = n)
# write.csv(cdfwpr_samplesize, "pr_trip_sample_size_by_year.csv")


#summary of over limits
aa <- PR_bag_data %>%
  dplyr::select(ID, over_limit)

bb <- left_join(raw_data, aa)

write.csv(bb, "over_limit_copper_trips.csv")

#annual summaries of kept, unobs, 
sum(raw_data$keptObs, na.rm=T)
sum(raw_data$keptUnObs, na.rm=T)
sum(raw_data$alive, na.rm=T)
sum(raw_data$relsDD, na.rm=T)
sum(raw_data$dead, na.rm=T)
sum(raw_data$SealTake, na.rm=T)



#cpue over time to see if there's a reduction in copper cpue in 2022
#will run with and without 2022
#will eventually add in the angler reported catches as well for a more complete 
#analysis
q = c(.25, .5, .75)
cpue_year <- raw_data %>%
  group_by(year) %>%
  summarize(
    min = min(cpue),
    quant25 = quantile(cpue, probs = q[1]), 
    quant50 = quantile(cpue, probs = q[2]),
    quant75 = quantile(cpue, probs = q[3]),
    max = max(cpue))
cpue_year
write.csv(cpue_year, 
          file.path(getwd(),"pr_bag_limit_by_year.csv"),
          row.names = FALSE)




#density of the number of fish per bag based on the number of anglers contributing
#to that bag use kept + unobskept
#alos look at the number of unobserved discarded to see if that changes in 2022


ggplot(raw_data %>% filter(!year == 2020) %>% filter(cpue <= 5), aes(x = cpue, y = as.factor(year), fill = as.factor(year))) +
  geom_density_ridges(alpha = 0.5) + xlab("Retained coppers per angler") + ylab("Density") +
  theme(legend.position = "none") + geom_vline(xintercept = 1) +
  scale_fill_viridis_d()
ggsave(file = file.path(dir, "pr_copper_cpue_year_area_max5.png"), width = 7, height = 7)




ggplot(bb %>% filter(!year == 2020) %>% filter(cpue <= 5) %>%
         filter(over_limit == 0), aes(x = cpue, y = as.factor(year), fill = as.factor(year))) +
  geom_density_ridges(alpha = 0.5) + xlab("Retained coppers per angler") + ylab("Density") +
  theme(legend.position = "none") + geom_vline(xintercept = 1) +
  scale_fill_viridis_d()
ggsave(file = file.path(dir, "pr_copper_cpue_year_area_nooverlimit.png"), width = 7, height = 7)



