###############################################################
##   copper rockfish tag and recaptures from R. Brooks
##   May 2023
##
##


library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

setwd("S:/copper_rockfish_2023/data/biology/")

tags1 <- read.csv("Copper-Tag-Recaptures-Final-20230502-RB.csv")

tags <- tags1 %>%
  filter(Exclude != "TRUE") %>%
  mutate_at(vars(Distance_traveled_km, Recapture_Lat_DD, Release_Lat_DD), as.numeric) 

over_KM <- tags %>%
  filter(Distance_traveled_km>49) %>%
  mutate(Direction_traveled = ifelse(Recapture_Lat_DD-Release_Lat_DD >0, "north", "south"))


growth0 <- tags %>%
  filter(Recapture_length_cm >0, Release_length_cm>0) %>%
  mutate_at(vars(Recapture_length_cm, Release_length_cm), as.numeric) %>%
  mutate(Growth_cm = Recapture_length_cm - Release_length_cm) %>%
  filter(Growth_cm > 2)


growth1 <- growth0 %>%
  dplyr::select(Tag_ID, Release_length_cm) %>%
  rename(Length = Release_length_cm) %>%
  mutate(Time = 0)

growth2 <- growth0 %>%
  mutate(Time_at_liberty = as.numeric(Time_at_liberty_days)/365) %>%
  dplyr::select(Tag_ID, Recapture_length_cm, Time_at_liberty) %>%
  rename(Time = Time_at_liberty,
         Length = Recapture_length_cm) %>%
  filter(Time > 0.9) %>%
  
  
  avg_growth_rate <-  growth0 %>%
  mutate(Time_at_liberty = as.numeric(Time_at_liberty_days)/365,
         Growth_rate = Growth_cm / Time_at_liberty)

growth1 <- growth1 %>%
  filter(Tag_ID %in% growth2$Tag_ID)

growth3 <- rbind(growth1, growth2) 


ggplot(growth3, aes(Time, Length, colour = Tag_ID, group = Tag_ID)) +
  geom_point() +
  geom_line(show.legend = FALSE)


ggplot(over_halfKM, aes(Distance_traveled_km, Release_length_cm)) +
  geom_point()
