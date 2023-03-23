# Compare 2019 and 2021 PISCO data files

#################### Set up: ####################
##### Remove all variables and turn off open graphics
rm(list=ls(all=TRUE))
graphics.off()

##### Load packages
library(tidyverse)  # includes dplyr, tidyr, ggplot2, stringr
library(here)  # useful for providing directory paths - it sets the base at the folder level (usually - where a RProject or Git repo or other identifiers are)

##### Load data
# Raw transects file
PISCO = read.csv(here("MLPA_fish_biomass_transect_raw.csv"))
PISCO.old = read.csv(here("PISCO_FISH_update5_22_19.csv"))
