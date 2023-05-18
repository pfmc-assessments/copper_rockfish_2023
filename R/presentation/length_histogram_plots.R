
library(here)
library(ggplot2)
library(grid)
library(dplyr)

# Load in age data from all sources
dir <- here("data")

load(here("data", "rec_bds",  "all_rec_length_data.rdata"))
rec_bds <- all_data
load(here("data", "pacfin_bds", "pacfin_cleaned_data.rdata"))
com_bds <- data

com_bds$area <- "south"
com_bds$area[com_bds$fleet %in% c("north.dead", "north.live")] <- "north"
com_bds$Source <- "Commercial Dead"
com_bds$Source[com_bds$fleet %in% c("south.live", "north.live")] <- "Commercial Live"

rec_bds$Source <- "CPFV"
rec_bds$Source[rec_bds$mode == "private"] <- "PR"

col_names <- c("year", "Source", "area", "lengthcm")
all_len <- rbind(rec_bds[, col_names], com_bds[, col_names])


ggplot(data = all_len, aes(x = year)) +
  geom_histogram(aes(y = ..count.., col = Source, fill = Source), binwidth = 1, position = "stack") +
  xlab("Year") +
  ylab("Count") +
  facet_grid(~factor(area, levels = c("south", "north"))) + 
  scale_fill_viridis_d() +
  theme_bw(base_size = 25) +
  theme(legend.position = c(0.1, 0.85), 
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 20))
ggsave(filename = file.path(dir, "plots", "length_histogram.png"),
       width = 24, height = 12)


ggplot(data = all_len[all_len$area == "south", ], aes(x = year)) +
  geom_histogram(aes(y = ..count.., col = Source, fill = Source), binwidth = 1, position = "stack") +
  xlab("Year") +
  ylab("Count") +
  #facet_grid(source~.) + 
  scale_fill_viridis_d() +
  theme_bw(base_size = 25) +
  theme(legend.position = c(0.2, 0.85), 
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(filename = file.path(dir, "plots", "length_histogram_south.png"),
       width = 12, height = 12)