###################################################################################
#
#       Copper rockfish 2023
#        NWFSC WCGBT survey 
#    	  data exploration 
#
#############################################################################################

devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
library(nwfscSurvey)
library(ggplot2)
library(dplyr)

dir_main <- "C:/Assessments/2023/copper_rockfish_2023/data/wcgbt"

#=====================================================================
# Pull all available data
#=====================================================================
load(file.path(dir_main, "catch_copper rockfish_NWFSC.Combo_2022-11-27.rdata"))
catch <- x
load(file.path(dir_main, "bio_copper rockfish_NWFSC.Combo_2022-11-27.rdata"))
bio <- x

# Filter down to California data only
catch <- catch[catch$Latitude_dd < 42, ]
bio <- bio[bio$Latitude_dd < 42, ]

catch$area <- 'north'
catch[catch$Latitude_dd < 34.47, 'area'] <- 'south'

bio$area <- 'north'
bio[bio$Latitude_dd < 34.47, 'area'] <- 'south'

catch$positive <- 0
catch$positive[catch$total_catch_numbers > 0] <- 1

bio$count <- 1
bio$length_bin <- plyr::round_any(bio$Length_cm, 2, floor)

table(catch$area[catch$total_catch_numbers > 0])
# north south 
#    58    99 

table(catch$positive, catch$Year, catch$area)
# north  
# 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2021
#  4    4    2    2    1    6    5    5    0    3    3    1    4    1    2    5    3    7
# 
# south
#   
# 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2021
#  4    1    3    1    4    5    2    4    3   16    6    7    5    8   10    6    4   10

table(bio$area)
# north south 
#   214   925 

PlotMap.fn(dat = catch, plot = 1)
PlotMap.fn(dat = catch[catch$area == 'north',], plot = 1)
PlotMap.fn(dat = catch[catch$area == 'south',], plot = 1)

plot_bio_patterns(
  bio = bio, 
  col_name = "Length_cm")

ggplot(bio, aes(Length_cm, fill = Sex, color = Sex)) + 
	geom_density(alpha = 0.4, lwd = 0.8, adjust = 0.5) + 
    xlab("Length (cm)") + ylab("Density") +
    scale_fill_viridis_d() +
    facet_grid(area~.) 
ggsave(filename = file.path(dir, "plots", ""),
	width = 10, height = 5)


ggplot(bio, aes(y = count, x = length_bin, fill = Sex))  + 
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Number of Length Samples") +
    facet_grid(area~.)  + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir_main, "plots", "wcgbt_length_samples_by_area.png"),
	width = 10, height = 8)

ggplot(bio[!is.na(bio$Otosag_id), ], aes(y = count, x = length_bin, fill = Sex))  + 
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Length (cm)") + ylab("Number of Length Samples with Otoliths") +
    facet_grid(area~.)  + 
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir_main, "plots", "wcgbt_length_samples_w_otoliths_by_area.png"),
	width = 10, height = 8)

ggplot(bio, aes(y = Length_cm, x = Age)) +
	geom_point(aes(col = Sex)) + 
	facet_grid(area~.)  + 
	scale_colour_viridis_d() + 
	xlab("Age") + ylab("Length (cm)") 

ggplot(catch, aes(y = positive, x = Year))  + 
	geom_histogram(aes(y = positive), position = "stack", stat="identity") + 
    xlab("Year") + ylab("Number of Positive Tows") +
    facet_grid(area~.)  + 
    theme(axis.text = element_text(size = 12),
      	axis.title = element_text(size = 12),
      	legend.title = element_text(size = 12),
      	legend.text = element_text(size = 12),
      	strip.text.y = element_text(size = 14)) +
    scale_fill_viridis_d()
ggsave(filename = file.path(dir_main, "plots", "wcgbt_positive_tows_by_area.png"),
	width = 10, height = 8)

ggplot(bio, aes(y = Length_cm, x = Year, group = Year)) +
	geom_boxplot() + 
	facet_grid(area~.)  + 
	xlab("Year") + ylab("Length (cm)") 
ggsave(filename = file.path(dir_main, "plots", "wcgbt_boxplot_lengths_by_year_area.png"),
	width = 10, height = 8)

ggplot(bio, aes(y = count, x = Year, fill = Sex))  + 
	geom_histogram(aes(y = count), position="stack", stat="identity") + 
    xlab("Year") + ylab("Number of Length Samples") +
    facet_grid(area~.)  + 
    scale_fill_viridis_d()
ggsave(filename = file.path(dir_main, "plots", "wcgbt_lengths_by_year_area.png"),
	width = 10, height = 8)

