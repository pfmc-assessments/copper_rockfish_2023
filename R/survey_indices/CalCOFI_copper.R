# Explore CalCOFI larval copper rockfish data
# Julia Coates, February 2023

library(HandyCode)
library(dplyr)
library(rnaturalearth)
library(tidyverse)
library(car)
library(rstanarm)
library(here)
setwd(file.path(here(), "data", "survey_indices", "calcofi"))

dat <- read.csv("CalCOFI.larvaldata.copper.csv")
dat <- dat[,1:26]
dat$line_station <- paste(dat$line, dat$station, sep="_")
siteswithcopper <- droplevels(subset(dat, caurinus>0))
sitesnocopper <- droplevels(subset(dat, caurinus<1))

sum(dat$caurinus)  #324 coppers have been found over 15 years

# show all line_stations that have seen a copper
cbind(sort(with(siteswithcopper, table(line_station)), decreasing=T))

# create character vector of line-stations to keep 
pos.sites <- names(with(siteswithcopper, table(line_station)))
length(pos.sites)  # 12 sites have detected copper out of 54 sites

# subset data to include all records from those sites, including zeros
index.df <- droplevels(subset(dat, line_station %in% pos.sites))
nocop.df <- droplevels(subset(dat, !(line_station %in% pos.sites)))
nocop.sites <- nocop.df %>% group_by(line_station) %>% summarise(first=head(line_station,1), count=n_distinct(line_station,1))
nocop.sites <- nocop.df %>% group_by(line_station) %>% summarise(latitude=head(latitude,1), longitude=head(longitude,1))


# look at seasonal distribution of samples
# season isn't in the dataset
#with(index.df, table(year, season))
#colSums(with(index.df, table(year, season)))

# look at seasonal distribution of POSITIVE samples
#with(subset(index.df, Pc_binary>0), table(year, season))
#colSums(with(subset(index.df, Pc_binary>0), table(year, season)))

# Check distribution of samples.  Season isn't a field in the data.  I think they only looked at winter collections.    
with(index.df, table(year))
with(index.df, table(line_station))
with(index.df, table(year, line_station))  # Some stations were missed in a few years but nothing consistent.  

# check distribution of positives
index.df$binary[index.df$caurinus>0] <- 1
index.df$binary[index.df$caurinus<1] <- 0
with(subset(index.df, binary>0), table(year))
with(subset(index.df, binary>0), table(line_station))
with(subset(index.df, binary>0), table(year, line_station))

# tables
out <- as.data.frame(index.df %>% group_by(year) %>% summarise(N.tows = length(binary), N.pos = sum(binary),
                                                        Prop.pos = sum(binary)/length(binary)))
colnames(out) <- c("Year", "Tows", "Positive", "Proportion")
write.csv(out, file = file.path(getwd(), "forSS", "calcofi_positive_tows.csv"), row.names = FALSE)

as.data.frame(index.df %>% group_by(line_station) %>% summarise(N.tows = length(binary), N.pos = sum(binary),
                                                                Prop.pos = sum(binary)/length(binary)))

trend <- as.data.frame(index.df %>%
                     group_by(year) %>%
                     summarise(prop.pos = sum(binary)/length(binary)))

pngfun(wd = file.path(getwd(), "plots"), file = "calcofi_trend.png")
plot(trend$year, trend$prop.pos, type='b', lwd = 2, pch = 16, ylim = c(0, max(trend$prop.pos) + 0.2),
     xlab = "Year", ylab = "Proportion Positive")
dev.off()

as.data.frame(index.df %>% group_by(year) %>% summarise(N.tows = length(caurinus), sum_copper = sum(caurinus),
                                                        ave.copper = sum(caurinus)/length(caurinus)))
with(as.data.frame(index.df %>%
                     group_by(year) %>%
                     summarise(ave.copper = sum(caurinus)/length(caurinus))),
     plot(year, ave.copper, type='o'))

# load CCA coords
cca.west <- read.csv("cca_west.csv")
cca.east <- read.csv("cca_east.csv")

# Create map object
states <- ne_states(country = 'United States of America', returnclass = 'sf')
mexico <- ne_states(country = 'Mexico', returnclass = 'sf')

# Get just CA
ca <- filter(states, name == "California")

print(
  map <- ggplot(ca) +
  geom_sf(data=mexico) +
  geom_sf() + 
  # Add sites where no copper were caught
  #geom_point(data = sites.no.hal, aes(longitude,latitude), pch=3, cex=0.7) + 
  # Add hauls with no positive obs
  #geom_point(data = subset(index.df, binary<1), aes(longitude,latitude), pch=4, col=1, cex=1) + 
    geom_point(data = nocop.sites, aes(longitude,latitude), pch=4, col=1, cex=1.2) + 
  # Add hauls where at least one copper was caught
  geom_point(data = subset(index.df, binary>0), aes(longitude,latitude), col=2, cex=1.2) + 
  # add CCAs
  #geom_polygon(data = cca.west, aes(lon,lat), fill=NA, linetype=1, colour='blue') +
  #geom_polygon(data = cca.east, aes(lon,lat), fill=NA, linetype=1, colour='blue') +
  # Make it look nice
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  # Define the projection (4326 = WGS84) and set your x/y limits manually
  coord_sf(crs = 4326,
           xlim = c(-121.5, -117), 
           ylim = c(31.5, 34.5))
)
ggsave(file = file.path(getwd(), "plots", "calcofi_map.png"), width = 7, height = 7)

# Print the map
#png("CalCOFI_site_map_copper.png", width=7, height=7, units="in", res=600)
#map
#dev.off()
