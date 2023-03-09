###################################################################################
#
#                 Combine ages into NWFSC HKL data and 
#                     save a clean rdata object
#                       Copper Rockfish 2023
#                         Chantel Wetzel
#
#############################################################################################

library(dplyr)
library(here)

dir <- file.path(here::here(), "data", "nwfsc_hkl")

hkl_all <- read.csv(file.path(dir, "H&LSurveyDataThru2022_DWarehouse version_03042023.csv"))
age_reads1 <- read.csv(file.path(dir, "NWFSC_HKL_2004-2019_COPP_AgeData_20210123.csv"))
age_reads2 <- read.csv(file.path(dir, "Updated_H&L metadata for  2021-22 copper to ageing lab_01272023.csv"))

ages <- rbind(
  age_reads1[, c("FishInfoID", "Final.Age")], 
  age_reads2[, c("FishInfoID", "Final.Age")])
colnames(ages) <- c("fish_info_id", "age")

hkl <- dplyr::left_join(hkl_all, ages)

which(!ages$fish_info_id %in% hkl_all$fish_info_id)
#[1]   83  372 1013 1014
not_included = ages[which(!ages$fish_info_id %in% hkl_all$fish_info_id),]

#age_reads2
#Species	Year	Vessel	Vessel ID	SetID	Site	Date	Latitude (N)	Longitude (W)	Depth (m)	Fork length (cm)	Wt (kg)	Sex	OtolithNum	FishInfoID	Bin #	Tray #	Age	Ager	date aged
#Copper Rockfish	2006	Aggre  	AG	06-01-05-00	396	 26-Sep-06	34 03.491	119 42.535	67.2	31	0.68	F	A0033	6302			5	tjohnson	9/2/2021
#Copper Rockfish	2018	Mirage	MI	18-04-01-067	66	04-Oct-18	33 18.040	118 18.112	81.3	31	0.56	F	A1251	78296			5
#Copper Rockfish	2018	Mirage	MI	18-04-01-067	66	04-Oct-18	33 18.049	118 18.068	83.8	33	0.68	M	A1254	78301			5

#Species	Year	Vessel	Date	Fork length (cm)	Length Bin	Sex	OtolithNum	FishInfoID	Tyler's Age	Tyler's Range	ODFW Age Estimate	Patrick's Age	Patrick's Range	Final Age
#Copper Rockfish	2008	Mirage	26-Sep-08	40	36-40	M	A0172	12964	11	9-12		11	9-12	11

check = which(
  hkl_all$year %in% c(2006, 2018) &
  hkl_all$set_id %in% c("06-01-05-006", "18-04-01-067"))
# No copper rockfish returned on this set and year

check = which(
  hkl_all$year == 2008 &
    hkl_all$date_yyyymmdd == "20080926" & 
    hkl_all$common_name == "Copper Rockfish" &
    hkl_all$length_cm == 40 & 
    hkl_all$vessel == "Mirage")
# 3 fish found but not clear which one

replace <- which(!is.na(hkl$age))
hkl$age_years[replace] <- hkl$age[replace]
hkl$lat <- hkl$drop_latitude_degrees
hkl$lon <- hkl$drop_longitude_degrees
hkl$area <- ifelse(hkl$cowcod_conservation_area_indicator == 1, "CCA", "Outside CCA")

save(hkl, file = file.path(dir, "nwfsc_hkl_2004-2022.rdata"))

# Do a quick look at the data
find = which(hkl$common_name == "Copper Rockfish" & !is.na(hkl$age))

age_len <- hkl[find, ] %>%
  group_by(sex, length_cm, age) %>%
  summarise(count = n())

write.csv(age_len, file = file.path(dir, "forSS", "nwfsc_hkl_age_length_sex_count.csv"))

ggplot(hkl[find, ], aes(y = length_cm, x = age)) +
  geom_point(aes(col = sex)) + 
  ylim(0, 55) + xlim(0, 55) +
  scale_colour_viridis_d() + 
  xlab("Age") + ylab("Length (cm)") 
ggsave(filename = file.path(dir, "plots", "nwfsc_hkl_age_at_length.png"),
       width = 10, height = 8)


