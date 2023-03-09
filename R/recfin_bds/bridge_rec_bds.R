############################################################################################
#	  Recreational data-processing for copper rockfish
#
#		          		September, 2020
#
#           			 Chantel Wetzel
############################################################################################

devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/PacFIN.Utilities")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/dataModerate_2021")

library(ggplot2)

dir = "E:/Assessments/2021/copper_rockfish_2021"
save_dir = "C:/Assessments/2023/copper_rockfish_2023/data/bridging"

len_bin = seq(10, 54, 2)


############################################################################################
#	Load Data
############################################################################################

# California Recreational
ca_recfin = read.csv(file.path(dir, "data", "recreational_comps", "ca", "Copper Revised CRFS Lengths No Region SD501-CALIFORNIA-1980-2020.csv"))
ca_recfin =	ca_recfin[ca_recfin$AGENCY_WATER_AREA_NAME != "MEXICO (AREAB AND P1B IMPORT, CPFV)", ]
ca_recfin = rename_budrick_recfin(data = ca_recfin)
ca_recfin_data = rename_recfin(data = ca_recfin,
					      area_grouping = list(c("CHANNEL", "SOUTH"), c("BAY AREA", "WINE", "CENTRAL", "REDWOOD", "NOT KNOWN")),
					      area_names = c("south_pt_concep", "north_pt_concep"),
					      area_column_name = "RECFIN_PORT_NAME",
					      mode_grouping = list(c("BEACH/BANK", "MAN-MADE/JETTY"), c("PARTY/CHARTER BOATS", "PRIVATE/RENTAL BOATS"), "NOT KNOWN"),
					      mode_names = c("rec_shore", "rec_boat", "rec_unknown"),
					      mode_column_name = "RECFIN_MODE_NAME" )
ca_recfin_data$Source = "RecFIN_MRFSS"

find = which(ca_recfin_data$RECFIN_MODE_NAME %in% c("PARTY/CHARTER BOATS", "PRIVATE/RENTAL BOATS") )
ca_recfin_data = ca_recfin_data[find,]
ca_recfin_data$mode <- "private"
ca_recfin_data$mode[ca_recfin_data$RECFIN_MODE_NAME == "PARTY/CHARTER BOATS"] <- "cpfv"
ca_recfin_data$Fleet <- ca_recfin_data$mode

ca_mrfss = read.csv(file.path(dir, 'data','recreational_comps', 'ca',  'ca_type3.csv'))
ca_mrfss = ca_mrfss[ca_mrfss$ST == 6 & ca_mrfss$SP_CODE == 8826010108, ]
ca_mrfss = ca_mrfss[!is.na(ca_mrfss$CNTY), ] # remove records without a county
ca_mrfss = ca_mrfss[ca_mrfss$YEAR != 2004, ] # overlap with crfss in 2004
ca_mrfss$STATE_NAME = "CA"
spc = c(37, 59, 73, 37, 111, 83) # 79 is San Luis Obispo which is north
npc = unique(ca_mrfss[!ca_mrfss$CNTY %in% spc, "CNTY"]) 

ca_mrfss_data = rename_mrfss(data = ca_mrfss,
							 len_col = "LNGTH",
							 area_grouping = list(spc, npc), 
							 area_names = c("south_pt_concep", "north_pt_concep"), 
							 area_column_name = "CNTY", 
							 mode_grouping = list(c(1,2), c(6, 7)),
					      	 mode_names = c("rec_shore", "rec_boat"),
					      	 mode_column_name = "MODE_FX" )

# for some reason CNTY 111 is not going to the south
find = which(ca_mrfss_data$CNTY == 111)
ca_mrfss_data[find,"State_Areas"] = "south_pt_concep"
find = which(ca_mrfss_data$MODE_FX %in% c(6,7) )
ca_mrfss_data = ca_mrfss_data[find,]
ca_mrfss_data$mode <- "cpfv"
ca_mrfss_data$mode[ca_mrfss_data$MODE_F == 8] <- 'private'

ca_mrfss_data$Fleet <- ca_mrfss_data$mode
############################################################################################
# Put all the data into one list
############################################################################################
input = list()
input[[1]] = ca_recfin_data
input[[2]] = ca_mrfss_data

############################################################################################
#	Create data frame with all the input data
############################################################################################
out = create_data_frame(data_list = input)

############################################################################################
# Clean up the data
############################################################################################
# Now lets do a check length check to filter out any anomolysis lengths
remove = which(out$Length > 65 | out$Length < 8)
#     Year Lat Lon State     State_Areas Areas Depth Sex  Length Weight Age Fleet Data_Type       Source Lhat_pred Lhat_low Lhat_high
# 2017  NA  NA    CA north_pt_concep  <NA>    NA   U 66.8000      -  NA  boat  RETAINED       RecFIN        NA       NA        NA
# 2008  NA  NA    CA north_pt_concep  <NA>    NA   U  4.2000   2.34  NA  boat  RETAINED       RecFIN        NA       NA        NA
# 1998  NA  NA    CA south_pt_concep  <NA>    NA   U  0.2649   <NA>  NA  boat      <NA> RecFIN_MRFSS        NA       NA        NA
# 1985  NA  NA    CA south_pt_concep  <NA>    NA   U 80.2000    9.5  NA  boat      <NA> RecFIN_MRFSS        NA       NA        NA
# 2012  NA  NA    OR              OR  <NA>    NA   U 66.8000   3.10  NA  boat  RETAINED       RecFIN        NA       NA        NA

# Looks good - let's remove those few lengths
out = out[-remove, ]

# Remove the released for the rest of the summaries for now:
released = out[which(out$Data_Type == "RELEASED"), ]

out = out[out$Data_Type %in% c("RETAINED", NA), ]

############################################################################################
#	North of Pt. Conception - California recreational length data
############################################################################################
nca = out[which(out$State_Areas == "north_pt_concep"), ]
nca$Length_cm = nca$Length

# create a table of the samples available by year
nca$Length_cm = nca$Length
nca$Trawl_id = 1:nrow(nca)

# There are only 10 fish sexed - change them to unsexed
nca$Sex = "U"

lfs = UnexpandedLFs.fn(
	dir = save_dir, 
    datL = nca[nca$Fleet == "cpfv",], 
    lgthBins = len_bin,
    sex = 0, partition = 0, 
    fleet = "cpfv", month = 6)

file.rename(from = file.path(save_dir,"forSS", "Survey_notExpanded_Length_comp_Sex_0_bin=10-54.csv"), 
			to= file.path(save_dir,"forSS", "nca_cpfv_rec_notExpanded_Length_comp_Sex_0_bin=10-54_may2021.csv")) 


lfs = UnexpandedLFs.fn(
	dir = save_dir, 
    datL = nca[nca$Fleet == "private",], 
    lgthBins = len_bin,
    sex = 0, partition = 0, 
    fleet = "private", month = 6)

file.rename(from = file.path(save_dir,"forSS", "Survey_notExpanded_Length_comp_Sex_0_bin=10-54.csv"), 
			to= file.path(save_dir,"forSS", "nca_private_rec_notExpanded_Length_comp_Sex_0_bin=10-54_may2021.csv")) 



############################################################################################
#	South of Pt. Conception - California recreational length data
############################################################################################

sca = out[which(out$State_Areas == "south_pt_concep"), ]
sca$Length_cm = sca$Length

# create a table of the samples available by year
sca$Length_cm = sca$Length
sca$Trawl_id = 1:nrow(sca)

# There are only 2 fish sexed - change them to unsexed
sca$Sex = "U"
lfs = UnexpandedLFs.fn(
	dir = save_dir, 
    datL = sca[sca$Fleet == "cpfv", ], 
    lgthBins = len_bin,
    sex = 0, partition = 0, fleet = "cpfv", month = 6)

file.rename(from = file.path(save_dir, "forSS", "Survey_notExpanded_Length_comp_Sex_0_bin=10-54.csv"), 
			to= file.path(save_dir, "forSS", "sca_cpfv_notExpanded_Length_comp_Sex_0_bin=10-54_may2021.csv")) 

lfs = UnexpandedLFs.fn(
	dir = save_dir, 
    datL = sca[sca$Fleet == "private", ], 
    lgthBins = len_bin,
    sex = 0, partition = 0, fleet = "private", month = 6)

file.rename(from = file.path(save_dir, "forSS", "Survey_notExpanded_Length_comp_Sex_0_bin=10-54.csv"), 
			to= file.path(save_dir, "forSS", "sca_private_notExpanded_Length_comp_Sex_0_bin=10-54_may2021.csv")) 
