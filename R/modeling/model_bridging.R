################################################
# Model Bridging
################################################

library(r4ss)
#area <- "sca"
area <- "nca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area,"_bridging")

#===============================================================================
# Compare the converted SS3 version
#===============================================================================

base_2021 <- SS_output(file.path(wd, "00_2021_base"))
ss_exe <- SS_output(file.path(wd, "0_0_ss_exe"))

modelnames <- c("2021: 3.30.16", "3.30.20")
mysummary <- SSsummarize(list(base_2021,  ss_exe))

SSplotComparisons(mysummary,
	subplots = c(2,4),
	filenameprefix = "0_model_convert_",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	print = TRUE,
	pdf = FALSE)

#===============================================================================
# 0_ move to new fleet structure
#===============================================================================

rec_fleet <- SS_output(file.path(wd, "0_1_rec_fleet"))

modelnames <- c("2021", "Recreational Fleets")
mysummary <- SSsummarize(list(base_2021,  rec_fleet))

SSplotComparisons(mysummary,
	filenameprefix = "0_1_rec_fleet",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

com_fleet <- SS_output(file.path(wd, "0_2_com_fleet"))

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets")
mysummary <- SSsummarize(list(base_2021,  rec_fleet, com_fleet))

SSplotComparisons(mysummary,
	filenameprefix = "0_2_com_fleet",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)


#===============================================================================
# 1.+ Update removals
#===============================================================================

biology <- SS_output(file.path(wd, "1.0_biology"))

com_catch <- SS_output(file.path(wd, "1.1_com_catch"))

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets", "Biology",
				"+ Commercial Catch")
mysummary <- SSsummarize(list(base_2021,  rec_fleet, com_fleet, biology, com_catch))

SSplotComparisons(mysummary,
	filenameprefix = "1.1_catch",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

rec_catch <- SS_output(file.path(wd, "1.2_rec_catch"))

modelnames <- c("2021", "Updated Fleets", "Update WL & Mat.",
				"+ Commercial Catch", "+ Recreational Catch")
mysummary <- SSsummarize(list(base_2021,  
	biology, com_fleet, com_catch, rec_catch))

SSplotComparisons(mysummary,
	filenameprefix = "1.2_catch",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

#===============================================================================
# 2.+ Add new length data
#===============================================================================

com_lengths <- SS_output(file.path(wd, "2.1_com_lengths"))

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets",
	"+ Commercial Catch", "+ Recreational Catch",
	"+ PacFIN Lengths")
mysummary <- SSsummarize(list(base_2021,  
	rec_fleet, com_fleet, com_catch, rec_catch, 
	com_lengths))

SSplotComparisons(mysummary,
	filenameprefix = "2.1_length",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

cpfv_lengths <- SS_output(file.path(wd, "2.2_cpfv_lengths"))

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets",
	"+ Commercial Catch", "+ Recreational Catch",
	"+ PacFIN Lengths", "+ Rec. CPFV Lengths")
mysummary <- SSsummarize(list(base_2021,  
	rec_fleet, com_fleet, com_catch, rec_catch, 
	com_lengths, cpfv_lengths))

SSplotComparisons(mysummary,
	filenameprefix = "2.2_cpfv_length",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

pr_lengths <- SS_output(file.path(wd, "2.3_pr_lengths"))

modelnames <- c("2021", "Fleets", "Biology",
	"+ Commercial Catch", "+ Recreational Catch",
	"+ PacFIN Lengths", "+ Rec. CPFV Lengths", "+ Rec. PR Lengths")
mysummary <- SSsummarize(list(base_2021,  
	com_fleet, biology, com_catch, rec_catch, 
	com_lengths, cpfv_lengths, pr_lengths))

SSplotComparisons(mysummary,
	filenameprefix = "2.3_all_length",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

dw_len <- SS_output(file.path(wd, "2.4_dw"))

modelnames <- c("2021", 
	"+ Commercial Catch", "+ Recreational Catch",
	"+ Com. Lengths", "+ Rec. CPFV Lengths", "+ Rec. PR Lengths",
	"+ Reweight")
mysummary <- SSsummarize(list(base_2021,  
	com_catch, rec_catch, 
	com_ages, cpfv_lengths, pr_lengths, dw_len))

SSplotComparisons(mysummary,
	filenameprefix = "2.4_all_comps",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

#===============================================================================
# 3.+ NWFSC HKL data - south only
#===============================================================================

hkl_len <- SS_output(file.path(wd, "3.1_hkl_index_len"))
hkl_age <- SS_output(file.path(wd, "3.2_hkl_ages"))
hkl_dw <- SS_output(file.path(wd, "3.3_hkl_dw"))


modelnames <- c("2021", "Update Com. & Rec. - DW",
"+ NWFSC HKL index and lengths", "+ NWFSC HKL CAAL",
"Data Weighted")
mysummary <- SSsummarize(list(base_2021,  
dw_len, hkl_len, hkl_age, hkl_dw))

SSplotComparisons(mysummary,
	filenameprefix = "3_nwfsc_hkl",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

#===============================================================================
# 4.+ ROV survey data
#===============================================================================

rov <- SS_output(file.path(wd, "4.1_rov_index"))
rov_dw <- SS_output(file.path(wd, "4.2_rov_index_dw"))

modelnames <- c("2021", "Update Com. & Rec. - DW",
#"NWFSC HKL - DW", 
"+ ROV Index and Lengths - DW")
mysummary <- SSsummarize(list(base_2021,  
dw_len, #hkl_dw, 
rov_dw))

SSplotComparisons(mysummary,
	filenameprefix = "4_rov",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

#===============================================================================
# Recreational indices
#===============================================================================
mrfss_pc <- SS_output(file.path(wd, "5.1_mrfss_pc_index"))
crfs_pr <- SS_output(file.path(wd, "5.2_crfs_pr_index"))


modelnames <- c("2021", "+ ROV Index and Lengths - DW", "+ MRFSS PC Index", "+ CRFS PR Index")
mysummary <- SSsummarize(list(base_2021,  
                             rov_dw, mrfss_pc, crfs_pr))

SSplotComparisons(mysummary,
                  filenameprefix = "5_rec_indices",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)
