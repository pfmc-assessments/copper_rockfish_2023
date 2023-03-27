################################################
# Model Bridging
################################################

library(r4ss)
area <- "sca"
# area <- "nca"
wd <- paste0("C:/Assessments/2023/copper_rockfish_2023/models/", area,"/_bridging")

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

com_catch <- SS_output(file.path(wd, "1.1_com_catch"))

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets",
				"+ Commercial Catch")
mysummary <- SSsummarize(list(base_2021,  rec_fleet, com_fleet, com_catch))

SSplotComparisons(mysummary,
	filenameprefix = "1.1_catch",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

rec_catch <- SS_output(file.path(wd, "1.2_rec_catch"))

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets",
				"+ Commercial Catch", "+ Recreational Catch")
mysummary <- SSsummarize(list(base_2021,  
	rec_fleet, com_fleet, com_catch, rec_catch))

SSplotComparisons(mysummary,
	filenameprefix = "1.2_catch",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

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

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets",
	"+ Commercial Catch", "+ Recreational Catch",
	"+ PacFIN Lengths", "+ Rec. CPFV Lengths", "+ Rec. PR Lengths")
mysummary <- SSsummarize(list(base_2021,  
	rec_fleet, com_fleet, com_catch, rec_catch, 
	com_lengths, cpfv_lengths, pr_lengths))

SSplotComparisons(mysummary,
	filenameprefix = "2.3_all_length",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

dw_len <- SS_output(file.path(wd, "2.4_dw"))

modelnames <- c("2021", "Recreational Fleets", "Commercial Fleets",
	"+ Commercial Catch", "+ Recreational Catch",
	"+ PacFIN Lengths", "+ Rec. CPFV Lengths", "+ Rec. PR Lengths",
	"+ Reweight")
mysummary <- SSsummarize(list(base_2021,  
	rec_fleet, com_fleet, com_catch, rec_catch, 
	com_lengths, cpfv_lengths, pr_lengths, dw_len))

SSplotComparisons(mysummary,
	filenameprefix = "2.4_all_length",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)


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

rov <- SS_output(file.path(wd, "4.1_rov_index"))
rov_dw <- SS_output(file.path(wd, "4.2_rov_index_dw"))

modelnames <- c("2021", "Update Com. & Rec. - DW",
"NWFSC HKL - DW", "+ ROV Index and Lengths - DW")
mysummary <- SSsummarize(list(base_2021,  
dw_len, hkl_dw, rov_dw))

SSplotComparisons(mysummary,
	filenameprefix = "4_rov",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)