################################################
# Model Bridging
################################################

library(r4ss)
area <- "sca"
#area <- "nca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area,"_bridging")

#===============================================================================
# Compare the converted SS3 version
#===============================================================================

base_2021 <- SS_output(file.path(wd, "00_2021_base"))
ss_exe <- SS_output(file.path(wd, "0_0_ss_exe"))

modelnames <- c("2021: 3.30.16", "3.30.21")
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
# 2.+ Add new length and age data for the fishery fleets
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


com_ages <- SS_output(file.path(wd, "2.4_com_ages"))

rec_ages <- SS_output(file.path(wd, "2.5_rec_ages"))

dw_fishery_comps <- SS_output(file.path(wd, "2.6_dw_fishery_comps"))

modelnames <- c("2021", 
	"+ Commercial Catch", "+ Recreational Catch",
	"+ Com. Lengths", "+ Rec. CPFV Lengths", "+ Rec. PR Lengths",
	"+ Com. Ages", "+ Rec. CPFV Ages", "+ Reweight")
mysummary <- SSsummarize(list(base_2021,  
	com_catch, rec_catch, 
	com_lengths, cpfv_lengths, pr_lengths, 
	com_ages, rec_ages, dw_fishery_comps))

SSplotComparisons(mysummary,
	filenameprefix = "2.6_all_comps",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

#===============================================================================
# 3.+ Recreational indices
#===============================================================================

mrfss_index <- SS_output(file.path(wd, "3.1_mrfss_cpfv_index"))

dwv_index <- SS_output(file.path(wd, "3.2_debwv_index"))

crfs_index <- SS_output(file.path(wd, "3.3_crfs_cpfv_index"))

crfs_pr_index <- SS_output(file.path(wd, "3.4_crfs_pr_index"))

modelnames <- c("2021", 
                "+ Commercial Catch", "+ Recreational Catch",
                "+ Fishery Lengths & Ages",
                "+ MRFSS CPFV Index", 
                #"+ DWV CPFV Index", 
                "+ CRFS CPFV Index",
                "+ CRFS PR Index")
mysummary <- SSsummarize(list(base_2021,  
                              com_catch, rec_catch, dw_fishery_comps,
                              mrfss_index, 
                              #dwv_index, 
                              crfs_index, crfs_pr_index))

SSplotComparisons(mysummary,
                  filenameprefix = "3.4_rec_indices",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#===============================================================================
# 4.+ ROV survey data
#===============================================================================

rov <- SS_output(file.path(wd, "4.1_rov_index"))
rov_dw <- SS_output(file.path(wd, "4.2_rov_index_dw"))

modelnames <- c("2021",
                "+ Fishery Lengths & Ages",
                "+ Fishery Indices",
                "+ ROV Index and Lengths - DW")
mysummary <- SSsummarize(list(base_2021, dw_fishery_comps, 
  crfs_pr_index, rov_dw))

SSplotComparisons(mysummary,
	filenameprefix = "4_rov",
	legendlabels = modelnames, 	
	plotdir = file.path(wd, "_plots"),
	pdf = TRUE)

#===============================================================================
# 5.+ CCFPR
#===============================================================================
ccfrp_comps <- SS_output(file.path(wd, "5.1_ccfrp_comps"))
ccfrp_comps_dw <- SS_output(file.path(wd, "5.2_ccfrp_comps_dw"))
ccfrp_index <- SS_output(file.path(wd, "5.3_ccfrp_index"))

modelnames <- c("2021",
                "+ Fishery Lengths & Ages",
                "+ Fishery Indices",
                "+ ROV Index and Lengths - DW",
                "+ CCFRP Lengths & Ages - DW",
                "+ CCFRP Index")
mysummary <- SSsummarize(list(base_2021, dw_fishery_comps, 
                              crfs_pr_index, rov_dw,
                              ccfrp_comps_dw, ccfrp_index))

SSplotComparisons(mysummary,
                  filenameprefix = "5_ccfrp",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#===============================================================================
# 5.+ NWFSC HKL data - south only
#===============================================================================

hkl_comps <- SS_output(file.path(wd, "5.4_nwfsc_hkl_comps"))
hkl_comps_dw <- SS_output(file.path(wd, "5.5_nwfsc_hkl_comps_dw"))
hkl_index <- SS_output(file.path(wd, "5.6_nwfsc_hkl_index"))

modelnames <- c("2021",
                "+ Fishery Lengths & Ages",
                "+ Fishery Indices",
                "+ ROV Index and Lengths - DW",
                "+ CCFRP Lengths & Ages - DW",
                "+ CCFRP Index", 
                "+ NWFSC HKL Lengths & Ages - DW",
                "+ NWFSC HKL Index")
mysummary <- SSsummarize(list(base_2021, dw_fishery_comps, 
                              crfs_pr_index, rov_dw,
                              ccfrp_comps_dw, ccfrp_index,
                              hkl_comps_dw, hkl_index))

SSplotComparisons(mysummary,
                  filenameprefix = "5_nwfsc_hkl",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#===============================================================================
# 6.+ Blocks
#===============================================================================

blocks <- SS_output(file.path(wd, "6.1_fleet_blocks"))

modelnames <- c("2021",
                "+ Fishery Lengths & Ages",
                "+ Fishery Indices",
                "+ ROV Index and Lengths",
                "+ CCFRP Index, Lengths, & Ages",
                "+ NWFSC HKL Index, Lengths, & Ages",
                "+ Blocks")
mysummary <- SSsummarize(list(base_2021, 
                              dw_fishery_comps, 
                              crfs_pr_index, 
                              rov_dw,
                              ccfrp_index,
                              hkl_index,
                              blocks))

SSplotComparisons(mysummary,
                  filenameprefix = "6_blocks",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


#===============================================================================
# 7.+ Rec Devs & Bias AdJ
#===============================================================================
recdevs <- SS_output(file.path(wd, "7.1_recdevs"))
recdevs <- SS_output(file.path(wd, "7.2_updated_bias_adj"))

pr_ages <- SS_output(file.path(wd, "7.3_rec_pr_ages"))

modelnames <- c("2021",
                "+ Blocks", "+ Rec. Bias Adj.", "+ Rec.PR Ages")
mysummary <- SSsummarize(list(base_2021, 
                              blocks, recdevs, pr_ages))

SSplotComparisons(mysummary,
                  filenameprefix = "7_recdevs",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#===============================================================================
# 8.+ Growth
#===============================================================================

growth <- SS_output(file.path(wd, "8.1_growth"))

modelnames <- c("2021",
                "+ Fleet Structure",
                "+ Update Biology",
                "+ Removals",
                "+ Fishery Lengths & Ages",
                "+ Fishery Indices",
                "+ ROV Index and Lengths",
                "+ CCFRP Index, Lengths, & Ages",
                "+ NWFSC HKL Index, Lengths, & Ages",
                "+ Blocks",
                "+ Rec. Devs.",
                "+ Growth Est.")

mysummary <- SSsummarize(list(base_2021, 
                              rec_fleet,
                              biology,
                              rec_catch,
                              dw_fishery_comps, 
                              crfs_pr_index, 
                              rov_dw,
                              ccfrp_index,
                              hkl_index,
                              blocks,
                              recdevs,
                              growth))

SSplotComparisons(mysummary,
                  filenameprefix = "8_growth",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

SSplotComparisons(mysummary,
                  subplots = c(2,4,12),
                  ylimAdj = 1.50, 
                  filenameprefix = "full_bridge_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  print = TRUE,
                  pdf = FALSE)


modelnames <- c("2021",
                "+ Fleet Structure",
                "+ Update Biology",
                "+ Removals",
                "+ Fishery Lengths & Ages",
                "+ Fishery Indices")

mysummary <- SSsummarize(list(base_2021, 
                              rec_fleet,
                              biology,
                              rec_catch,
                              dw_fishery_comps,
                              crfs_pr_index))

SSplotComparisons(mysummary,
                  subplots = c(2,4),
                  ylimAdj = 1.10, 
                  filenameprefix = "full_bridge_1_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  print = TRUE,
                  pdf = FALSE)

modelnames <- c("2021",
                "+ ROV Index and Lengths",
                "+ CCFRP Index, Lengths, & Ages",
                #"+ NWFSC HKL Index, Lengths, & Ages",
                "+ Blocks",
                "+ Rec. Devs.",
                "+ Growth Est.")

mysummary <- SSsummarize(list(base_2021, 
                              rov_dw,
                              ccfrp_index,
                              #hkl_index,
                              blocks,
                              recdevs,
                              growth))

SSplotComparisons(mysummary,
                  subplots = c(2,4),
                  ylimAdj = 1.10, 
                  filenameprefix = "full_bridge_2_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  print = TRUE,
                  pdf = FALSE)
