################################################
# Model Bridging
################################################

library(r4ss)
area <- "sca"
# area <- "nca"
wd <- paste0("C:/Assessments/2023/copper_rockfish_2023/models/", area,"/_bridging")

base_2021 <- SS_output(file.path(wd, "0_2021_base"))

ss_exe <- SS_output(file.path(wd, "0_ss_exe"))

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