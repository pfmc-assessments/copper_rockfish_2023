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
