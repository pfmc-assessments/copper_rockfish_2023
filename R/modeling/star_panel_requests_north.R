###############################################
# STAR Panel Requests: South of Point Conception
################################################

library(r4ss)
area <- "nca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)

base <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch"))

# Maturity 
or_mat <- SS_output(file.path(wd, "_sensitivities", "9.12_revised_sebastes_2021_catch_mat_or_est"))

modelnames <- c("Base", "Oregon Maturity")
mysummary <- SSsummarize(list(base, or_mat))

SSplotComparisons(mysummary,
                  filenameprefix = "request_1_maturity_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  subplot = c(2, 4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

# STAR Panel Request 3: Sigma R
sigmar <- SS_output(file.path(wd, "_sensitivities", "9.12_revised_sebastes_2021_catch_sigmaR_1.0"))
tune_comps(replist = sigmar, dir = file.path(wd, "_sensitivities", "9.12_revised_sebastes_2021_catch_sigmaR_1.0"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
sigmar_dw <- SS_output(file.path(wd, "_sensitivities", "9.12_revised_sebastes_2021_catch_sigmaR_1.0_dw"))
centered <- SS_output(file.path(wd, "_sensitivities", "9.12_revised_sebastes_2021_catch_sigmaR_1.0_dw_rec_opt_1"))


modelnames <- c("Base", "SigmaR = 1.0",  "SigmaR = 1.0 Reweighted")
mysummary <- SSsummarize(list(base, sigmar, sigmar_dw))

SSplotComparisons(mysummary,
                  filenameprefix = "request_3_north_sigmaR_dw_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  btarg = -1,
                  minbthresh = -1,
                  pdf = TRUE)

growth_selex <- SS_output(file.path(wd, "9.13_star_request_2"))


modelnames <- c("Base", "Growth Selectivity age 6+ = 1")
mysummary <- SSsummarize(list(base, growth_selex))

SSplotComparisons(mysummary,
                  filenameprefix = "request_2_north_growth_selex_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  subplot = c(2, 4, 10, 12), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

ccfrp_block_dw <- SS_output(file.path(wd, "9.13_star_request_4"))
tune_comps(replist = ccrp_split, dir = file.path(wd, "9.13_star_request_4b"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

ccfrp_split_dw <- SS_output(file.path(wd, "9.13_star_request_4b_dw"))


modelnames <- c("Base", "CCFRP Block in Q")
mysummary <- SSsummarize(list(base, ccfrp_block_dw))

SSplotComparisons(mysummary,
                  filenameprefix = "request_5_ccfrp_block_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  #subplot = c(2, 4, 10, 12), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)
                  #pdf = TRUE)
