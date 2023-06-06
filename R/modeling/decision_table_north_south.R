#######################################################################################################
#
# 			Decision Table: Copper Rockfish North of Point Conception
#
#######################################################################################################

library(r4ss)

# Specify the directory
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

source(file.path(user_dir, "R", "modeling", "decision_table_solver.R"))

wd = file.path(user_dir, "models")
base_north = "9.11_revised_pre-star_base"
base_south = "14.4_revised_pre-star_base_converged"


# Approach #1----------------------------------------------------------------
# This first approach finds the 12.5 and 87.5 quantiles of the SSB_2023 estimate
# based on the within model estimated uncertainty

north = SS_output(file.path(wd, "nca", "_decision_table", base_north))
north_sb = north$derived_quants[north$derived_quants$Label == "SSB_2023", ]

sigma <- north$Pstar_sigma # 0.273
# Based on the SB sigma the low M = 0.084 with a SB = 181.14
# Based on the SB sigma the high M = with a SB = 336.7
# Unable to find a high M to match the upper SB estimates

ofl_sigma <- north$OFL_sigma # 0.262
north_sb[,"Value"]/(exp(-1.15*ofl_sigma)); north_sb[,"Value"]/(exp(1.15*ofl_sigma))
# SB low = 182.5278, SB = 333.8
# Look at steepness values instead, based on the profiles:
# h =  0.637 and h = 0.892

find_para(dir = file.path(wd, "nca", "_decision_table", base_north), 
          base = north, 
          yr = 2023, 
          parm = c("SR_parm[2]"), 
          quant = c(0.875), 
          est = FALSE, 
          sigma = round(ofl_sigma, 3), 
          tol = 0.005, use_115 = TRUE)

low <- SS_output(file.path(wd, "nca", "_decision_table", "9.11_revised_pre-star_base_SR_parm[2]_decision_table_1.15_0.262_0.125"))
hi <- SS_output(file.path(wd, "nca", "_decision_table", "9.11_revised_pre-star_base_SR_parm[2]_decision_table_1.15_0.262_0.875"))


modelnames <- c("h = 0.72", " h = 0.637", "h = 0.892")
mysummary <- SSsummarize(list(north, low, hi))

SSplotComparisons(mysummary, 
                  endyrvec = 2023, 
                  legendlabels = modelnames, 
                  plotdir = file.path(wd, "nca", "_decision_table", '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_north, "_h_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)


#find_para(dir = file.path(wd, "nca", "_decision_table", base_north), 
#          base = north, 
#          yr = 2023, 
#          parm = "MGparm[1]", 
#          quant = c(0.125, 0.875), 
#          ctl_name = "2023_ca_n_copper.ctl", 
#          parm_string = "NatM_uniform_Fem_GP_1", 
#          est = FALSE, 
#          sigma = round(ofl_sigma, 3),
#          tol = 0.005, 
#          use_115 = TRUE)

#===================================================================================
# South
#===================================================================================

south = SS_output(file.path(wd, "sca", "_decision_table", base_south))
south_sb = south$derived_quants[south$derived_quants$Label == "SSB_2023", ]
sb_cv <- south_sb[,3] / south_sb[,2] #0.306
sigma <- south$Pstar_sigma # 0.299
ofl_sigma <- south$OFL_sigma # 0.275
south_sb[,"Value"]/(exp(-1.15*ofl_sigma)); south_sb[,"Value"]/(exp(1.15*ofl_sigma))

# SB low = 24.3, SB = 45.6
# Look at steepness values instead, based on the profiles:
# Could not find a h that created the low state of nature 
# SB high = 45.6 with an h = 0.93


find_para(dir = file.path(wd, "sca", "_decision_table", base_south), 
          base = south, 
          yr = 2023, 
          parm = c("SR_parm[2]"), 
          quant = c(0.875), 
          est = FALSE, 
          sigma = round(ofl_sigma, 3), 
          tol = 0.005, use_115 = TRUE)


low <- SS_output(file.path(wd, "sca", "_decision_table", "14.4_revised_pre-star_base_converged_SR_parm[2]_decision_table_1.15_0.275_0.125"))
hi <- SS_output(file.path(wd, "sca", "_decision_table", "14.4_revised_pre-star_base_converged_SR_parm[2]_decision_table_1.15_0.275_0.875"))


modelnames <- c("h = 0.72", " h = 0.637", "h = 0.93")
mysummary <- SSsummarize(list(south, low, hi))

SSplotComparisons(mysummary, 
                  endyrvec = 2023, 
                  legendlabels = modelnames, 
                  plotdir = file.path(wd, "sca", "_decision_table", '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_south, "_h_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)
