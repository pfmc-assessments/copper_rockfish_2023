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
base_north = "10.0_north_post_star_base"
base_south = "15.0_south_post_star_base"


# Approach #1----------------------------------------------------------------
# This first approach finds the 12.5 and 87.5 quantiles of the SSB_2023 estimate
# based on the within model estimated uncertainty

north = SS_output(file.path(wd, "nca", "_decision_table", base_north))
north_sb = north$derived_quants[north$derived_quants$Label == "SSB_2023", ]

sigma <- north$Pstar_sigma # 0.313
# Based on the SB sigma the low M = xx with a SB = 145.6
# Based on the SB sigma the high M = with a SB = 299.3
# Unable to find a high M to match the upper SB estimates

ofl_sigma <- north$OFL_sigma # 0.30
north_sb[,"Value"]/(exp(-1.15*ofl_sigma)); north_sb[,"Value"]/(exp(1.15*ofl_sigma))
# SB low = 147.9, SB = 294.7
# Look at steepness values instead, based on the profiles:
# pre-star base h =  0.637 and h = 0.892
# post-star base h high = 0.859 and h low of = 0.655

find_para(dir = file.path(wd, "nca", "_decision_table", base_north), 
          base = north, 
          yr = 2023, 
          parm = c("SR_parm[2]"), 
          quant = c(0.875), 
          est = FALSE, 
          sigma = round(ofl_sigma, 3), 
          tol = 0.005, use_115 = TRUE)

lo <- SS_output(file.path(wd, "nca", "_decision_table", "10.0_north_post_star_base_SR_parm[2]_decision_table_1.15_0.3_0.125"))
hi <- SS_output(file.path(wd, "nca", "_decision_table", "10.0_north_post_star_base_SR_parm[2]_decision_table_1.15_0.3_0.875"))


modelnames <- c("Base Model: h = 0.72", "Low State of Nature: h = 0.655", "High State of Nature: h = 0.859")
mysummary <- SSsummarize(list(north, lo, hi))

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
sigma <- south$Pstar_sigma # 0.30
ofl_sigma <- south$OFL_sigma # 0.277
south_sb[,"Value"]/(exp(-1.15*ofl_sigma)); south_sb[,"Value"]/(exp(1.15*ofl_sigma))

# SB low = 23.3, SB = 44.103
# Look at steepness values instead, based on the profiles:
# Could not find a h that created the low state of nature 
# Pre-STAR Base SB high = 45.6 with an h = 0.93
# Post-STAR Base h low = none and h high = 0.929 (same as before) 
# An h of 0.54 produces the lowest spawning output in the final model years

find_para(dir = file.path(wd, "sca", "_decision_table", base_south), 
          base = south, 
          yr = 2023, 
          parm = c("SR_parm[2]"), 
          quant = c(0.875, 0.125), 
          est = FALSE, 
          sigma = round(ofl_sigma, 3), 
          tol = 0.005, use_115 = TRUE)


lo <- SS_output(file.path(wd, "sca", "_decision_table", "15.0_south_post_star_base_SR_parm[2]_decision_table_1.15_0.277_0.125"))
hi <- SS_output(file.path(wd, "sca", "_decision_table", "15.0_south_post_star_base_SR_parm[2]_decision_table_1.15_0.277_0.875"))


modelnames <- c("Base Model: h = 0.72", "Low State of Nature: h = 0.54", "High State of Nature: h = 0.929")
mysummary <- SSsummarize(list(south, lo, hi))

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
