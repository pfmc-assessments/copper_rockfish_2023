#######################################################################################################
# Copper Rockfish: North of Pt. Conception
# Run profiles, retrospectives, and jitters
#######################################################################################################

library(r4ss)
# Locatedi in the pfmc-assessments organization repo
# pak::pkg_install("pfmc-assessments/nwfscDiag")
library(nwfscDiag)
library(dplyr)


# Specify the directory
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

model_dir <- file.path(user_dir, "models", "nca")
# Specify why model you would like to profile, retro, and/or jitter
base_name <- "7.2_fix_desc_pr_2017"

# Specify the parameters and the space to profile
get = get_settings_profile(
  parameters =  c("L_at_Amin_Mal_GP_1", "L_at_Amax_Fem_GP_1",  "L_at_Amax_Mal_GP_1", "NatM_uniform_Fem_GP_1", "NatM_uniform_Mal_GP_1", "SR_BH_steep", "SR_LN(R0)"),
  low =  c(13, 44.0, 44.0, 0.08, 0.08, 0.30, -0.5),
  high = c(20, 52.0, 52.0, 0.14, 0.14, 0.95,  1.5),
  step_size = c(1, 1, 1, 0.01, 0.01, 0.05, 0.10),
  param_space = c('real','real', 'real', 'real', 'real', 'real', 'relative'),
  use_prior_like = c(0, 0, 0, 1, 1, 1, 0)
)

# This specifies to run ALL the diagnostics, if you want to do only some of them revise the "run" input line
model_settings = get_settings(
  settings = list(
  base_name = base_name,
  profile_details = get,
  run = "jitter", #c("profile",  "jitter", "retro"),
  retro_yrs = -1:-20,
  jitter_fraction = 0.10,
  Njitter = 20))

# Run line
run_diagnostics(mydir = model_dir, model_settings = model_settings)

