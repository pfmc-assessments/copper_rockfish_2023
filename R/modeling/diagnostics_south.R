#######################################################################################################
# Copper Rockfish: South of Pt. Conception
# Run profiles, retrospectives, and jitters
#######################################################################################################

library(r4ss)
# Locatedi in the pfmc-assessments organization repo
# pak::pkg_install("pfmc-assessments/nwfscDiag")
library(nwfscDiag)

# Specify the directory
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

model_dir <- file.path(user_dir, "models", "sca")
# Specify why model you would like to profile, retro, and/or jitter
base_name <- "2.0_mi_dw"

# Specify the parameters and the space to profile
get = get_settings_profile(
  parameters =  c( "L_at_Amax_Fem_GP_1",  "L_at_Amax_Mal_GP_1", "NatM_uniform_Fem_GP_1", "SR_BH_steep", "SR_LN(R0)"),
  low =  c( 43.0, 43.0, 0.08, 0.30, -0.5),
  high = c( 52.0, 52.0, 0.14, 0.95,  1.5),
  step_size = c(0.5, 0.5, 0.005, 0.05, 0.10),
  param_space = c('real', 'real', 'real', 'real', 'relative'),
  use_prior_like = c(0, 0, 0, 0, 0)
)

# This specifies to run ALL the diagnostics, if you want to do only some of them revise the "run" input line
model_settings = get_settings(
  settings = list(
    base_name = base_name,
    profile_details = get,
    run = c("profile"), #"retro"), #, "jitter"),
    retro_yrs = -1:-10,
    jitter_fraction = 0.10))

# Run line
run_diagnostics(mydir = model_dir, model_settings = model_settings)


