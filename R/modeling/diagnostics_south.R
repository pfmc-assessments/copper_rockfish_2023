#######################################################################################################
# Copper Rockfish: South of Pt. Conception
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

model_dir <- file.path(user_dir, "models", "sca")
# Specify why model you would like to profile, retro, and/or jitter
base_name <- "13.2_cpfv_dome_dw"

# Specify the parameters and the space to profile
get = get_settings_profile(
  parameters =  c("NatM_uniform_Fem_GP_1", "NatM_uniform_Mal_GP_1", "SR_BH_steep", "SR_LN(R0)"),
  low =  c(0.09, 0.10, 0.30, -0.50),
  high = c(0.14, 0.15, 0.95,  0.70),
  step_size = c(0.005, 0.005, 0.05, 0.10),
  param_space = c('real', 'real', 'real', 'relative'),
  use_prior_like = c(1, 1, 1, 0)
)

# get = get_settings_profile(
#   parameters =  c("L_at_Amin_Fem_GP_1", "L_at_Amin_Mal_GP_1", "NatM_uniform_Fem_GP_1",
#                    "Size_DblN_peak_Commercial_dead(1)"),
#   low =  c(14, 14, 0.08, 38),
#   high = c(20, 20, 0.13, 47),
#   step_size = c(0.5, 0.5,  0.01, 1),
#   param_space = c('real', 'real', 'real', 'real'),
#   use_prior_like = c(0, 0, 0, 0)
# )
# 
# get = get_settings_profile(
#   parameters =  c("SR_LN(R0)"),
#   low =  c(5),
#   high = c( 6.5),
#   step_size = c( 0.25),
#   param_space = c( 'real'),
#   use_prior_like = c( 0)
# )

# This specifies to run ALL the diagnostics, if you want to do only some of them revise the "run" input line
model_settings = get_settings(
  settings = list(
    base_name = base_name,
    profile_details = get,
    run =  c("profile", "retro", "jitter"), 
    retro_yrs = -1:-5,
    Njitter = 50,
    jitter_fraction = 0.10))

# Run line
run_diagnostics(mydir = model_dir, model_settings = model_settings)

#==============================================================================
# Rerun a profile value
#==============================================================================
rerun_profile_vals(mydir = file.path(model_dir, base_name),
           model_settings = model_settings,
           para_name =  "NatM_uniform_Mal_GP_1",
           run_num = c(3,4),
           data_file_nm = "2023_ca_s_copper.dat")


library(ss3diags)
base_name <- base_name
model <- SS_output(file.path(model_dir, base_name))

# Runs Test
dir.create(file.path(model_dir, base_name, "runs_test"))
ss3diags::SSplotRunstest(
  model, 
  plotdir = file.path(model_dir, base_name, "runs_test"),
  print = TRUE)
sspar(mfrow = c(3, 2), plot.cex = 0.8)
ss3diags::SSplotRunstest(
  model,
  add = TRUE,
  verbose = FALSE,
  plotdir = file.path(model_dir, base_name, "runs_test"),
  print = TRUE)

sspar(mfrow = c(1, 1), plot.cex = 0.8)
SSplotJABBAres(model, add = TRUE, subplot = 'len')

load(file.path(model_dir, paste0(base_model,"_retro"), "retro_output.Rdata"))
sspar(mfrow = c(3, 2), plot.cex = 0.8)
SSplotHCxval(
  retroSummary, 
  add = TRUE, 
  verbose = F, 
  ylimAdj = 1.3, 
  legendcex = 0.7)

sspar(mfrow = c(1, 1), plot.cex = 0.7)
mvln = SSdeltaMVLN(model, run = "SMA")
sspar(mfrow = c(3, 2), plot.cex = 0.7)
SSplotEnsemble(mvln$kb, ylabs = mvln$labels, add = T, verbose = F)



