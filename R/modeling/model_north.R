###############################################
# Northern Modeling Explorations
################################################

library(r4ss)
area <- "nca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)

#=================================================================================
# 2021 Base Model 
#=================================================================================
base_2021 <- SS_output(file.path(wd, "_bridging", "00_2021_base"))

int_model <- SS_output(file.path(wd, "0.1_init_model"))

#=================================================================================
# Start at model 2.0
#=================================================================================
rm_low_samps <- SS_output(file.path(wd, "2.0_rm_ages_lowN"))

# Change the early CPFV ages from CAAL to marginals
early_cpfv_marg <- SS_output(file.path(wd, "2.1_cpfv_early_marginal_ages"))
get_model_quants(repfile = early_cpfv_marg)
SS_plots(early_cpfv_marg, plot = c(2:4, 16:18))
# NLL = 1376.53 Age NLL = 1022.48, R0 = 6.2
# Age_like: 1022.48  73.3583 0 415.437 83.6527 70.0301 0 0 0 379.999


# Add age-based selectivity to the growth fleet
growth_selex <- SS_output(file.path(wd, "2.2_growth_selex"))
get_model_quants(repfile = growth_selex)
SS_plots(growth_selex, plot = c(2, 16:18))
# NLL = 1379.93 Length NLL = 395.409, Age NLL = 1023.12, R0 = 6.2
# Age_like: 1023.12  73.3877 0 415.268 83.9536 70.0673 0 0 0 380.449
# Does not improve the fit to the age data but retain for now

com_asym <- SS_output(file.path(wd, "2.3_com_selex_fix_final"))
get_model_quants(repfile = com_asym)
# NLL = 1380.61 (delta + 0.68), Length NLL = 396.288 (delta + 0.88), Age NLL = 1022.93

# Fix some of the CPFV selectivity parameter 6 for some blocks
cpfv_selex <- SS_output(file.path(wd, "2.4_cpfv_selex_fix_final"))
SS_plots(cpfv_selex, plot = c(1:2, 16:18))
get_model_quants(repfile = cpfv_selex)
# NLL = 1380

# Fix some of the CPFV selectivity parameter 6 for some blocks
pr_selex <- SS_output(file.path(wd, "2.5_pr_selex_fix_final"))
SS_plots(pr_selex, plot = c(2, 16))
get_model_quants(repfile = pr_selex)
# Fix the final selectivity asymptotic
# NLL = 1379.81
# Fix both the final and the previous block asymptotic
# NLL = 1385.65
# Given the change in 6 NLL units, turning the desc and logit selectivity in the 2017 block back on
tune_comps(replist = pr_selex, dir = file.path(wd, "2.5_pr_selex_fix_final"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

# Revist the growth selectivity and compare the likelihood when fixing it to by asymptotic
# Growth selectivity being estimated  NLL = 1379.93
growth_asym <- SS_output(file.path(wd, "2.6_growth_selex_asm"))
get_model_quants(repfile = growth_asym)
SS_plots(growth_asym, plot = c(2,4))
# Estimate the init selectivity parameter in 2.7_growth_selex_init reduces the NLL to 1370.41 but
# much of the improvement is due to the kength fits
init_growth <- SS_output(file.path(wd, "2.7_growth_selex_init"))
SS_plots(init_growth, plot = c(2,4,18))
get_model_quants(repfile = init_growth)

# Look across model versions to date ===============================================================
mi_dw <- SS_output(file.path(wd, "3.0_mi_dw"))
francis_dw <- SS_output(file.path(wd, "3.0_francis_dw"))

modelnames <- c("2021", "Updated Data", "2.0 Rm. Low N", "Early Ages Marginals", "Selex Update", "Francis", "MI")
mysummary <- SSsummarize(list(base_2021, int_model, early_cpfv_marg, rm_low_samps, pr_selex, francis_dw, mi_dw))

SSplotComparisons(mysummary,
                  filenameprefix = "3_data_weights_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)
#===================================================================================================

# Updated the data-weights to those suggested by Francis
# Re-weighted the model eliminated the snake curve in the early devs but still has a run of
# positive devs in the early years pre-data
early_devs <- SS_output(file.path(wd, "3.1_fix_early_devs"))
SS_plots(early_devs, plot = c(2, 4, 16))
get_model_quants(repfile = early_dev)
# The early pattern is even more concerning

# Removal all of pre-MRFSS length data and the early CPFV ages
rm_early_data <- SS_output(file.path(wd, "3.2_early_dev_rm_early_data"))
SS_plots(rm_early_data, plot = c(2, 4, 16))
get_model_quants(repfile = rm_early_data)
# This does not help and actually makes later devs more extreme

# Return to fixing early devs but move it further up to 1970 where the devs seemed to actually be informed by data
late_dev_start <- SS_output(file.path(wd, "3.3_start_devs_later"))
SS_plots(late_dev_start, plot = c(2, 4, 16))
get_model_quants(repfile = late_dev_start)

# Keep late main start and turn early devs back on
main_devs <- SS_output(file.path(wd, "3.4_early_devs_late_main"))
SS_plots(main_devs, plot = c(2, 4, 16:18))
get_model_quants(repfile = main_devs)
# NLL = 948.266

# Look across model versions of the devation models ===============================================================

modelnames <- c("Francis",  "Rm. Early Data", "Turn off Early Devs.", "Devs. 1970", "Extend Early Devs.")
mysummary <- SSsummarize(list(francis_dw, rm_early_data, early_devs, late_dev_start, main_devs))

SSplotComparisons(mysummary,
                  filenameprefix = "3_recdevs_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#====================================================================================================================
# Returning to selectivity a bit to see how lack of blocks, settlement timing, and other items may impact the recent
# rec dev estimates.

# Add block back into the live fish fleet
com_live_block <- SS_output(file.path(wd, "3.5_add_com_live_block"))
SS_plots(com_live_block, plot = c(2, 4, 16:18))
get_model_quants(repfile = com_live_block)
# DON'T THINK THIS MODEL CONVERGED CORRECTLY
# Adding this back in to the model completely changes the early rec devs in a unexpected way. 
# They look much more informed rather than just having a string of positive rec devs but not
# clear how a fishery that started in 1994 could change these early devs.
# NLL = 943.753

com_live_block2 <- SS_output(file.path(wd, "3.6_live_block_des_final"))
SS_plots(com_live_block2, plot = c(2, 4, 16:18))
# NLL = 939 (pre-dw)
# NLL = 972 post-data-weighting
tune_comps(replist = com_live_block2, dir = file.path(wd, "3.6_live_block_des_final"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
# Re-weighted the model where the recommended weight for the live len increase to ~0.27 from 0.11
# The visual aggregated fit to the length comps looks worse rather than better but there is a 
# much higher effective sample size and the visual fit in the recent years is better.  
# Keep this but may circle back...

modelnames <- c("Francis",  "Extend Early Devs.", "Add Live Selex Bock")
mysummary <- SSsummarize(list(francis_dw, main_devs, com_live_block2))

SSplotComparisons(mysummary,
                  filenameprefix = "3_recdevs_com_live_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)
#===================================================================================================

# Revisit setting settlement to the first month based on experiences with the south model
settlement <- SS_output(file.path(wd, "3.7_settlement"))
SS_plots(settlement, plot = c(2, 4, 16:18))
get_model_quants(repfile = settlement)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   971.505   -46.8268    431.331 579.398 6.23084   440.564 338.203               0.767659
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                    0.108                 48.4248                    0.108               46.8342

settlement_init <- SS_output(file.path(wd, "3.8_settlement_age_selex_init"))
SS_plots(settlement_init, plot = c(2))
get_model_quants(repfile = settlement_init)
#Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   971.342   -46.7915     431.07 579.578 6.23285   441.034 339.135               0.768954
#Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                  48.401                    0.108               46.8433

#==================================================================================================
# Add new ROV length data
# This model removes the com live blocks and returns to a single estimated selectivity
# Switches the growth fleet to age selex 10 where selectivity = 1 for age 1+ which is what it
# was estimating anyway.
rov_len <- SS_output(file.path(wd, "4.0_rov_lengths"))
SS_plots(rov_len, plot = c(2, 16))
get_model_quants(repfile = rov_len)
tune_comps(replist = rov_len, dir = file.path(wd, "4.0_rov_lengths"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   994.343    -46.858    454.274 578.198 6.17494   439.384 340.548               0.775057
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 48.6065                    0.108               46.8975

# Allow the selectivity for this fleet to be domed
rov_selex <- SS_output(file.path(wd, "4.1_rov_selex"))
SS_plots(rov_selex, plot = c(2, 16))
get_model_quants(repfile = rov_selex)
# NLL = 993.936 Length NLL = 453.931

# Retain the selectivity from 4.1 and group the lengths into super years
rov_super_year <- SS_output(file.path(wd, "4.2_rov_selex_super_year"))
SS_plots(rov_super_year, plot = c(2, 16))
# NLL = 990.947
get_model_quants(repfile = rov_super_year)

# Combine the super period length together by hand - 
rov_super_year_alt <- SS_output(file.path(wd, "4.2_rov_selex_super_year_alt"))
SS_plots(rov_super_year_alt, plot = c(4))
get_model_quants(repfile = rov_super_year_alt)
# NLL = 991.3

# Re-data weight the model using Francis and keep the ROV length associated to the collection year
# with asymptotic selectivity for the ROV survey
update <- SS_output(file.path(wd, "5.0_updated_structure"))
SS_plots(update)
get_model_quants(repfile = update)
#Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#[1,]   970.679   -46.8706     414.35  595.02 6.17445   438.455 334.114               0.762025
#Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#[1,]                      0.108                 48.6395                    0.108               46.8255

# Allow the rec devs to not be zero-centered
non_centered_devs <- SS_output(file.path(wd, "5.1_non_zero_centered_devs"))
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   967.848   -47.3545    413.289 595.107 6.28569    488.87 267.773               0.547738
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 48.5925                    0.108               46.7988
SS_plots(non_centered_devs, plot = 4)
get_model_quants(repfile = non_centered_devs)

modelnames <- c("Updated 2023 Data","Rm. Low Sample Years", "Francis",  "Extend Early Devs.", "Updated ROV Data", "Non-Centered Devs." )
mysummary <- SSsummarize(list(int_model, rm_low_samps, francis_dw, main_devs, update, non_centered_devs))

SSplotComparisons(mysummary,
                  filenameprefix = "5_rov_non_centered_devs_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)
