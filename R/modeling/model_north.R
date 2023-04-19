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

non_centered_devs_fix_selex <- SS_output(file.path(wd, "5.2_non_zero_centered_dev_fix_some_selex"))
SS_plots(non_centered_devs_fix_selex)

# Add the COOP CPFV carcass marginal ages
add_carcass_ages <- SS_output(file.path(wd, "5.3_add_carcass_marginals"))
SS_plots(add_carcass_ages)
# Add new CAAL plots from r4ss
SS_plots(add_carcass_ages, aalyear = 2022, plot = 18)
get_model_quants(repfile = add_carcass_ages)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   970.894   -47.4422     413.38 598.117 6.28448   488.226 267.073               0.547027
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                    0.108                 48.5926                    0.108               46.7995

# Add the COOP CPFV length data
add_coop_lens <- SS_output(file.path(wd, "5.4_add_coop_lengths"))
SS_plots(add_coop_lens, plot = c(2, 16))
get_model_quants(repfile = add_coop_lens)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   977.565   -47.4746    422.547 595.619 6.28532   488.814 266.712                0.54563
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 48.5938                    0.108               46.7474
tune_comps(replist = add_coop_lens, dir = file.path(wd, "5.4_add_coop_lengths"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

# Add new CAAL plots from r4ss
dw <- SS_output(file.path(wd, "5.5_francis_dw"))
SS_plots(dw)
SS_plots(dw, aalyear = 2022, plot = 18)
SS_plots(dw, aalyear = c(2019:2022), aalbin = c(38, 40))
get_model_quants(repfile = dw)
# NLL =  952.144   Survey = -47.4459    Length = 402.232 Age = 590.974 R0 = 6.28979

modelnames <- c("Non-Centered Devs.", "Add CPFV-Coop. Carc. Marginals", "Add CPFV-Coop. Lengths", "Francis DW")
mysummary <- SSsummarize(list(non_centered_devs, add_carcass_ages, add_coop_lens, dw))

SSplotComparisons(mysummary,
                  filenameprefix = "5_add_coop_data_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# I don't think is appropriate since the index was calculated as one time-series but just looking at the
# impact of splitting the index.
q_block <- SS_output(file.path(wd, "6.0_ccrfp_q_time_block"))
SS_plots(q_block)
get_model_quants(repfile = q_block)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   950.023   -49.3189    402.466 590.816 6.26956   480.537 244.357               0.508509
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 48.5708                    0.108               46.7397

# F-type option 4 - just made guess values for the init F values, should be refined
f_type <- SS_output(file.path(wd, "6.1_f_type"))
SS_plots(f_type)
get_model_quants(repfile = f_type)
# NLL = 958.3 with Fs estimated for all fleets

# Revisit the CCFRP selectivity
# Don't block the peak and ascending 
ccfrp_selex <- SS_output(file.path(wd, "6.2_ccfrp_selex"))
SS_plots(ccfrp_selex)
get_model_quants(repfile = ccfrp_selex)
# NLL = 952.099 vs. 952.144 from the dw model

# Lower sigma R based on suggested values
sigmaR <- SS_output(file.path(wd, "6.3_tune_sigmaR"))
SS_plots(sigmaR, plot = 4)
get_model_quants(repfile = sigmaR)
# The new suggested values are even lower from 0.45 to now 0.42 but minor
# NLL = 956.1

# Add back in the come live block on select to see how the rec devs  change
# The fit to all the length data improves by ~ 8 units however the visual to the 
# aggregated length comps does not look better has a higher effN
live_selex <- SS_output(file.path(wd, "6.4_com_live_selex"))
SS_plots(live_selex, plot = c(2, 4, 16))
# NLL  = 948.062   -47.2889    396.267 592.635 R0 =  6.27617   depl = 0.558296
get_model_quants(repfile = live_selex)
tune_comps(replist = live_selex, dir = file.path(wd, "6.4_com_live_selex"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

selex_francis_dw <- SS_output(file.path(wd, "6.5_francis_dw"))

modelnames <- c("5.5 Francis DW", "CCFRP Selex", "SigmaR = 0.5",  "Com. Live Selex", "Updated DW")
mysummary <- SSsummarize(list(dw, ccfrp_selex, sigmaR, live_selex, selex_francis_dw))

SSplotComparisons(mysummary,
                  filenameprefix = "6_selex_",
                  legendlabels = modelnames, 
                  legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# 6.6 Move 2020 in negative phase forecast, pin hist. com live selex asmptotic
# Estimate box sex M values
est_m <- SS_output(file.path(wd, "6.6_est_m"))
get_model_quants(repfile = est_m)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#  939.193   -49.9995    411.085  577.33 5.66394   610.167 136.352               0.223466
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male
#                 0.0682519                 48.3917                0.0731751
# Length at Amax - Male
#              46.5975

est_h <- SS_output(file.path(wd, "6.7_est_h"))
get_model_quants(repfile = est_h)
# NLL 948.231


modelnames <- c("6.5 Francis DW", "Est. M", "Est. h")
mysummary <- SSsummarize(list(selex_francis_dw, est_m, est_h))

SSplotComparisons(mysummary,
                  filenameprefix = "6_est_m_h_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

dw <- SS_output(file.path(wd, "6.8"))
SS_plots(dw)
get_model_quants(repfile = dw)
tune_comps(replist = dw, dir = file.path(wd, "6.8"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

lambda_growth <- SS_output(file.path(wd, "6.9_lambda_growth"))
SS_plots(lambda_growth)
lambda_pr <- SS_output(file.path(wd, "6.10_lambda_pr_len_ages"))
SS_plots(lambda_pr)
lambda_cpfv <- SS_output(file.path(wd, "6.11_lambda_cpfv_len_ages"))
SS_plots(lambda_cpfv)

modelnames <- c("6.6", "Lambda Growth", "Lambda PR", "Lambda CPFV")
mysummary <- SSsummarize(list(dw, lambda_growth, lambda_pr, lambda_cpfv))

SSplotComparisons(mysummary,
                  filenameprefix = "6_lambda_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# Decrease the min value for the peak parameter and allow the init selec (para 5)
# to be estimated.
rov <- SS_output(file.path(wd, "6.12_rov_selex"))
SS_plots(rov, plot = c(2, 16))
get_model_quants(repfile = rov)
# NLL = 948.308 Length = 414.43

# Grab the MLE par file from jitter and rerun
mle <- SS_output(file.path(wd, "7.0_mle"))
get_model_quants(repfile = mle)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   947.967   -47.3407    414.311 574.992 6.28443   490.505 280.495               0.571849

settlement <- SS_output(file.path(wd, "7.1_fix_lmin_settlement"))
get_model_quants(repfile = settlement)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   947.706   -47.3337    413.578 575.516 6.33266   487.716 276.289               0.566496

# Fix the ascending parameter in the PR fleet in the 2017 block
fix_selex <- SS_output(file.path(wd, "7.2_fix_desc_pr_2017"))
get_model_quants(repfile = fix_selex)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   947.212   -47.3987    413.628 575.161 6.33682   489.601 278.766               0.569373

modelnames <- c("6.6", "7.0 MLE", "Settlement & Lmin", "Fix PR Asc. 2017")
mysummary <- SSsummarize(list(dw, mle, settlement, fix_selex))

SSplotComparisons(mysummary,
                  filenameprefix = "7_mle_fix_params_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


rm_mrfss_cpfv <- SS_output(file.path(wd, "7.3_rm_mrfss_cpfv_data"))
get_model_quants(repfile = rm_mrfss_cpfv)
SS_plots(rm_mrfss_cpfv)

modelnames <- c("7.3", "Remove MRFSS Era CPFV Data")
mysummary <- SSsummarize(list(fix_selex, rm_mrfss_cpfv))

SSplotComparisons(mysummary,
                  filenameprefix = "7.3_rm_mrfss_data_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

