###############################################
# Southern Modeling Explorations
################################################

library(r4ss)
area <- "sca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)

# 2021 Base Model 
base_2021 <- SS_output(file.path(wd, "_bridging", "00_2021_base"))

int_model <- SS_output(file.path(wd, "0.1_init_model"))
# LIKELIHOOD report:2 2337.6


# Pin selectivity parameter 6 to asymptotic since it was being estimated asymptotic for all
# blocks except for the 2022 year which was estimating dome at the very largest sizes 52 cm+
# Remove the commercial live block in 2022 do lack of data in this single year to inform
# selectivity estimation
com_selex <- SS_output(file.path(wd, "0.2_com_selex"))
# NLL = 2339, R0 = 5.5
SS_plots(com_selex, plot = 2)

# The recreational block was accidently misspecified to be 2000-2001 rather than the correct 2000-2021.
rec_block <- SS_output(file.path(wd, "0.3_rec_blocks"))
# NLL = 2341
SS_plots(com_selex, plot = 2)

# Explore dome-shaped selectivity for the CCFRP survey to try to fit the length comps better
ccfrp_selex <- SS_output(file.path(wd, "0.4_ccfrp_selex"))
SS_plots(ccfrp_selex, plot = c(2,16))
# NLL = 2312, R0 = 5.5
# The selectivity is estimated to be domed and provides a improved fit to these data
# CCRFS Length NLL = 15.6154 vs the asymptotic NLL = 26.9081
# Fixing the descending selex parameter because it was being estimated near the bound

# Reduce the lower bound for the first selex parameter and turn on parameter 6 to explore dome-shaped selex
rov_selex <- SS_output(file.path(wd, "0.5_rov_selex"))
SS_plots(rov_selex, plot = c(2,16))
# NLL = 2310 (length NLL = 8.68692 vs. 10.9937 before), R0 = 5.5, depl = 0.16
# Female Linf = 47.0013000, Male Linf = 46.5


##################### Note Commercial dead selectivity for 2022 is unstable ############################
###    Removing that final block for now due to lack of comp data in 2022 may want to fix later    #####
com_block <- SS_output(file.path(wd, "0.6_rm_com_block"))
SS_plots(com_block, plot = c(2))
# NLL = 2308, R0 = 5.5, Linf F = 47.0

# Explore removing the 2014 ROV lengths due to small sample size and then lumping the later years data into
# a super year period
rov_super <- SS_output(file.path(wd, "0.7_rov_super_year"))
SS_plots(rov_super, plot = c(2,16))
# Data Removed NLL not comparable
# Removing the 2014 data and putting the 2019-2021 comps into a super period really changes the aggregated 
# distribution of the data (super period is more peaky), reduces the effective sample size of these data,
# and shifts selectivity. Not sure the best approach but am going to remove the super year period but still
# not use the 2014 data that has very small sample size

# Reweight the model based on the selectivity changes and also realized that the growth
# fleet ages were not being data weighted (good grief)
mi_dw <- SS_output(file.path(wd, "0.8_growth_dw_w_selex"))
SS_plots(mi_dw)


# Change both the commercial and cpfv caal ages to marginals
fishery_marginals <- SS_output(file.path(wd, "1.0_fishery_marginals"))
SS_plots(fishery_marginals, plot = c(17))
# NLL = 2401 (not comparible), Linf F = 46.8 cm M = 46.0 cm

# Apparently the growth ages were not data weighted - add dw and update all other weighting based on 
# switching the fishery ages to marginals.
growth_dw <- SS_output(file.path(wd, "1.1_dw_growth_ages"))
SS_plots(growth_dw, plot = c(17,18))
# NLL = 1998.4 (not comparable), R0 = 5.5, Linf F = 47.3, M = 45.7, depl = 0.15

# Switch ALL age data from CAAL to marginals and add each back one at a time to see how growth and recruitment
# estimates is being influenced by the data.
marginal <- SS_output(file.path(wd, "1.2_all_marginals"))
SS_plots(marginal, plot = c(1:4, 16, 17))
# NLL = 1232
# Depletion blows up 100% with R0 increasing to 5.9
# Linf F = 43.7 M  = 44.4

hkl_caal <- SS_output(file.path(wd, "1.3_nwfsc_hkl_caal"))
SS_plots(hkl_caal, plot = c(1:4, 16, 17, 18))
# NLL = 1842.9, Linf F = 47.2 M = 44.6, R0 = 5.8, depletion = 103%  
# Age_like: 1146.83  10.808 0 15.6455 0 23.6569 0 812.55 0 284.169

growth_caal <- SS_output(file.path(wd, "1.4_growth_caal"))
SS_plots(growth_caal, plot = c(1:4, 16, 17, 18))
# NLL = 1361, Linf F = 47.3 M = 44.5 cm, R0 = 5.5, depletion = 0.18
# Age_like: 706.315  11.267 0 14.8161 0 24.7724 0 217.627 0 437.833

# Also switched the L2 parameter to age 25 
hkl_growth_caal <- SS_output(file.path(wd, "1.5_hkl_growth_caal"))
SS_plots(hkl_growth_caal, plot = c(1:4, 16, 17, 18))
# NLL = 1965, Linf F = 47.7 M = 44.9, R0 = 5.5, depletion = 0.15
# Age_like  1.30822e+03     1.13890e+01     0.00000e+00 1.45253e+01  0.00000e+00 26.07630  0.00000  813.8390

# Remove the 2022 COOP ages to see the influence of these data
rm_coop_ages <- SS_output(file.path(wd, "1.6_rm_coop_caal"))
SS_plots(rm_coop_ages, plot = c(1:4, 16, 17, 18))
# If these CAAL are remove the recommended dw is increased from 0.37 to 0.57
# Linf F = 48.3, M = 45.3, R0 = 5.5, depl = 0.11

dw_caal <- SS_output(file.path(wd, "1.7_dw_caal"))
# Estimates of growth with only the Growth and NWFSC HKL ages as CAAL
# L_at_Amin_Fem_GP_1                                   11.4568000     
# L_at_Amax_Fem_GP_1                                   47.7577000     
# VonBert_K_Fem_GP_1                                    0.1762710     
# L_at_Amin_Mal_GP_1                                    7.6058900     
# L_at_Amax_Mal_GP_1                                   44.8907000     
# VonBert_K_Mal_GP_1                                    0.2414590    


#======================================================================================
# Do a quick progress comparison
#======================================================================================
modelnames <- c("2021", "All New Data", "Selectivity", "NWFSC HKL & Growth CAAL", "- 2022 Coop. Ages")
mysummary <- SSsummarize(list(base_2021, int_model,  mi_dw , dw_caal, rm_coop_ages))

SSplotComparisons(mysummary,
                  filenameprefix = "1_selex_caal_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#======================================================================================

tune_comps(replist = mi_dw, dir = file.path(wd, "2.0_mi_dw"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)

# All ages being used as conditional ages
mi_dw_2.0 <- SS_output(file.path(wd, "2.0_mi_dw"))
SS_plots(mi_dw_2.0)
# LIKELIHOOD report:2 2019.14
# Linf F = 48.1, M = 44.8 cm

frances_dw <- SS_output(file.path(wd, "2.1_frances_dw"))
SS_plots(frances_dw)
# Linf F = 47.2, M = 46.9 
# The dirichlet parameters are hitting the upper bounds - would need to revise
# the input sample size to be equal to the number of fish

dirichlet_dw <-  SS_output(file.path(wd, "2.2_dirichlet_dw"))
# Linf F = 46.9 cm, M = 46.6 

modelnames <- c("2021", "MI", "Frances", "Dirichlet (Param on Upper Bound)")
mysummary <- SSsummarize(list(base_2021, mi_dw_2.0, frances_dw, dirichlet_dw))

SSplotComparisons(mysummary,
                  filenameprefix = "2_dw_growth_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#===============================================================================
# Dig into growth options
#==============================================================================

L2 <-  SS_output(file.path(wd, "2.3_L2"))
get_model_quants(repfile = L2)
SS_plots(L2)
# NLL = 2018.4 vs. 2019 from 2.0_mi_dw model
# This does reduce the Linf estimate since it is now linked to the size at age 20
# where females are still reaching ~ 48 cm by age 50

sd_laa <- SS_output(file.path(wd, "2.4_sd_laa"))
SS_plots(sd_laa, plot = c(1, 4, 16))
# NLL > 4800

cv_age <- SS_output(file.path(wd, "2.5_cv_age"))
get_model_quants(repfile = cv_age)
# NLL = 2039 worse than the CV(LAA) model

growth_platoons <- SS_output(file.path(wd, "2.6_growth_platoons"))
get_model_quants(repfile = growth_platoons)
SS_plots(growth_platoons)

# Remove the unsexed CAAL for larger fish from the growth fleet and switch both
# of the fishery ages to marginals because fleet 1 only has a few ages in 2022
# and the early recreational ages are all unsexed.
ages <- SS_output(file.path(wd, "3.0_ages"))
SS_plots(ages, plot = c(2, 16:18))
get_model_quants(repfile = ages)
#Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#1986.81   -31.0703    681.264 1331.74  5.4943   204.393 31.4678               0.153957
#Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#      0.108                 46.8461                    0.108                 44.39

# Remove years by fleet where the numbr of fish sampled is < 10
rm_len <- SS_output(file.path(wd, "3.1_rm_low_n"))
SS_plots(rm_len, plot = c(2, 16:18))
get_model_quants(repfile = rm_len)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
# 1969.32   -31.0554    664.123 1331.19 5.49389    204.41 31.7731               0.155438
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 46.8494                    0.108               44.3858

# Increase the age that Lmin is estimated relative to try to improve stability of the model.
# Estiamte the hessian on this run to understand how the Lmin is being estimated
L1 <- SS_output(file.path(wd, "3.2_L1_age2"))
SS_plots(L1)
get_model_quants(repfile = L1)
#Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#  1971.49   -31.1485    665.649 1331.65 5.48879   203.238 34.3151               0.168842
#Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                    0.108                 46.6653                    0.108               44.4732

# Estimate a length based selectivity for the growth fleet
growth_len_sex <- SS_output(file.path(wd, "3.3_growth_len_selex"))
SS_plots(growth_len_sex, plot = c(1:2, 16:18))
get_model_quants(repfile = growth_len_sex)
# Selectivity estimated is highly domed on a narrow length range
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   1971.74   -31.1678    665.876  1331.7 5.48836   202.992 33.2028               0.163567
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 46.6698                    0.108                44.498

# Add age base selectivity for the growth fleet
growth_age_selex <- SS_output(file.path(wd, "3.4_growth_age_selex"))
SS_plots(growth_age_selex, plot = c(1:4, 16:18))
get_model_quants(repfile = growth_age_selex)
# Total NLL  Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   1996.21   -30.9827    667.066 1356.61 5.48922   206.492 28.5993               0.138501
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                     0.108                 46.5961                    0.108                44.386

growth_age_len <- SS_output(file.path(wd, "3.5_growth_len_age_selex"))
SS_plots(growth_age_len, plot = c(1:4, 17:20))
get_model_quants(repfile = growth_age_len)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#    1996.3   -30.9806    666.906 1356.34 5.49916   208.264 27.1163               0.130201
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 46.6741                    0.108                44.365

# Allow for the estimation of init selectivity for the growth fleet
growth_age <- SS_output(file.path(wd, "3.6_growth_age_selex_init_dw"))
get_model_quants(repfile = growth_age)
SS_plots(growth_age)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#    2094.6   -30.6148    806.764 1314.08 5.48157   202.188 33.2238               0.164321
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 46.5676                    0.108               44.4103

# Split out the 2022 ages into a separate fleet mirroring to the CPFV fleet selectivity
coop_selex <- SS_output(file.path(wd, "3.7_add_coop_fleet"))
get_model_quants(repfile = coop_selex)
SS_plots(coop_selex, plot = c(2, 17:20))
# Growth LL for growth & coop = 355.908 95.5284 = 451.4 (vs. 438.385 for selex = 1 and 426.26 for a single growth fleet)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
#   1983.05   -30.7875    663.524 1346.25 5.49045   202.447  35.057               0.173166
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male Length at Amax - Male
#                      0.108                 46.5478                    0.108               44.4322

coop_selex2 <- SS_output(file.path(wd, "3.8_coop_age_selex"))
get_model_quants(repfile = coop_selex2)
# NLL = 1959.3

coop_selex3 <- SS_output(file.path(wd, "3.9_coop_age_selex_dw"))
get_model_quants(repfile = coop_selex3)
SS_plots(coop_selex3)
# NLL = 2278.9

# Not a base but a clean starting point for additional explorations,
# incorporating selectivity explorations for the growth fleet where all
# growth are in a single fleet (coop not split out) and estimating age based selectivity.
base <- SS_output(file.path(wd, "4.0_base"))
SS_plots(base)
get_model_quants(base)
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
# 2094.57   -30.6142    806.744 1314.08 5.48159   202.203 33.2776               0.164575
# Natural Mortality - Female Length at Amax - Female Natural Mortality - Male
#                      0.108                 46.5675                    0.108
# Length at Amax - Male
#               44.4094

modelnames <- c("2021", "MI", "L1 = 2", "Coop. Selex", "Growth Age Selex", "L1, L2, & Growth Selex")
mysummary <- SSsummarize(list(base_2021, mi_dw_2.0, L1, coop_selex3, base))

SSplotComparisons(mysummary,
                  filenameprefix = "3_growth_selex_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


#========================================================================================
# Explore how to stabilize the commercial selectivity
#========================================================================================

# 4.0_base 
# Total NLL Survey NLL Length NLL Age NLL log(R0) SB Virgin SB 2023 Fraction Unfished 2023
# 2094.57   -30.6142    806.744 1314.08 5.48159   202.203 33.2776               0.164575

# Allow for dome-shaped selectivity in the early period
early_dome <- SS_output(file.path(wd, "4.1_est_com_selex6"))
SS_plots(early_dome, plot = 2)
get_model_quants(early_dome)
# NLL = 2095.22

# Remove the 2002-2016 block since it is being estimated essentially the same as the 
# early period for the commercial dead fishery
rm_com_block <- SS_output(file.path(wd, "4.2_rm_com_selex_early_block"))
SS_plots(rm_com_block, plot = 2)
get_model_quants(rm_com_block)
# NLL = 2094.82
20947.
# Phase 6 is sluggish in estimation turning off the forecast deviations
# Fix the ascending limb at the estimated value for the commercial dead fishery
com_asc <- SS_output(file.path(wd, "4.3_fix_com_asc_selex"))
SS_plots(com_asc)
get_model_quants(com_asc)
# NLL = 2094.87

rec_asc <- SS_output(file.path(wd, "4.4_fix_rec_2022_asc_desc"))
SS_plots(rec_asc)
get_model_quants(rec_asc)

# Start the model from the par file from the jitter run with the lowest NLL
mle <- SS_output(file.path(wd, "4.5_mle"))
get_model_quants(mle)
# NLL = 2094.63 R0 = 5.486 Depl = 0.166304

rov <- SS_output(file.path(wd, "5.0_update_rov_data"))
SS_plots(rov, plot = c(2, 16))
get_model_quants(rov)
# NLL = 2122.95

modelnames <- c("ROV 80/20", "ROV 73/27 & New Lengths")
mysummary <- SSsummarize(list(mle, rov))

SSplotComparisons(mysummary,
                  filenameprefix = "5_rov_newdata_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# Change the settlement timing to July - Marginal improvement in the fit to the length data ~ 1 NLL
# Significantly worse fit to the age data 1353 here vs. 1316 in the rov model
timing <- SS_output(file.path(wd, "5.1_settlement"))
get_model_quants(timing)
SS_plots(timing, plot = c(16,18, 19))
# NLL = 2158.65 R0 = 5.43268, Depl = 0.133

# Increase L1 to age 3 since the size at settlement is now mid year at a smaller size
L1_3 <- SS_output(file.path(wd, "5.2_L1=3"))
get_model_quants(L1_3)
SS_plots(L1_3, plot = c(1, 2, 16,18))
# NLL = 2184

# Remove any years with < 20 length samples from the fishing fleets - reweight the mode
# Apply Francis data weigths
rm_low_samps_dw <- SS_output(file.path(wd, "5.3_rm_low_samps"))
tune_comps(replist = rm_low_samps_dw, dir = file.path(wd, "5.3_rm_low_samps"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
get_model_quants(rm_low_samps_dw)
# NLL = 2290.72, R0 = 5.424, Depl = 0.197

# Fixing the value of Lmin and CV1 for each sex to try to improve model stability
fix_L1 <- SS_output(file.path(wd, "5.4_fix_L1_CV1"))
get_model_quants(fix_L1)
SS_plots(fix_L1)
# NLL = 2196.06 R0 = 5.48 Depl = 0.20

# Explore estimation of M where male is set to equal female M
est_m <- SS_output(file.path(wd, "5.5_est_m"))
SS_plots(est_m)
get_model_quants(est_m)
tune_comps(replist = est_m, dir = file.path(wd, "5.5_est_m"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
# NLL = 2139.24 R0 = 5.6, Depl = 0.293, Linf F = 46.7
# Natural Mortality F =  0.117 M = 0.129 

modelnames <- c("MLE", "ROV 73/27 & New Lengths", "Rm < 20 Samps", "Fix L1 & CV1", "Est. M")
mysummary <- SSsummarize(list(mle, rov, rm_low_samps_dw, fix_L1, est_m))

SSplotComparisons(mysummary,
                  filenameprefix = "5_fixed_est_parameters_",
                  legendlabels = modelnames, 	
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

fix_selex <- SS_output(file.path(wd, "5.7_fix_asc_desc"))
get_model_quants(fix_selex)
# NLL = 2141
rm_block <- SS_output(file.path(wd, "5.8_rm_2022_rec_block"))
get_model_quants(rm_block)
# NLL = 2142.73with 7 fewer parameters

modelnames <- c("5.7", "Remove CPFV/PR 2022 Block")
mysummary <- SSsummarize(list(fix_selex, rm_block))

SSplotComparisons(mysummary,
                  filenameprefix = "5.7_rm_rec_block_",
                  legendlabels = modelnames, 
                  endyr = 2034,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

exe <- SS_output(file.path(wd, "6.0_exe"))
SS_plots(exe,aalyear = 2003:2022)

modelnames <- c("5.8 Rec. Simple Blocks", "3.30.21")
mysummary <- SSsummarize(list(rm_block, exe))

SSplotComparisons(mysummary,
                  filenameprefix = "6.0_exe_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

non_zero_centered <- SS_output(file.path(wd, "6.1_non_zero_centered_devs"))
SS_plots(non_zero_centered, plot = 4)

modelnames <- c("6.0 Zero-Centered Devs", "6.1 Non-zero-Centered Devs.")
mysummary <- SSsummarize(list(exe, non_zero_centered))

SSplotComparisons(mysummary,
                  filenameprefix = "6.1_centered_devs_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

update_pr_index <- SS_output(file.path(wd, "6.2_update_pr_index"))
modelnames <- c( "6.1 Non-zero-Centered Devs.", "6.2 Update PR Index")
mysummary <- SSsummarize(list(non_zero_centered, update_pr_index))

SSplotComparisons(mysummary,
                  filenameprefix = "6.2_update_pr_index",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


coop_wcgbt <- SS_output(file.path(wd, "6.3_coop_wcgbt"))
SS_plots(coop_wcgbt, aalyear = 2003:2022, plot = c(2, 16:18))
tune_comps(replist = coop_wcgbt, dir = file.path(wd, "6.3_coop_wcgbt"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)


modelnames <- c("6.2 Update PR Index", "6.3 Seperate COOP & WCGBT")
mysummary <- SSsummarize(list(update_pr_index, coop_wcgbt))
SSplotComparisons(mysummary,
                  filenameprefix = "6.3_separate_coop_wcgbt",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

pearson <- SS_output(file.path(wd, "6.4_pearson"))
SS_plots(pearson, plot = 2)
tune_comps(replist = pearson, dir = file.path(wd, "6.4_pearson"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)

modelnames <- c("6.2 Update PR Index", "6.3 Seperate COOP & WCGBT", "6.4 Add Pearson Data")
mysummary <- SSsummarize(list(update_pr_index, coop_wcgbt, pearson))
SSplotComparisons(mysummary,
                  filenameprefix = "6.4_separate_ages_all_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

coop_selex <- SS_output(file.path(wd, "6.5_coop_len_selex"))
SS_plots(coop_selex)
tune_comps(replist = coop_selex, dir = file.path(wd, "6.5_coop_len_selex"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)

modelnames <- c("6.2 Update PR Index", "6.3 Seperate COOP & WCGBT", "6.4 Add Pearson Data", "6.5 COOP Selex")
mysummary <- SSsummarize(list(update_pr_index, coop_wcgbt, pearson, coop_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "6.5_separate_ages_all_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


rm_early_ages <- SS_output(file.path(wd, "6.6_rm_hist_ages"))
SS_plots(rm_early_lens, plot = 4)
get_model_quants(rm_early_lens)
# After exploration it appears that both the early recreational CPFV lengths and these historical
# ages are contributing to that giant recruitment. If you remove both the early length and age data
# the large recruitment goes away and if you just remove the ages there is still a peak but less so and spread
# across multiple years in the late 60s.
rm_early_data <- SS_output(file.path(wd, "6.6_rm_hist_ages_lens"))

modelnames <- c("6.5 COOP Selex", "Remove pre-MRFSS Lengths & Ages", "Remove Only Ages")
mysummary <- SSsummarize(list(coop_selex, rm_early_data, rm_early_ages))
SSplotComparisons(mysummary,
                  filenameprefix = "6.6_rm_early_data_",
                  ylimAdj = 1.25,
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# Increased the sigmaR to 0.70 which then revises the suggested sigmaR back to 0.60
sigmaR <- SS_output(file.path(wd, "6.7_sigmaR"))
SS_plots(sigmaR, plot = 4)

# This feels like a better end place for those early devs.
early_devs <- SS_output(file.path(wd, "6.8_change_early_devs"))
SS_plots(early_devs, plot = 4)
tune_comps(replist = early_devs, dir = file.path(wd, "6.8_change_early_devs"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)


mi <- SS_output(file.path(wd, "6.9_early_devs_mi"))
francis <- SS_output(file.path(wd, "6.10_early_devs_francis"))

modelnames <- c("6.5 COOP Selex", "MI", "Francis")
mysummary <- SSsummarize(list(coop_selex, mi, francis))
SSplotComparisons(mysummary,
                  filenameprefix = "6.9_data_weight_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

wcgbt_lens <- SS_output(file.path(wd, "6.11_unexpanded_wcgbt_lens"))
SS_plots(wcgbt_lens, plot = c(2, 16:18))
tune_comps(replist = wcgbt_lens, dir = file.path(wd, "6.11_unexpanded_wcgbt_lens"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)
modelnames <- c("6.5 COOP Selex", "MI", "Francis", "Update WCGBT Lengths - Francis")
mysummary <- SSsummarize(list(coop_selex, mi, francis, wcgbt_lens))
SSplotComparisons(mysummary,
                  filenameprefix = "6.11_wcgbt_lengths_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

mi <- SS_output(file.path(wd, "7.0_mi"))
tune_comps(replist = mi, dir = file.path(wd, "7.0_mi"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)

tune_comps(replist = francis, dir = file.path(wd, "7.0_francis"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)


# Reset to model 6.3 with the update PR index and add 1965 of main devs
model = SS_output(file.path(wd, "8.0_pr_index_early_devs"))
tune_comps(replist = model, dir = file.path(wd, "8.0_pr_index_early_devs"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)
SS_plots(model)

centered_devs = SS_output(file.path(wd, "8.1_centered_devs"))
tune_comps(replist = centered_devs, dir = file.path(wd, "8.1_centered_devs"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)
SS_plots(centered_devs)

modelnames <- c("8.0 Non-centered devs.", "8.1 Centered devs.")
mysummary <- SSsummarize(list(model, centered_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "8.0_devs_combined_growth_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)