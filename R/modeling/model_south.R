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

sigmaR <- SS_output(file.path(wd, "8.2_sigmaR_70"))
tune_comps(replist = sigmaR, dir = file.path(wd, "8.2_sigmaR_70"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)
SS_plots(sigmaR)

modelnames <- c("8.0 Non-centered devs.", "8.1 Centered devs.", "8.2 Centered & SigmaR = 0.70")
mysummary <- SSsummarize(list(model, centered_devs, sigmaR))
SSplotComparisons(mysummary,
                  filenameprefix = "8.2_devs_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

phasing <- SS_output(file.path(wd, "8.3_phasing - Copy"))
end_devs <- SS_output(file.path(wd, "8.3_phasing_dev_end_yr"))
noncentered <- SS_output(file.path(wd, "8.3_phasing_noncentered_devs"))
pr_index <- SS_output(file.path(wd, "8.3_phasing_pr_index"))
hkl <- SS_output(file.path(wd, "8.3_phasing_rm_hkl_2022"))
nodevs <- SS_output(file.path(wd, "8.3_phasing_nodevs"))

modelnames <- c("Phasing", "Extend Devs", "Non-Centered", "PR Index 2022", "Rm. HKL 2022", "No. Devs.")
mysummary <- SSsummarize(list(phasing, end_devs, noncentered, pr_index, hkl, nodevs))
SSplotComparisons(mysummary,
                  filenameprefix = "8.3_phasing_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

est_m <- SS_output(file.path(wd, "8.4_est_m"))
est_h <- SS_output(file.path(wd, "8.4_est_h"))
tune_comps(replist = est_h, dir = file.path(wd, "8.4_est_h"), 
           option = "MI", write = TRUE, allow_up_tuning = TRUE)

est_h_rov <- SS_output(file.path(wd, "8.5_est_h_rm_rov_sd"))

catch <- SS_output(file.path(wd, "_sensitivities", "8.1_centered_devs_catch"))
estm <- SS_output(file.path(wd, "_sensitivities", "8.1_centered_devs_est_m"))
mat <- SS_output(file.path(wd, "_sensitivities", "8.1_centered_devs_mat"))
same_m <- SS_output(file.path(wd, "_sensitivities", "8.1_centered_devs_bio_offset"))
modelnames <- c("Centered Devs.", "Lower PR Catch", "Est M", "Same M by Sex", "Mat = 30.5")
mysummary <- SSsummarize(list(centered_devs, catch, estm, same_m, mat))
SSplotComparisons(mysummary,
                  filenameprefix = "8.1_sensitivities_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)



# Start model 8.1 -----
centered_dev <- SS_output(file.path(wd, "8.1_centered_devs"))
nosurveys <- SS_output(file.path(wd, "8.1a_nosurveys"))
ROVonly <- SS_output(file.path(wd, "8.1a_ROVonly"))
CCFRPonly <- SS_output(file.path(wd, "8.1c_CCFRPonly"))
HKLonly <- SS_output(file.path(wd, "8.1d_HKLonly"))

modelnames <- c("centered_dev", "nosurveys", "ROVonly", "CCFRPonly", "HKLonly")
mysummary <- SSsummarize(list(centered_dev, nosurveys, ROVonly, CCFRPonly, HKLonly))
SSplotComparisons(mysummary,
                  filenameprefix = "8.1_surveysoff_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


#Model 8.2
#no HKL survey and centered devs
noHKL <- SS_output(file.path(wd, "8.2_noHKL_devOpt2"))
tune_comps(replist = noHKL, dir = file.path(wd, "8.2_noHKL_devOpt2"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
SS_plots(noHKL)
#Hook and line survey is driving something - now to figure out what

#Put pieces of HKL back in
noHKL <- SS_output(file.path(wd, "8.2_noHKL_devOpt2"))
noHKLages <- SS_output(file.path(wd, "8.2_noHKLages_devOpt2"))
noHKLindex <- SS_output(file.path(wd, "8.2_noHKLindex_devOpt2"))
noHKLlengths <- SS_output(file.path(wd, "8.2_noHKLlengths_devOpt2"))

modelnames <- c("noHKL", "noHKLages", "noHKLindx", "noHKLlenghts")
mysummary <- SSsummarize(list(noHKL, noHKLages, noHKLindex, noHKLlengths))
SSplotComparisons(mysummary,
                  filenameprefix = "8.2_HKL_piecies_",
                  legendlabels = modelnames, 
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


#no rec indices
noRecIndex <- SS_output(file.path(wd, "8.3_noRecSurveys_devOpt2"))
tune_comps(replist = noRecIndex, dir = file.path(wd, "8.3_noRecSurveys_devOpt2"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
SS_plots(noRecIndex)



#for fun - remove the FI surveys and drop down the Rec historical catches
test1 <- SS_output(file.path(wd, "8.4_nosurveys_cutReccatches"))
tune_comps(replist = test1, dir = file.path(wd, "8.4_nosurveys_cutReccatches"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
SS_plots(test1)
#nothing useful here


base <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est"))
h_prior <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_h_prior"))
fix_mh <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_fix_m_h"))
rec_opt <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rec_opt_1"))
rm_ages <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_ages"))
rm_all_surveys <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_all_surveys"))
rm_ccfrp <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_ccfrp"))
rm_cpfv_indices <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_cpfv_indices"))
rm_growth <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_growth"))
rm_hkl <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl"))
rm_hkl_lens <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_lens"))
rm_hkl_bio <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_len_ages"))
rm_hkl_index <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_index"))
rm_hkl_ages <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_ages"))
rm_pr_index <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_pr_index"))
rm_rov <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_rov"))
split_hkl_comps <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_split"))
selex_block <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_selex_block"))
allow_dome <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_split_selex"))
selex_q_block <- SS_output(file.path(wd, "_sensitivities", "8.7_centered_m_h_est_rm_hkl_selex_q_block"))


modelnames <- c("Base", "- All Surveys", "- CCFRP", "- ROV", "- NWFSC HKL", "- PR Index", "- All CPFV Indices")
mysummary <- SSsummarize(list(base, rm_all_surveys, rm_ccfrp, rm_rov, rm_hkl, rm_pr_index, rm_cpfv_indices))
SSplotComparisons(mysummary,
                  filenameprefix = "8.7_surveys_indices_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

modelnames <- c("Base", "Rm NWFSC HKL", "Rm NWFSC HKL Ages", "Rm NWFSC HKL Lengths", 
                "Rm NWFSC HKL Lengths & Ages", "Rm NWFSC HKL Index")
mysummary <- SSsummarize(list(base, rm_hkl, rm_hkl_ages, rm_hkl_lens, rm_hkl_bio, rm_hkl_index))
SSplotComparisons(mysummary,
                  filenameprefix = "8.7_hkl_data_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


modelnames <- c("Base", "Rm NWFSC HKL", "Split NWFSC HKL Lengths & Ages", "Split and Allow Dome", 
                "One Fleet w/ Selex Block", "One Fleet w/ Q & Selex Block")
mysummary <- SSsummarize(list(base, rm_hkl, split_hkl_comps, allow_dome, selex_block, selex_q_block))
SSplotComparisons(mysummary,
                  filenameprefix = "8.7_hkl_data_split_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


modelnames <- c("Base", "- NWFSC HKL", "- NWFSC HKL Ages", "- Growth Ages")
mysummary <- SSsummarize(list(base, rm_hkl, rm_hkl_ages, rm_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "8.7_hkl_data_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

modelnames <- c("Base", "h Prior", "Fix M & h", "Rec. Opt = 1")
mysummary <- SSsummarize(list(base, h_prior, fix_mh , rec_opt))
SSplotComparisons(mysummary,
                  filenameprefix = "8.7_parameters_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

hkl_selex <- SS_output(file.path(wd, "8.8_hkl_selex_block"))
tune_comps(replist = hkl_selex, option = "MI", write = FALSE, allow_up_tuning = TRUE)
# NLL = 2096.51, R0 gradient = 0.0000389224000 (max gradient component)
hkl_fixed_mh <- SS_output(file.path(wd, "8.8_hkl_selex_block_fix_m_h"))
# NLL = 2100.25, R0 gradient =  0.00000178486000

modelnames <- c("No Split Est. M & h", "Split Est.", "No Split", "Split")
mysummary <- SSsummarize(list(base, hkl_selex, centered_dev, hkl_fixed_mh))
SSplotComparisons(mysummary,
                  filenameprefix = "8.8_hkl_split_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# Working off model 8.8 that includes selectivity block with fixed M & h
# NWFSC HKL data re-processed excluding fish from the CCAs and depths closed to fishing (>73 meters)
updated_hkl <- SS_output(file.path(wd, "9.0_update_hkl_w_block_fix_mh"))
SS_plots(updated_hkl)
modelnames <- c("NWFSC HKL Selex Block - All Data", "NWFSC HKL Selex Block - Open Areas")
mysummary <- SSsummarize(list(hkl_fixed_mh, updated_hkl))
SSplotComparisons(mysummary,
                  filenameprefix = "9.0_updated_hkl_data_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


add_hkl_ages_growth <- SS_output(file.path(wd, "9.1_add_omitted_hkl_ages_to_growth"))
SS_plots(add_hkl_ages_growth)
# NLL = 2124.06
modelnames <- c("NWFSC HKL Selex Block - All Data", "NWFSC HKL Selex Block - Open Areas", 
                "Add Omitted Ages to Growth")
mysummary <- SSsummarize(list(hkl_fixed_mh, updated_hkl, add_hkl_ages_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "9.1_updated_hkl_data_growth_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

add_hkl_ages_growth_rm_hkl <- SS_output(file.path(wd, "9.1_add_omitted_hkl_ages_to_growth_rm_hkl"))
add_hkl_ages_growth_est_mh <- SS_output(file.path(wd, "9.1_add_omitted_hkl_ages_to_growth_est_m_h"))
add_hkl_ages_growth_est_mh_rm_hkl <- SS_output(file.path(wd, "9.1_add_omitted_hkl_ages_to_growth_est_m_h_rm_hkl"))
modelnames <- c("9.1 Add Omitted Ages to Growth", "-Rm. NWFSC HKL",
                "9.1 Add Omitted Ages to Growth & Est M/h", "-Rm. NWFSC HKL Est M/h")
mysummary <- SSsummarize(list(add_hkl_ages_growth, add_hkl_ages_growth_rm_hkl,
                              add_hkl_ages_growth_est_mh, add_hkl_ages_growth_est_mh_rm_hkl))
SSplotComparisons(mysummary,
                  filenameprefix = "9.1_sensitivities_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


cpfv_asym <- SS_output(file.path(wd, "9.2_cpfv_asym_2000"))
# NLL = 2151 where the increase in likelihood is primarily coming for the ages in 
# the NWFSC HKL and Growth fleet
# CPFV length like difference is ~ 8 units
# This model results with the NWFSC HKL peak parameter from 2014-2022 at the upper bound of 52.9
# The ROV selectivity also becomes much closer to asymptotic in this run
modelnames <- c("NWFSC HKL Selex Block - All Data", "NWFSC HKL Selex Block - Open Areas", 
                "Add Omitted Ages to Growth", "CPFV Asym. Selex 2000+")
mysummary <- SSsummarize(list(hkl_fixed_mh, updated_hkl, add_hkl_ages_growth, cpfv_asym))
SSplotComparisons(mysummary,
                  filenameprefix = "9.2_cpfv_asym_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)
# Sensitivities to look at estimation of selectivity and removal of the NWFSC HKL fleet
cpfv_asym_est <- SS_output(file.path(wd, "9.2_cpfv_asym_2000_param"))
cpfv_asym_rm_hkl <- SS_output(file.path(wd, "9.2_cpfv_asym_2000_rm_hkl"))
cpfv_asym_est_m_h <- SS_output(file.path(wd, "9.2_cpfv_asym_2000_est_m_h"))
cpfv_asym_est_m_h_rm_hkl <- SS_output(file.path(wd, "9.2_cpfv_asym_2000_est_m_h_rm_hkl"))
modelnames <- c("CPFV Asym. Selex 2000+", "CPFV Asym. Rm. NWFSC HKL", 
                "CPFV Asym. + Est. M & h", "CPFV Asym. + Est. M & h, Rm. NWFSC HKL")
mysummary <- SSsummarize(list(cpfv_asym, cpfv_asym_rm_hkl, cpfv_asym_est_m_h,
                              cpfv_asym_est_m_h_rm_hkl))
SSplotComparisons(mysummary,
                  filenameprefix = "9.2_cpfv_asym_sens_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


modelnames <- c("NWFSC HKL Selex Block", "NWFSC HKL Selex Block & Est. M/h",
                "NWFSC HKL Selex Block & CPFV Asym.", "NWFSC HKL Selex Block & CPFV Asym. & Est. M/h")
mysummary <- SSsummarize(list(add_hkl_ages_growth, add_hkl_ages_growth_est_mh, 
                              cpfv_asym, cpfv_asym_est_m_h))
SSplotComparisons(mysummary,
                  filenameprefix = "9.1_9.2_est_selex_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

coop_ages <- SS_output(file.path(wd, "9.3_cpfv_asym_w_coop_ages"))
SS_plots(coop_ages)
# This model results with the NWFSC HKL peak parameter from 2014-2022 at the upper bound of 52.9
modelnames <- c("NWFSC HKL Selex Block - All Data", "NWFSC HKL Selex Block - Open Areas", 
                "Add Omitted Ages to Growth", "CPFV Asym. Selex 2000+", "+ Coop. Ages w/ CPFV")
mysummary <- SSsummarize(list(hkl_fixed_mh, updated_hkl, add_hkl_ages_growth, cpfv_asym, coop_ages))
SSplotComparisons(mysummary,
                  filenameprefix = "9.3_cpfv_asym_coop_ages_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

coop_ages_lens <- SS_output(file.path(wd, "9.4_cpfv_asym_w_coop_ages_lens"))
SS_plots(coop_ages_lens)
# This model results with the NWFSC HKL peak parameter from 2014-2022 at the upper bound of 52.9
modelnames <- c("NWFSC HKL Selex Block - All Data", "NWFSC HKL Selex Block - Open Areas", 
                "Add Omitted Ages to Growth", "CPFV Asym. Selex 2000+", 
                "+ Coop. Ages w/ CPFV", "+ Coop. Len. & Ages w/ CPFV")
mysummary <- SSsummarize(list(hkl_fixed_mh, updated_hkl, add_hkl_ages_growth, cpfv_asym, 
                              coop_ages, coop_ages_lens))
SSplotComparisons(mysummary,
                  filenameprefix = "9.4_cpfv_asym_coop_ages_lens_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.30,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

est_mh <- SS_output(file.path(wd, "9.5_cpfv_coop_est_m_h"))
SS_plots(est_mh)
# This model results with the NWFSC HKL peak parameter from 2014-2022 at the upper bound of 52.9
modelnames <- c("NWFSC HKL Selex Block - All Data", "NWFSC HKL Selex Block - Open Areas", 
                "Add Omitted Ages to Growth", "CPFV Asym. Selex 2000+", 
                "+ Coop. Ages w/ CPFV", "+ Coop. Len. & Ages w/ CPFV", "+ Est. M & h")
mysummary <- SSsummarize(list(hkl_fixed_mh, updated_hkl, add_hkl_ages_growth, cpfv_asym, 
                              coop_ages, coop_ages_lens, est_mh))
SSplotComparisons(mysummary,
                  filenameprefix = "9.5_cpfv_coop_est_mh_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

growth <- SS_output(file.path(wd, "9.6_mirror_growth_fleets"))
modelnames <- c("+ Est. M & h", "Mirror Growth Fleets")
mysummary <- SSsummarize(list(est_mh, growth))
SSplotComparisons(mysummary,
                  filenameprefix = "9.6_mirror_growth_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

growth_selex <- SS_output(file.path(wd, "9.6_mirror_growth_fleets_selex"))
# The age-selex is now domed peaking at young age but the improvement to the NLL
# is only 4 units for 2 extra parameters
SS_plots(growth_selex, plot = c(2, 16:18))

# This run keeps the coop length in the cpfv fleet but moves the ages back to the growth fleet
coop_ages_growth <- SS_output(file.path(wd, "9.7_move_coop_ages_to_growth"))
SS_plots(coop_ages_growth)

# 9.8 Fix ROV asymptotic, free up CCFRP selectivity, and remove the 2022 coop lengths
selex <- SS_output(file.path(wd, "10.2_selex"))
SS_plots(selex)

selex_fixm <- SS_output(file.path(wd, "10.2_selex_fix_m"))
SS_plots(selex_fixm)


selex_growth_fixm <- SS_output(file.path(wd, "10.2_selex_alt_growth_fix_m"))
SS_plots(selex_growth_fixm)

selex_alt <- SS_output(file.path(wd, "10.2_selex_alt_growth"))
SS_plots(selex_alt)
round(selex_alt$likelihoods_used$values,1)
selex_alt$parameters_with_highest_gradients
selex_alt$likelihoods_by_fleet[c(10,14), ]

sigmaR <- SS_output(file.path(wd, "10.3_sigmaR"))
# NLL = 2157.27
round(sigmaR$likelihoods_used$values,1)
sigmaR$parameters_with_highest_gradients
sigmaR$likelihoods_by_fleet[c(10,14), ]

logistic <- SS_output(file.path(wd, "10.4_logistic"))
round(logistic$likelihoods_used$values,1)
logistic$likelihoods_by_fleet[c(10,14), ]
logistic$parameters_with_highest_gradients
SS_plots(logistic, plot = c(2, 16))

logistic2 <- SS_output(file.path(wd, "10.5"))
round(logistic2$likelihoods_used$values,1)
logistic2$likelihoods_by_fleet[c(10,14), ]
logistic2$parameters_with_highest_gradients
SS_plots(logistic2, plot = c(2, 16:18))

com_dead <- SS_output(file.path(wd, "10.5_rm_com_block"))
round(com_dead$likelihoods_used$values,1)
com_dead$likelihoods_by_fleet[c(10,14), ]
SS_plots(com_dead)

cpfv_block <- SS_output(file.path(wd, "10.6_cpfv_block"))
round(cpfv_block$likelihoods_used$values,1)
cpfv_block$likelihoods_by_fleet[c(10,14), ]
cpfv_block$parameters_with_highest_gradients
SS_plots(cpfv_block, plot = c(2, 16:18))
tune_comps(replist = cpfv_block, dir = file.path(wd, "10.6_cpfv_block"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

mi <- SS_output(file.path(wd, "10.7_mi"))
francis <- SS_output(file.path(wd, "10.7_francis"))
# The two models below do not have redone data weights
mi_fix_m <- SS_output(file.path(wd, "10.7_mi_fix_m"))
francis_fix_m <- SS_output(file.path(wd, "10.7_francis_fix_m"))
tune_comps(replist = francis_fix_m, dir = file.path(wd, "10.7_francis_fix_m"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)

modelnames <- c("10.2_selex_alt_growth", "MI", "Francis", "MI - Fix M", "Francis - Fix M")
mysummary <- SSsummarize(list(selex_alt, mi, francis, mi_fix_m, francis_fix_m))
SSplotComparisons(mysummary,
                  filenameprefix = "10.7_dw_m_treatment_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#-------------------------------------------------------------------------------
# Model 10.7 sensitivity
#Add in the new area-weighted cpfv index
francis <- SS_output(file.path(wd, "10.7_francis"))
francis_cpfv <- SS_output(file.path(wd, "10.7_francis_update_cpfv"))
francis_cpfv_freehkl <- SS_output(file.path(wd, "10.7_francis_update_cpfv_freehklselex"))
simple_hkl_selex <- SS_output(file.path(wd, "10.7_francis_update_cpfv_simplifyhklselex"))
SS_plots(simple_hkl_selex)
tune_comps(replist = simple_hkl_selex, dir = file.path(wd, "10.7_francis_update_cpfv_simplifyhklselex"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

rmMRFSS <- SS_output(file.path(wd, "10.7_francis_update_cpfv_simplifyhklselex_rmMRFSS"))
SS_plots(rmMRFSS)
tune_comps(replist = rmMRFSS, dir = file.path(wd, "10.7_francis_update_cpfv_simplifyhklselex_rmMRFSS"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)


rmSurveyVar <-  SS_output(file.path(wd, "10.7_noaddvar"))

tune_comps(replist = rmSurveyVar, dir = file.path(wd, "10.7_noaddvar"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
SS_plots(rmSurveyVar)

modelnames <- c("UpdateCPFV1", "Simply_HKLselex2","RemoveMRFSS3", "RemoveSurveyVar")
mysummary <- SSsummarize(list(francis_cpfv, simple_hkl_selex, rmMRFSS, rmSurveyVar))
SSplotComparisons(mysummary,
                  filenameprefix = "10.7_additive_updates_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#------------------------------------------------------------------------------
# The models below include the updated area-weighted crfs cpfv index, remove the 
# mrfss cpfv index, and explore new area-weighted nwfsc hkl indices with M fixed
# for both sexes
update_rec_indices <- SS_output(file.path(wd, "10.9_updated_rec_indices"))
hkl_delta_log <- SS_output(file.path(wd, "10.9_hkl_delta_log"))
hkl_delta_log_re <- SS_output(file.path(wd, "10.9_hkl_delta_log_re"))
tune_comps(replist = hkl_delta_log_re, dir = file.path(wd,  "10.9_hkl_delta_log_re"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

modelnames <- c("Update CRFS & Remove MRFSS Index", "NWFSC HKL Area-Weighted Delta-Log",
                "NWFSC HKL Area-Weighted Delta-Log RE")
mysummary <- SSsummarize(list(update_rec_indices, hkl_delta_log, hkl_delta_log_re))
SSplotComparisons(mysummary,
                  filenameprefix = "10.9_area_weighted_indices_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

round(hkl_delta_log_re$likelihoods_used$values,1)
hkl_delta_log_re$likelihoods_by_fleet[c(10,14), ]
#NLL = 1929.2

# Double check selectivity given the new index
hkl_selex <- SS_output(file.path(wd, "10.9_hkl_delta_log_re_selex"))
round(hkl_selex$likelihoods_used$values,1)
hkl_selex$likelihoods_by_fleet[c(10,14), ]
#NLL = 1935 
# Logistic selex has the better likelihood fit coming from the lengths and the parameter priors

# Model 11.0_francis incorporates the area-weighted indices for both the CRFS CPFV index and the
# NWFSC HKL index (with RE). Logistic selectivity for the NWFSC HKL survey. The MRFS CPFV is 
# not being fit in the model (lambda = 0). Reweighted the model via the Francis method.
base <- SS_output(file.path(wd, "11.0_francis"))
alt_base <- SS_output(file.path(wd, "11.0_francis_cpfv_dome"))
tune_comps(replist = alt_base, dir = file.path(wd,  "11.0_francis_cpfv_dome"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

modelnames <- c("11.0 CPFV Selex Asym. 2004+", "11.0 CPFV Selex Domed 2004+")
mysummary <- SSsummarize(list(base, alt_base))
SSplotComparisons(mysummary,
                  filenameprefix = "11.0_base_alt",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# base = 8.7 model
crfs_index <- SS_output(file.path(wd, "11.1_8.7_build_replace_crfs_index"))
hkl_data <- SS_output(file.path(wd, "11.2_8.7_build_replace_hkl_index"))
rm_mrfss <- SS_output(file.path(wd, "11.3_8.7_build_rm_mrfss_index"))
rm_com_block_logistic <- SS_output(file.path(wd, "11.4_8.7_build_com_selex_block"))
SS_plots(rm_com_block_logistic, plot = 2)
hkl_selex <- SS_output(file.path(wd, "11.5_8.7_build_hkl_selex"))
rov_selex <- SS_output(file.path(wd, "11.6_8.7_build_rov_selex"))
rov_selex$likelihoods_by_fleet[c(10,14), ]
add_cpfv_block <- SS_output(file.path(wd, "11.7_8.7_build_add_cpfv_block"))
add_cpfv_block$likelihoods_by_fleet[c(10,14), ]

rov_dome <- SS_output(file.path(wd, "11.8_8.7_build_rov_dome"))
tune_comps(replist = rov_dome, dir = file.path(wd,  "11.8_8.7_build_rov_dome"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

dw <- SS_output(file.path(wd, "11.9_8.7_build_dw"))

modelnames <- c("8.7", "Update CRFS CPFV Index", "Update NWFSC HKL Index", "Rm. MRFSS CPFV Index",
                "Rm. Com. Dead Block", "Update HKL Selex", "Add CPFV Block", "DW")
mysummary <- SSsummarize(list(base, crfs_index, hkl_data, rm_mrfss, rm_com_block_logistic,
                              hkl_selex, rov_dome, dw))
SSplotComparisons(mysummary,
                  filenameprefix = "11.9_build_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


modelnames <- c("11.0 Francis CPFV Dome", "11.9", "11.9 Fix M & h")
mysummary <- SSsummarize(list(alt_base, dw, fix_mh))
SSplotComparisons(mysummary,
                  filenameprefix = "11.0_vs_11.9_build_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

rm_coop <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_rm_coop"))
rm_growth <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_rm_growth"))
rm_hkl <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_rm_hkl"))
rm_rov <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_rm_rov"))
rm_ccfrp <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_rm_ccfrp"))
rm_surveys <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_rm_all_surveys"))

modelnames <- c("11.9", "Rm. Coop. Ages", "Rm. Growth Ages", "Rm. NWFSC HKL",
                "Rm. ROV", "Rm. CCFRP", "Rm. All Surveys")
mysummary <- SSsummarize(list(dw, rm_coop, rm_growth, rm_hkl, rm_rov, rm_ccfrp, rm_surveys))
SSplotComparisons(mysummary,
                  filenameprefix = "11.9_sensitivities_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)

fix_m <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_fix_m"))
fix_h <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_fix_h"))
fix_mh <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_fix_m_h"))
com_asym <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_cpfv_asym"))
# NLL = 2113.84
fix_mh_com_asym <- SS_output(file.path(wd, "_sensitivities", "11.9_8.7_build_dw_fix_m_h_cpfv_asym"))
# NLL = 2116.38
# Fix both Ms & h with CPFV selex dome-shaped NLL = 2086.6

tune_comps(replist = fix_mh, dir = file.path(wd,   "_sensitivities", "11.9_8.7_build_dw_fix_m_h"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

modelnames <- c("11.9 Est. M & h", "Fix M", "Fix h", "Fix M & h", "11.9 Est M & h CPFV Asym.", "Fix M & h, CPFV Asym.")
mysummary <- SSsummarize(list(dw, fix_m, fix_h, fix_mh, com_asym, fix_mh_com_asym))
SSplotComparisons(mysummary,
                  filenameprefix = "11.9_sens_m_h_cpfv_selex_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  #legendloc = "bottomleft",
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)

round(dw$likelihoods_used$values,1)
round(fix_m$likelihoods_used$values,1)
round(fix_h$likelihoods_used$values,1)
round(fix_mh$likelihoods_used$values,1)
# Estimate both M & h NLL = 2083.6
# Fix both Ms when estimating h NLL = 2086.2 improvement of only 2.6 units 
# Fix h and estimate Ms NLL = 2083.6 same as the model that estimates both M & h
# Fix both Ms & h NLL = 2086.6

# Based on these sensitivities fix both Ms and h and allow CPFV fleet to estimate some level of dome-shaped selectivity
base <- SS_output(file.path(wd,  "11.10_8.7_build_dw_fix_m_h"))
model <- base; base_name <- "11.10_8.7_build_dw_fix_m_h"; model_dir <- wd
SS_plots(base)
round(base$likelihoods_used$values,1)

# NLL = 2167.8
base <- SS_output(file.path(wd, "12.0_base"))
base_mi <- SS_output(file.path(wd, "12.0_base_mi"))
tune_comps(replist = base, dir = file.path(wd,  "12.0_base"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)

rm_coop <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_coop"))
rm_growth <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_growth"))
rm_hkl <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_hkl"))
rm_rov <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_rov"))
rm_ccfrp <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_ccfrp"))
rm_surveys <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_surveys"))
rm_rec_indices <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_rec_indices"))
rm_hkl_ages <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_hkl_ages"))
cpfv_asym <- SS_output(file.path(wd, "_sensitivities", "12.0_base_cpfv_asym"))
cpfv_asym_hkl <- SS_output(file.path(wd, "_sensitivities", "12.0_base_cpfv_asym_rm_hkl"))
no_rec_devs <- SS_output(file.path(wd, "_sensitivities", "12.0_base_no_rec_devs"))

modelnames <- c("12.0", "Rm. Coop. Ages", "Rm. Growth Ages", "Rm. NWFSC HKL",
                "Rm. ROV", "Rm. CCFRP", "Rm. All Surveys", "Rm. Rec. Indices")
mysummary <- SSsummarize(list(base, rm_coop, rm_growth, rm_hkl, rm_rov, rm_ccfrp, rm_surveys, rm_rec_indices))
SSplotComparisons(mysummary,
                  filenameprefix = "12.0_sensitivities_2_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)

modelnames <- c("12.0", "CPFV Selex Asym.", "CPFV Selex Asym. & Rm. NWFSC HKL", "Rm. NWFSC HKL",
                "Rm. NWFSC HKL Ages")
mysummary <- SSsummarize(list(base, cpfv_asym, cpfv_asym_hkl, rm_hkl, rm_hkl_ages))
SSplotComparisons(mysummary,
                  filenameprefix = "12.0_sensitivities_1_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)

modelnames <- c("12.0", "No Rec. Devs.")
mysummary <- SSsummarize(list(base, no_rec_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "12.0_sensitivities_3_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)

growth_block <- SS_output(file.path(wd, "_sensitivities", "12.0_base_growth_block"))
SS_plots(growth_block, plot = c(2, 16:19))
growth_block$likelihoods_by_fleet[c(10,14), ]
tune_comps(replist = growth_block, dir = file.path(wd, "_sensitivities", "12.0_base_growth_block"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
# NLL = 2144.8


growth_block_len <- SS_output(file.path(wd, "_sensitivities", "12.0_base_growth_block_lengths"))
SS_plots(growth_block_len, plot = c(2, 16))
growth_block_len$likelihoods_by_fleet[c(10,14), ]
tune_comps(replist = growth_block_len, dir = file.path(wd, "_sensitivities", "12.0_base_growth_block_lengths"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

modelnames <- c("12.0 Francis", "12.0 MI")
mysummary <- SSsummarize(list(base, base_mi))
SSplotComparisons(mysummary,
                  filenameprefix = "12.0_dw_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

# Building up and exploring from 12.0 with francis data weights
# NLL = 2167.8
base <- SS_output(file.path(wd, "12.0_base"))
# Add the MRFSS index back in 
mrfss <- SS_output(file.path(wd, "_sensitivities", "12.0_base_add_mrfss"))
mrfss_devs <- SS_output(file.path(wd, "_sensitivities", "12.0_base_add_mrfss_dev_late_start"))
mrfss_rm_growth <- SS_output(file.path(wd, "_sensitivities", "12.0_base_add_mrfss_rm_growth"))
mrfss_devs_rm_growth <- SS_output(file.path(wd, "_sensitivities", "12.0_base_add_mrfss_dev_late_start_rm_growth"))


modelnames <- c("12.0 Francis", "+ MRFSS CPFV", "+ MRFSS, Late Devs.", "+ MRFSS - Growth", "+MRFSS, -Growth, Late Devs.")
mysummary <- SSsummarize(list(base, mrfss, mrfss_devs, mrfss_rm_growth, mrfss_devs_rm_growth))
SSplotComparisons(mysummary,
                  filenameprefix = "12.0_mrfss_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)


rm_2022_ages <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_2022_ages"))
rm_2022_lens <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_2022_lens"))
rm_2022_ages_lens <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_2022_len_ages"))

modelnames <- c("12.0 Francis", "- 2022 Ages", "- 2022 Lengths", "- Ages & Lengths")
mysummary <- SSsummarize(list(base, rm_2022_ages, rm_2022_lens, rm_2022_ages_lens))
SSplotComparisons(mysummary,
                  filenameprefix = "12.0_2022_data_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)

rm_2022_com_ages <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_2022_com_ages"))
rm_2022_ccrfp_ages <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_ccfrp_ages"))
rm_2022_hkl_ages <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_2022_hkl_ages"))
rm_2022_growth_ages <- SS_output(file.path(wd, "_sensitivities", "12.0_base_rm_2022_growth_ages"))

modelnames <- c("12.0 Francis", "- 2022 Com. Ages", "- 2022 CCFRP Ages", "- 2022 HKL Ages", "- 2022 Growth Ages")
mysummary <- SSsummarize(list(base, rm_2022_com_ages, rm_2022_ccrfp_ages, rm_2022_hkl_ages, rm_2022_growth_ages))
SSplotComparisons(mysummary,
                  filenameprefix = "12.0_2022_age_data_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)

# rm_2022_growth_ages$current_depletion
#  0.163807
# rm_2022_ages_lens$current_depletion
#  0.1340202
# base$current_depletion
#  0.1961074

# Move coop-ages into their own fleet since the model is sensitive to lumping these into one fleet with a selex block
# Lengths added but lambda = 0; Age-Selectivity
age_selex <- SS_output(file.path(wd, "12.2_coop_fleet_age_selex"))
# Switch to length-based selectivity for both fleet 9 & 10
len_selex <- SS_output(file.path(wd, "12.2_coop_fleet_len_selex"))
# Force the COOP length-based select asymptotic given where there data were collected
len_selex_asym <- SS_output(file.path(wd, "12.2_coop_fleet_len_selex_asym"))
tune_comps(replist = len_selex_asym, dir = file.path(wd,  "12.2_coop_fleet_len_selex_asym"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
len_selex_asym_dw <- SS_output(file.path(wd, "12.2_coop_fleet_len_selex_asym_dw"))

len_selex$likelihoods_by_fleet[c(10,14), ]
len_selex_asym$likelihoods_by_fleet[c(10,14), ]

modelnames <- c("12.0 Francis", "2 Growth Fleets - Age Selex", "2 Growth Fleets - Len Selex", 
                "2 Growth Fleets - Len Selex Asym.", "2 Growth Fleets - Len Selex Asym.- DW")
mysummary <- SSsummarize(list(base, age_selex, len_selex, len_selex_asym, len_selex_asym_dw))
SSplotComparisons(mysummary,
                  filenameprefix = "12.2_growth_fleets_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

rov_asym <- SS_output(file.path(wd, "12.3_rov_asym"))
rov_asym_dn <- SS_output(file.path(wd, "12.3_rov_asym_dn"))
rov_asym$likelihoods_by_fleet[c(10,14), ]
rov_asym_dn$likelihoods_by_fleet[c(10,14), ]

modelnames <- c("12.0 Francis",  "2 Growth Fleets - Len Selex Asym.- DW", "ROV Asym.", "ROV Asym. DN", "+ MRFSS CPFV Index")
mysummary <- SSsummarize(list(base, len_selex_asym_dw, rov_asym, rov_asym_dn, add_mrfss))
SSplotComparisons(mysummary,
                  filenameprefix = "12.3_rov_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

add_mrfss <- SS_output(file.path(wd, "12.4_add_mrfss_index"))
# The below model has the MRFSS CPFV index, double-normal ROV selectivity, and pearson, hkl, and coop ages in a single fleet
move_ages <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages"))
tune_comps(replist = move_ages, dir = file.path(wd,  "12.5_move_pearson_hkl_ages"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
rm_pearson_hkl_ages <- SS_output(file.path(wd, "12.5_rm_pearson_hkl_ages"))
tune_comps(replist = rm_pearson_hkl_ages, dir = file.path(wd,  "12.5_rm_pearson_hkl_ages"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

move_ages_dw <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages_dw"))
modelnames <- c("12.0 Francis",  "2 Growth Fleets - Len Selex Asym.- DW",  "ROV Asym. DN", "+ MRFSS CPFV Index",
                "Move Pearson/HKL Ages", "Move Ages - DW", "Remove Pearson/HKL Ages DW")
mysummary <- SSsummarize(list(base, len_selex_asym_dw, rov_asym_dn, add_mrfss, move_ages,  move_ages_dw, rm_pearson_hkl_ages))
SSplotComparisons(mysummary,
                  filenameprefix = "12.4_ages_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

no_devs <- SS_output(file.path(wd, "12.6_no_rec_devs"))
devs_2000 <- SS_output(file.path(wd, "12.6_start_devs_2000"))
devs_1990 <- SS_output(file.path(wd, "12.6_start_devs_1990"))
devs_1980 <- SS_output(file.path(wd, "12.6_start_devs_1980"))

tune_comps(replist = devs_1980, dir = file.path(wd,  "12.6_start_devs_1980"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

modelnames <- c("12.0 Francis",   "Move Ages - DW", "No Rec. Devs.", "Rec. Devs. 2000", "Rec Devs. 1990", "Rec. Devs. 1980")
mysummary <- SSsummarize(list(base, move_ages_dw, no_devs, devs_2000, devs_1990, devs_1980))
SSplotComparisons(mysummary,
                  filenameprefix = "12.6_rec_devs_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

devs_1990_h <- SS_output(file.path(wd, "12.6_start_devs_1990_est_h"))
devs_1980_h <- SS_output(file.path(wd, "12.6_start_devs_1980_est_h"))
round(devs_1980_h$likelihoods_used$values,1)
round(devs_1980$likelihoods_used$values,1)

modelnames <- c("Move Ages - DW", "Rec Devs. 1990", "Rec. Devs. 1980", "Rec Devs. 1990, Est. h", "Rec. Devs. 1980, Est h")
mysummary <- SSsummarize(list(move_ages_dw,devs_1990, devs_1980, devs_1990_h, devs_1980_h))
SSplotComparisons(mysummary,
                  filenameprefix = "12.6_rec_devs_est_h_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)
devs_1990_dw <- SS_output(file.path(wd, "12.6_start_devs_1990_dw"))
devs_1980_dw <- SS_output(file.path(wd, "12.6_start_devs_1980_dw"))
modelnames <- c("Move Ages - DW", "Rec Devs. 1990", "Rec. Devs. 1980")
mysummary <- SSsummarize(list(move_ages_dw,devs_1990_dw, devs_1980_dw))
SSplotComparisons(mysummary,
                  filenameprefix = "12.6_rec_devs_est_h_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)


add_cpfv_selex <- SS_output(file.path(wd, "12.7_cpfv_selex_2017"))
SS_plots(add_cpfv_selex, plot = c(2, 16))


rm_hkl_all <- SS_output(file.path(wd, "12.8_rm_hkl"))
rm_hkl_all_dw <- SS_output(file.path(wd, "12.8_rm_hkl_dw"))
SS_plots(rm_hkl_all_dw)
tune_comps(replist = rm_hkl_all, dir = file.path(wd,  "12.8_rm_hkl"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
rm_hkl_ages <- SS_output(file.path(wd, "12.8_rm_hkl_ages"))
rm_hkl_index <- SS_output(file.path(wd, "12.8_rm_hkl_index"))
rm_hkl_len_ages <- SS_output(file.path(wd, "12.8_rm_hkl_len_ages"))
modelnames <- c("Full Rec. Devs", "Rec. Devs. 1980", "- NWFSC HKL All Data", "- NWFSC HKL Ages", "- NWFSC HKL Index", 
                "- NWFSC HKL Comps.", "- NWFSC HKL All Data DW")
mysummary <- SSsummarize(list(move_ages_dw, devs_1980_dw, rm_hkl_all, rm_hkl_ages, rm_hkl_index, rm_hkl_len_ages,
                              rm_hkl_all_dw))
SSplotComparisons(mysummary,
                  filenameprefix = "12.8_nwfsc_hkl_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

# Explore info about M & h
move_ages_dw <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages_dw"))
round(move_ages_dw$likelihoods_used$values,1) # NLL = 2503
no_devs <- SS_output(file.path(wd, "12.6_no_rec_devs"))
round(no_devs$likelihoods_used$values,1) # NLL = 2850
devs_m_h <- SS_output(file.path(wd, "12.9_full_devs_m_h"))
# NLL = 2501
no_devs_m_h <- SS_output(file.path(wd, "12.9_no_devs_m_h"))
round(no_devs_m_h $likelihoods_used$values,1) # NLL = 2503
devs_1980_dw <- SS_output(file.path(wd, "12.6_start_devs_1980_dw"))
round(devs_1980_dw$likelihoods_used$values,1)# NLL = 2548
devs_1980_m_h <- SS_output(file.path(wd, "12.9_1980_devs_m_h"))
round(devs_1980_m_h$likelihoods_used$values,1) # NLL = 2545.6

modelnames <- c("Full Rec. Devs", "Rec. Devs. 1980", "No Rec. Devs.", "Full-Est. M & h", "1980 Devs-Est M & h")
mysummary <- SSsummarize(list(move_ages_dw, devs_1980_dw, no_devs, devs_m_h, devs_1980_m_h))
SSplotComparisons(mysummary,
                  filenameprefix = "12.9_devs_est_m_h_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

rm_add_var_surveys <- SS_output(file.path(wd, "12.10_no_added_var"))
rm_add_var_rov_ccfrp <- SS_output(file.path(wd, "12.11_no_added_var_ccfrp_rov"))
SS_plots(rm_add_var_rov_ccfrp)

modelnames <- c("Rec. Devs. 1980", "No Added Survey Var.", "No Added Survey Var. - CCFRP & ROV")
mysummary <- SSsummarize(list(devs_1980_dw, rm_add_var_surveys, rm_add_var_rov_ccfrp))
SSplotComparisons(mysummary,
                  filenameprefix = "12.10_survey_var_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

# Using the model where devs begin in 1980 explore dropping the NWFS HKL index but adding the lengths and ages
# to the COOP Fleet
drop_hkl_index <- SS_output(file.path(wd, "12.12_drop_hkl_index"))
SS_plots(drop_hkl_index, plot = c(2, 16:19))
tune_comps(replist = drop_hkl_index, dir = file.path(wd,  "12.12_drop_hkl_index"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
cpfv_selex <- SS_output(file.path(wd, "12.13_drop_hkl_index_cpfv_selex"))
SS_plots(cpfv_selex, plot = c(2, 16:20))
tune_comps(replist = cpfv_selex, dir = file.path(wd,  "12.13_drop_hkl_index_cpfv_selex"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)


drop_hkl_index <- SS_output(file.path(wd, "12.12_drop_hkl_index_dw"))
cpfv_selex <- SS_output(file.path(wd, "12.13_drop_hkl_index_cpfv_selex_dw"))
cpfv_selex_devs <- SS_output(file.path(wd, "12.14_drop_hkl_index_cpfv_selex_dw_all_devs_dw"))
tune_comps(replist = cpfv_selex_devs, dir = file.path(wd,  "12.14_drop_hkl_index_cpfv_selex_dw_all_devs_dw"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

modelnames <- c("All Rec. Devs", "Rec. Devs. 1980", "Drop NWFS HKL Index", "Drop HKL Index, CPFV Asym.",
                "All Rec. Devs. Com Asym Drop HKL Index")
mysummary <- SSsummarize(list(move_ages_dw, devs_1980_dw, drop_hkl_index, cpfv_selex, cpfv_selex_devs))
SSplotComparisons(mysummary,
                  filenameprefix = "12.12_rm_hkl_index_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

cpfv_asym <- SS_output(file.path(wd, "12.15_all_devs_hkl_index_cpfv_asym"))
tune_comps(replist = cpfv_asym, dir = file.path(wd,  "12.15_all_devs_hkl_index_cpfv_asym"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

cpfv_asym_dev_1980 <- SS_output(file.path(wd, "12.14_drop_hkl_index_cpfv_selex_dw_all_devs_devs_1980"))
tune_comps(replist = cpfv_asym_dev_1980, dir = file.path(wd,  "12.14_drop_hkl_index_cpfv_selex_dw_all_devs_devs_1980"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

#============================================================================================================
#
# 12.5_move_pearson_hkl_ages_dw
# Splits the growth ages into two fleets 1. WCGBT and 2. Pearson/NWFSC HKL/Coop Ages
# Estimate recruitment deviations for all years
# Retains the NWFSC HKL index and composition data
#
# 12.6_start_devs_1980_dw - alternative to the 12.5 model where devs start in 1980
# 
# 12.14_drop_hkl_index_cpfv_selex_dw_all_devs_dw
# Drops the NWFSC HKL index and moves the lengths and ages into the Coop Growth fleet
# Estimates recruitment deviations for all years
# Fixes the CPFV selectivity in the final block asymptotic
#
# 12.14_drop_hkl_index_cpfv_selex_dw_all_devs_devs_1980
# 
# 12.15_all_devs_hkl_index_cpfv_asym
# Estimate recruitment deviations for all years
# Retains the NWFSC HKL index and composition data
# Fixes the CPFV selectivity in the final block asymptotic
#
#
#============================================================================================================

full <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages_dw"))
SS_plots(full)
# One female M value - running informed by length data (hkl)
# Male M good: minimum around 0.115, informed by age data (growth and ccfrp) and recruitment 
# h good: value > 0.72 with all info coming from recruitment
# R0 all values okay but profile funky: infor from age data (lower R0 - CPFV) and recruitment 
# retro - normal with lower value maximum Mohn's of -1.24

late_devs <- SS_output(file.path(wd, "12.6_start_devs_1980_dw"))
SS_plots(late_devs)
# M female good: min ~0.11 informed by age data (coop growth lower M), length (HKL), and recruitment 
# M male good: min ~0.109 informed by age data (growth lower M), length, and recruitment
# h goodish: min ~0.72 informed by recruitment (higher), length data (cpfv), and age data (coop, lower)
# R0 goodish: tight min at 5.5ish, informed by length data (tight info), age data (lower), and recruitment 
# retro - standard with lower value maximum Mohn's of -1.47

# Alternative late start devs: 12.6_start_devs_1990_dw_profile_NatM_uniform_Fem_GP_1_prior_like_1
# M female good: informed by age data (lower - coop) and recruitment
# M male rerunning
# h good: minimum ~ 0.72 informed by recruitment (higher), lengths (higher), and ages (lower - coop)
# R0 good: sharp minimum around 5.5 informed by length data primarily and age data

drop_index <- SS_output(file.path(wd, "12.14_drop_hkl_index_cpfv_selex_dw_all_devs_dw"))
SS_plots(drop_index)
# Female M good: ~0.108 informed primarily by length data (coop)
# Male M good: ~0.108 informed by age and length data
# h rerun
# R0 good: age data lower (CPFV) and recruitment 
# retro standard but maximum awful with Mohn's rho of -2.48

drop_index_late_devs <- SS_output(file.path(wd, "12.14_drop_hkl_index_cpfv_selex_dw_all_devs_devs_1980_dw"))
SS_plots(drop_index_late_devs)
# M female good: minimum 0.108 informed by length data (coop)
# M male good: minimum 0.108 informed by length data (coop) and ages (coop)
# h rerun
# R0 good: cpfv length data informing 

fix_cpfv_selex <- SS_output(file.path(wd, "12.15_all_devs_hkl_index_cpfv_asym_dw"))
SS_plots(fix_cpfv_selex)

modelnames <- c("Full Model", "Full Model Late Devs.", "-NWFSC HKL Index & Fix CPFV Selex", 
                "-NWFSC HKL Index & Fix CPFV Selex - Late Devs.", "Fix CPFV Selex")
mysummary <- SSsummarize(list(full, late_devs, drop_index, drop_index_late_devs, fix_cpfv_selex))
SSplotComparisons(mysummary,
                  filenameprefix = "13_alternative_models_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)


full_est_m <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages_dw_est_m_dw"))
tune_comps(replist = full_est_m, dir = file.path(wd,  "12.5_move_pearson_hkl_ages_dw_est_m"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

late_devs_est_m <- SS_output(file.path(wd, "12.6_start_devs_1980_dw_est_m_dw"))
tune_comps(replist = late_devs_est_m, dir = file.path(wd,  "12.6_start_devs_1980_dw_est_m"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

drop_coop <- SS_output(file.path(wd, "12.14_drop_hkl_index_cpfv_selex_dw_all_devs_drop_coop"))


modelnames <- c("Full Model", "Full Model Est. M", "Full Model Late Devs.", "Full Model Late Devs. Est. M")
mysummary <- SSsummarize(list(full, full_est_m, late_devs, late_devs_est_m))
SSplotComparisons(mysummary,
                  filenameprefix = "13_alternative_models_est_m_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

full_model_est_m_cpfv_asym <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages_est_m_cpfv_selex"))
modelnames <- c("Full Model", "Full Model Est. M", "Full Model Est. M & CPFV Asym.")
mysummary <- SSsummarize(list(full, full_est_m, full_model_est_m_cpfv_asym))
SSplotComparisons(mysummary,
                  filenameprefix = "13_alternative_models_est_m_cpfv_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

#============================================================================================================
# Identifying a base model
#============================================================================================================

# Work from model "12.5_move_pearson_hkl_ages_dw" and/or the "12.5_move_pearson_hkl_ages_dw_est_m_dw"
# Start initial from the model that does not estimate M and then turn these parameter on after data/model
# structure updated
# Revisions from the 12.5 model are:
# 1. Update the CCFRP index to the non-area-weighted delta-lognormal index
# 2. Allow the ROV survey to have some level of dome given the limited sampling at the maximum depth range
# 3. Explore estimating vs. pinning CPFV selectivity
# 4. Potentially update the CRFS CPFV index based on Melissa' new work-up

#full <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages_dw"))
#full_est_m <- SS_output(file.path(wd, "12.5_move_pearson_hkl_ages_dw_est_m_dw"))

update_ccfrp_index <- SS_output(file.path(wd, "13.0_update_ccfrp_index"))
SS_plots(update_ccfrp_index)
update_rov_selex <- SS_output(file.path(wd, "13.1_rov_selex"))
SS_plots(update_rov_selex, plot = 2)
tune_comps(replist = update_rov_selex, dir = file.path(wd,  "13.1_rov_selex"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

com_asym <- SS_output(file.path(wd, "13.2_cpfv_selex_dw"))
SS_plots(com_asym)
tune_comps(replist = com_asym, dir = file.path(wd, "13.2_cpfv_selex_dw"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

cpfv_dome <- SS_output(file.path(wd, "13.2_cpfv_dome_dw"))
tune_comps(replist = cpfv_dome, dir = file.path(wd, "13.2_cpfv_dome_dw"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

cpfv_dome_est_m <- SS_output(file.path(wd, "13.3_cpfv_dome_est_m"))
tune_comps(replist = cpfv_dome_est_m, dir = file.path(wd, "13.3_cpfv_dome_est_m"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

cpfv_asym_est_m <- SS_output(file.path(wd, "13.3_cpfv_selex_est_m"))
tune_comps(replist = cpfv_asym_est_m, dir = file.path(wd, "13.3_cpfv_selex_est_m"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)
SS_plots(cpfv_dome_est_m); SS_plots(cpfv_asym_est_m); SS_plots(cpfv_dome); SS_plots(cpfv_asym)

modelnames <- c("12.5 (Fixed M)", "+ Update CCFRP Index", "+ Update ROV Selex", "+ CPFV Dome Selex", "+ CPFV Asym. Selex", 
                "+ CPFV Dome Est. M", "+ CPFV Asym. Est. M")
mysummary <- SSsummarize(list(full, update_ccfrp_index, update_rov_selex, cpfv_dome, cpfv_asym, cpfv_dome_est_m, cpfv_asym_est_m))
SSplotComparisons(mysummary,
                  filenameprefix = "13.0-2__",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd,  "_plots"),
                  pdf = TRUE)

all_marginals <- SS_output(file.path(wd, "_sensitivities", "13.3_cpfv_selex_est_m_marginals"))
tune_comps(replist = all_marginals, dir = file.path(wd, "_sensitivities", "13.3_cpfv_selex_est_m_marginals"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)


modelnames <- c("+ CPFV Asym. Est. M", "All Marginals")
mysummary <- SSsummarize(list(cpfv_asym_est_m, all_marginals))
SSplotComparisons(mysummary,
                  filenameprefix = "13.0_marginals__",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_sensitivities", "_plots"),
                  pdf = TRUE)


#============================================================================================================
# Diagnostics on the 13.2 model suite looking at cpfv selex and estimating m
#============================================================================================================
#
# 13.2_cpfv_dome_dw
# Jitter - BETTER MODEL FOUND (RUN 40)
# - M female profile good: length data from the HKL fleet
# - M male need to rerun one
# - h need to rerun one
# - R0 good - recruitment is primarily informing with age data saying lower (CPFV)
# - Only marginally bad
# MCMC Failed on this model=
#   
#   13.2_cpfv_selex (asymptotic)
# - Jitter good
# - M female - awful wild in shooting up
# - M male - same as female
# - Steepness also wild
# - R0 need to rerun one - recruitment and age data
# 
# 13.2 cpfv dome estimate m
# Jitter - BETTER MODEL FOUND (model 32)
# - M female - higher value based on recruitment with some age data from CPFV and growth
# - M male - recruitment and age data (CPFV)
# - h need to rerun one: recruitment LL not minimizing
# - R0 good - recruitment and age data 
# - Retro - marginally "best" (max mohns = -0.99)
# MCMC
# Parameters not moving 
# - selparm[12]
# - selparm[14]
# - selparm[27]
# - selparm[32] 
# - selparm[41] worst sd
# 
# 13.2 cpfv asymptotic and estimate m
# - Jitter found best fit
# - M female - wild and awful
# - M male good - recruitment and cpfv age data
# - h need to rerun one value - 
#   - R0 good - recruitment, length data (cpfv -lower), and age
# - Retro - marginally worse than 
###################################################################################################################

# The historical CPFV ages seem to have a very large influence in the model - do a sensitivity where these
# data have a low lambda

base <- SS_output(file.path(wd, "13.4_cpfv_dome_best_fit"))

# Lambda = 0.01
cpfv_ages <- SS_output(file.path(wd, "13.5_lambda_hist_ages"))

# CPFV Ages Lambda = 0.01 & Coop growth Lambda = 2
coop_ages <- SS_output(file.path(wd, "13.5_lambda_coop_ages"))

# CPFV Ages Lambda = 0.01 & Coop growth Lambda = 2
rm_survey_data <- SS_output(file.path(wd, "13.5_rm_survey_data"))

modelnames <- c("13.4", "Lambda CPFV Ages = 0.01", "+ Lambda Coop Growth = 2", "- All Survey Data")
mysummary <- SSsummarize(list(base, cpfv_ages, coop_ages, rm_survey_data ))
SSplotComparisons(mysummary,
                  filenameprefix = "13.5_ages__",
                  legendlabels = modelnames, 
                  ylimAdj = 1.40,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

fixed_base <- SS_output(file.path(wd, "13.6_dome_cpfv_lambda"))
SS_plots(fixed_base)

# Fix the dome at a higher level
slight_dome <- SS_output(file.path(wd, "13.6_slight_dome_cpfv_lambda"))
SS_plots(slight_dome)

fixed_base_m <- SS_output(file.path(wd, "13.6_slight_dome_cpfv_lambda_est_m"))
SS_plots(fixed_base_m)

combine_cpfv_block <- SS_output(file.path(wd, "13.6_dome_cpfv_lambda_combine_late_early_block"))
SS_plots(combine_cpfv_block)

modelnames <- c("13.2",  "13.6 CPFV Dome Fixed M", "13.6 Slight Dome Fixed M", 
                "13.6 Slight Dome Est M", "13.6 CPFV Block", "13.7")
mysummary <- SSsummarize(list(cpfv_dome, fixed_base, slight_dome, fixed_base_m, combine_cpfv_block, pre_base))
SSplotComparisons(mysummary,
                  filenameprefix = "13.6_dome_level_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#=======================================================================================
# 13.7_cpfv_block Model aligns the recreational blocks for CPFV and PR to both
# have a block 1916-1999, 2000-2003, and 2004-2022 where the first and the third
# block selectivities are the same
# Running the hessian on this model and re-estimating a lot of the selecitivy parameter
# with the plan of fixing any poorly estimated ones at the new MLE estimates.
# Leaving the CPFV age data lambda at 0.10
#=======================================================================================

pre_base <- SS_output(file.path(wd, "13.7_cpfv_block"))
SS_plots(pre_base, plot = c(2, 16))
tune_comps(replist = pre_base, dir = file.path(wd, "13.7_cpfv_block"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

# Adjust the bias recruitment paramters if needed
# Adjust data-weights
# Run MCMC on new model structure
# Run diagnostics for the new model with M fixed

# Test out estimating M in this model
pre_base_m <- SS_output(file.path(wd, "13.7_cpfv_block_est_m"))
SS_plots(pre_base_m)

pre_base_rm_hkl <- SS_output(file.path(wd, "13.7_cpfv_block_rm_hkl"))


# Test out new HKL index that uses all site and keeps all composition data associated
# with the survey fleet in the model
update_hkl <- SS_output(file.path(wd, "13.8_nwfsc_hkl"))
tune_comps(replist = update_hkl, dir = file.path(wd, "13.8_nwfsc_hkl"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

update_hkl_alt <- SS_output(file.path(wd, "13.8_nwfsc_hkl_dn_selex"))
tune_comps(replist = update_hkl_alt, dir = file.path(wd, "13.8_nwfsc_hkl_dn_selex"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
# Adding all the extra parameters of the double normal and having a block only
# decreased the NLL by < 2 units

update_hkl_asym <- SS_output(file.path(wd, "13.8_nwfsc_hkl_dn_selex_v2"))
tune_comps(replist = update_hkl_asym, dir = file.path(wd, "13.8_nwfsc_hkl_dn_selex_v2"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
SS_plots(update_hkl_asym, plot = 2)

update_hkl_asym_rm_hkl <- SS_output(file.path(wd, "13.8_nwfsc_hkl_dn_selex_v2_rm_hkl"))
# NLL = 2616.9 there is less that 1 NLL change when fixing the selex asym in the last block and 
# estimating a descending and logit parameter
# The descending and logit parameters in the early block have high SD

modelnames <- c( "13.7", "Update HKL w/ Logistic Selex Block", "Update HKL w/ DN Selex Block")
mysummary <- SSsummarize(list(pre_base, update_hkl, update_hkl_asym))
SSplotComparisons(mysummary,
                  filenameprefix = "13.7_hkl_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#====================================================================================
# Base Model 14.0
# After exploration no block in the HKL selectivity
# Remove Pearson Ages in order to better fit the COOP Ages
#====================================================================================

base_model <- SS_output(file.path(wd,  "14.0_base_forecast"))
tune_comps(replist = base_model, dir = file.path(wd, "14.0_base"), 
           option = "MI", write = FALSE, allow_up_tuning = TRUE)
SS_plots(base_model)

base_model_sigma <- SS_output(file.path(wd, "14.0_base_sigmaR"))

rm_hkl <- SS_output(file.path(wd, "14.0_base_rm_hkl"))
tune_comps(replist = rm_hkl, dir = file.path(wd, "14.0_base_rm_hkl"), 
  option = "Francis", write = FALSE, allow_up_tuning = TRUE)

modelnames <- c("13.7", "13.7 -remove HKL", "14.0", "14.0 -remove HKL")
mysummary <- SSsummarize(list(pre_base, pre_base_rm_hkl, base_model, rm_hkl))
SSplotComparisons(mysummary,
                  filenameprefix = "14.0_hkl_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.20,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

SSunavailableSpawningOutput(
  replist = base_model,
  print = TRUE,
  plotdir = file.path(wd, "14.0_base_forecast", "plots"))


coop <- SS_output(file.path(wd, "_sensitivities", "14.0_base_add_coop_cpfv_lambda1"))
tune_comps(replist = coop, dir = file.path(wd, "_sensitivities", "14.0_base_add_coop_cpfv_lambda1"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)

no_var <- SS_output(file.path(wd, "_sensitivities", "14.0_base_no_added_var"))

# Create comparison with the removal of the CDFW ROV data
no_rov <- SS_output(file.path(wd, "_sensitivities", "14.0_base_rm_rov"))
updated_rov <- SS_output(file.path(wd, "_sensitivities", "14.0_base_new_rov_index"))

modelnames <- c("14.0 Pre-STAR base", "Remove CDFW ROV data", "Corrected CDFW ROV Index")
mysummary <- SSsummarize(list(base_model, no_rov, updated_rov))

SSplotComparisons(mysummary,
                  filenameprefix = "14.0_rov_data_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.2,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

#================================================================================
# Remove ROV Data\
remove_rov <- SS_output(file.path(wd, "14.1_base_rm_rov"))

# Correct Sebastes Genus Catches in 2021 and 2022
fixed_catch <- SS_output(file.path(wd, "14.2_base_sebastes_catches"))
tune_comps(replist = fixed_catch, dir = file.path(wd, "14.2_base_sebastes_catches"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
SS_plots(fixed_catch, btarg = -1, minbthresh = -1)

modelnames <- c("Pre-STAR base", "Remove CDFW ROV data", "+Corrected Rec. Catches")
mysummary <- SSsummarize(list(base_model, remove_rov, fixed_catch))

SSplotComparisons(mysummary,
                  filenameprefix = "14.2_corrected_base_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.2,
                  btarg = -1,
                  minbthresh = -1,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)

base <- SS_output(file.path(wd, "14.3_revised_pre-star_base"))
SS_plots(fixed_catch, btarg = -1, minbthresh = -1)

SSplotComparisons(mysummary,
                  filenameprefix = "14.3_corrected_base_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  subplot = c(2, 4, 11), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)


post_star_base <- SS_output(file.path(wd, "15.0_south_post_star_base"))
tune_comps(replist = post_star_base, dir = file.path(wd, "15.0_south_post_star_base"), 
           option = "Francis", write = FALSE, allow_up_tuning = TRUE)
SS_plots(post_star_base, btarg = -1, minbthresh = -1)

post_star_base_reweight <- SS_output(file.path(wd, "15.0_south_post_star_base_reweight"))
modelnames <- c("Jittered", "Jittered Reweighted")
mysummary <- SSsummarize(list(post_star_base, post_star_base_reweight))

SSplotComparisons(mysummary,
                  filenameprefix = "15.0_jitter_weighted_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.2,
                  btarg = -1,
                  minbthresh = -1,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)