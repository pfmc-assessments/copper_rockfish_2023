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
# Remove the commercial live block in 2022 due to lack of data in this single year to inform
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
