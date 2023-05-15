########################################################
# Create Sensitivity table for Copper Rockfish 2023
#   written by : Chantel Wetzel
########################################################

library(r4ss)
library(here)

###################################################################
# North of Pt Conception
###################################################################
area <- 'nca'
base_model = "9.8_selex_fix"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area, "_sensitivities")
setwd(wd)
out.dir <- wd

base_loc <- file.path(user_dir, "models", area, base_model)

model_list <- c("est_m", #1
                "est_h", #2
                "est_m_h", #3
                "no_added_var",#4
                "no_devs", #5
                "dirichlet", #6
                "mi") #7

model_list2 =  c("lmin_equal_ave", #1
                 "lmin_equal_south", #2
                 "cut_pr_catch", #3
                 "add_hist_rec_to_growth", #4
                 "rov_super_period", #5
                 "lens_only") #6
                
model_list3 =  c("rm_coop_ages", #1
                 "rm_all_ages", #2
                 "rm_ccfrp", #3
                 "rm_rov", #4
                 "rm_all_surveys", #5
                 "rm_cpfv_index", #6
                 "rm_dwv", #7
                 "rm_pr_index") #8

model_list <- paste0(base_model, "_", model_list)
model_list2 <- paste0(base_model, "_", model_list2)
model_list3 <- paste0(base_model, "_", model_list3)

#out.list = NULL	
base   <- SS_output( base_loc, printstats = FALSE, verbose = FALSE) 

sens_1  <- SS_output( file.path(wd, model_list[1]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
sens_2  <- SS_output( file.path(wd, model_list[2]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
sens_3  <- SS_output( file.path(wd, model_list[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_4  <- SS_output( file.path(wd, model_list[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_5  <- SS_output( file.path(wd, model_list[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_6  <- SS_output( file.path(wd, model_list[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_7  <- SS_output( file.path(wd, model_list[7]), printstats = FALSE, verbose = FALSE, covar = FALSE)

sens2_1  <- SS_output( file.path(wd, model_list2[1]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_2  <- SS_output( file.path(wd, model_list2[2]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_3  <- SS_output( file.path(wd, model_list2[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_4  <- SS_output( file.path(wd, model_list2[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_5  <- SS_output( file.path(wd, model_list2[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_6  <- SS_output( file.path(wd, model_list2[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)

sens3_1  <- SS_output( file.path(wd, model_list3[1]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_2  <- SS_output( file.path(wd, model_list3[2]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_3  <- SS_output( file.path(wd, model_list3[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_4  <- SS_output( file.path(wd, model_list3[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_5  <- SS_output( file.path(wd, model_list3[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_6  <- SS_output( file.path(wd, model_list3[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_7  <- SS_output( file.path(wd, model_list3[7]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_8  <- SS_output( file.path(wd, model_list3[8]), printstats = FALSE, verbose = FALSE, covar = FALSE)


modelnames <- c("Base Model",
                "Estimate M",
                "Estimate h", 
                "Estimate M & h",
                "No Added Variance",
                "No Rec. Devs.",
                "Dirichlet DW", 
                "McAllister-Ianelli DW")

modelnames2 <- c("Base Model",
                 "L2 Equal to 13.6 cm",
                 "L2 Equal to South Ests.",
                 "Reduce Rec. Catch 1970-82",
                 "Add Hist. CPFV Ages to Growth",
                 "ROV Len. Super Period",
                 "Lengths Only")

modelnames3 <- c("Base Model",
                 "Rm. Coop. Ages",
                 "Rm. All Ages",
                 "Rm. CCFRP",
                 "Rm. CDFW ROV",
                 "Rm. All Surveys",
                 "Rm. CPFV Index", 
                 "Rm. DWV Index",
                 "Rm. PR Index")

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_7))
x2 <- SSsummarize(list(base, sens2_1, sens2_2, sens2_3, sens2_4, sens2_5, sens2_6))
x3 <- SSsummarize(list(base, sens3_1, sens3_2, sens3_3, sens3_4, sens3_5, sens3_6, sens3_7, sens3_8))

SSplotComparisons(x, 
                  endyrvec = 2023, 
                  legendlabels = modelnames, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_model, "_forecast_final_1_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

SSplotComparisons(x, 
                  endyrvec = 2023, 
                  legendlabels = modelnames, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topleft", 
                  filenameprefix = paste0(base_model, "_forecast_final_1_"),
                  subplot = c(11), 
                  print = TRUE)

SSplotComparisons(x2, 
                  endyrvec = 2023, 
                  legendlabels = modelnames2, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  ylimAdj = 1.15,
                  filenameprefix = paste0(base_model, "_forecast_final_2_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

SSplotComparisons(x2, 
                  endyrvec = 2023, 
                  legendlabels = modelnames2, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topleft", 
                  ylimAdj = 1.15,
                  filenameprefix = paste0(base_model, "_forecast_final_2_"),
                  subplot = c(11), 
                  print = TRUE)

SSplotComparisons(x3, 
                  endyrvec = 2023, 
                  legendlabels = modelnames3, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  ylimAdj = 1.15,
                  filenameprefix = paste0(base_model, "_forecast_final_3_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

SSplotComparisons(x3, 
                  endyrvec = 2023, 
                  legendlabels = modelnames3, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topleft", 
                  ylimAdj = 1.15,
                  filenameprefix = paste0(base_model, "_forecast_final_3_"),
                  subplot = c(11), 
                  print = TRUE)

###################################################################################
# Jason Style Sensitivity Figure
###################################################################################

modelnames <- c("Base Model",
                "Estimate M",
                "Estimate h", 
                "Estimate M & h",
                "No Added Variance",
                "No Rec. Devs.",
                "Dirichlet DW", 
                "McAllister-Ianelli DW")

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_7))

wd_dat <- file.path(paste0(wd,"/_plots")) 
# Sensitivity figure is something I adapted from Jason's Original that is in r4ss (SS_Sensi_plot)
# Here is where my version can be found: https://github.com/chantelwetzel-noaa/dover_sole_2021/blob/master/code/sensi_plot_Dover.R
Sensi_plot_dover(model.summaries=x,
                 dir = wd_dat,
                 current.year=2023,
                 mod.names = modelnames, #List the names of the sensitivity runs
                 likelihood.out = c(0, 1, 0),
                 Sensi.RE.out="Sensi_RE_out.DMP", #Saved file of relative errors
                 CI=0.95, #Confidence interval box based on the reference model
                 TRP.in=-1, #Target relative abundance value
                 LRP.in=-1, #Limit relative abundance value
                 sensi_xlab="Sensitivity scenarios", #X-axis label
                 ylims.in=c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1, -1,1, -1,1,-1,1), #Y-axis label
                 plot.figs=c(1,1,1,1,1,1), #Which plots to make/save? 
                 #sensi.type.breaks=c(4.5, 6.5, 9.5), #vertical breaks that can separate out types of sensitivities
                 #anno.x=c(3, 5.5, 8, 10.5), # Vertical positioning of the sensitivity types labels
                 #anno.y=c(0.83,0.80,0.85,0.9), # Horizontal positioning of the sensitivity types labels
                 #anno.lab=c("Parameters", "Data Weighting", "Selectivity", "Index"), #Sensitivity types labels
                 horizontal = TRUE) 

modelnames <- c("Base Model",
                "L2 Equal to 13.6 cm",
                "L2 Equal to South Ests.",
                 "Reduce Rec. Catch 1970-82",
                 "Add Hist. CPFV Ages to Growth",
                 "ROV Lens. Super Period",
                 "Lengths Only", 
                 "Rm. Coop. Ages",
                 "Rm. All Ages",
                 "Rm. CCFRP",
                 "Rm. CDFW ROV",
                 "Rm. All Surveys",
                 "Rm. CPFV Index", 
                 "Rm. DWV Index",
                 "Rm. PR Index")

x <- SSsummarize(list(base, 
                      sens2_1, sens2_2, sens2_3, sens2_4, sens2_5, sens2_6, 
                      sens3_1, sens3_2, sens3_3, sens3_4, sens3_5, sens3_6, sens3_7, sens3_8))

###################################################################################
# Create a Table of Results
###################################################################################

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_7))
modelnames <- c("Base Model",
                "Estimate M",
                "Estimate h", 
                "Estimate M & h",
                "No Added Variance",
                "No Rec. Devs.",
                "Dirichlet DW", 
                "McAllister-Ianelli DW")


ii = 1:length(modelnames)
n = length(modelnames)
out<- matrix(NA, 24, max(ii))

out = rbind(
  as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2023", 1:n]),
  as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2023", 1:n]), 
  as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_uniform_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_uniform_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Mal_GP_1", 1:n]) )  

out = as.data.frame(out)
colnames(out) = modelnames
rownames(out) = c("Total Likelihood",
                  "Survey Likelihood",
                  "Length Likelihood",
                  "Age Likelihood",
                  "Recruitment Likelihood",
                  "Forecast Recruitment Likelihood",
                  "Parameter Priors Likelihood",
                  "log(R0)",
                  "SB Virgin",
                  "SB 2023",
                  "Fraction Unfished 2023",
                  "Total Yield - SPR 50",
                  "Steepness",
                  "Natural Mortality - Female",
                  "Length at Amin - Female",
                  "Length at Amax - Female",
                  "Von Bert. k - Female",
                  "CV young - Female",
                  "CV old - Female",
                  "Natural Mortality - Male",
                  "Length at Amin - Male",
                  "Length at Amax - Male",
                  "Von Bert. k - Male",
                  "CV young - Male",
                  "CV old - Male")


write.csv(out, file = file.path(out.dir, paste0(base_model, "_1_sensitivities_final.csv")))

t = sa4ss::table_format(x = out,
                        caption = 'Sensitivities relative to the base model.',
                        label = 'sensitivities-1',
                        longtable = TRUE,
                        font_size = 9,
                        digits = 3,
                        landscape = TRUE,
                        col_names = modelnames)

kableExtra::save_kable(t,
                       file = "C:/Assessments/2023/copper_rockfish_2023/documents/nca/tex_tables/sensitivities_1_final.tex")


x = x2
ii = 1:length(modelnames2)
n = length(modelnames2)
out<- matrix(NA, 24, max(ii))

out = rbind(
  as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2023", 1:n]),
  as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2023", 1:n]), 
  as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_uniform_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_uniform_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Mal_GP_1", 1:n]) )  

out = as.data.frame(out)
colnames(out) = modelnames2
rownames(out) = c("Total Likelihood",
                  "Survey Likelihood",
                  "Length Likelihood",
                  "Age Likelihood",
                  "Recruitment Likelihood",
                  "Forecast Recruitment Likelihood",
                  "Parameter Priors Likelihood",
                  "log(R0)",
                  "SB Virgin",
                  "SB 2023",
                  "Fraction Unfished 2023",
                  "Total Yield - SPR 50",
                  "Steepness",
                  "Natural Mortality - Female",
                  "Length at Amin - Female",
                  "Length at Amax - Female",
                  "Von Bert. k - Female",
                  "CV young - Female",
                  "CV old - Female",
                  "Natural Mortality - Male",
                  "Length at Amin - Male",
                  "Length at Amax - Male",
                  "Von Bert. k - Male",
                  "CV young - Male",
                  "CV old - Male")


write.csv(out, file = file.path(out.dir, paste0(base_model, "_2_sensitivities_final.csv")))

t = sa4ss::table_format(x = out,
                        caption = 'Sensitivities relative to the base model.',
                        label = 'sensitivities-2',
                        longtable = TRUE,
                        font_size = 9,
                        digits = 3,
                        landscape = TRUE,
                        col_names = modelnames2)

kableExtra::save_kable(t,
                       file = "C:/Assessments/2023/copper_rockfish_2023/documents/nca/tex_tables/sensitivities_2_final.tex")


x = x3
ii = 1:length(modelnames3)
n = length(modelnames3)
out<- matrix(NA, 24, max(ii))

out = rbind(
  as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2023", 1:n]),
  as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2023", 1:n]), 
  as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_uniform_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_uniform_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Mal_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Mal_GP_1", 1:n]) )  

out = as.data.frame(out)
colnames(out) = modelnames3
rownames(out) = c("Total Likelihood",
                  "Survey Likelihood",
                  "Length Likelihood",
                  "Age Likelihood",
                  "Recruitment Likelihood",
                  "Forecast Recruitment Likelihood",
                  "Parameter Priors Likelihood",
                  "log(R0)",
                  "SB Virgin",
                  "SB 2023",
                  "Fraction Unfished 2023",
                  "Total Yield - SPR 50",
                  "Steepness",
                  "Natural Mortality - Female",
                  "Length at Amin - Female",
                  "Length at Amax - Female",
                  "Von Bert. k - Female",
                  "CV young - Female",
                  "CV old - Female",
                  "Natural Mortality - Male",
                  "Length at Amin - Male",
                  "Length at Amax - Male",
                  "Von Bert. k - Male",
                  "CV young - Male",
                  "CV old - Male")


write.csv(out, file = file.path(out.dir, paste0(base_model, "_3_sensitivities_final.csv")))

t = sa4ss::table_format(x = out,
                        caption = 'Sensitivities relative to the base model.',
                        label = 'sensitivities-3',
                        longtable = TRUE,
                        font_size = 9,
                        digits = 3,
                        landscape = TRUE,
                        col_names = modelnames3)

kableExtra::save_kable(t,
                       file = "C:/Assessments/2023/copper_rockfish_2023/documents/nca/tex_tables/sensitivities_3_final.tex")
