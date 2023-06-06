########################################################
# Create Sensitivity table for Copper Rockfish 2023
#   written by : Chantel Wetzel
########################################################

library(r4ss)
library(here)

###################################################################
# South of Pt Conception
###################################################################
area <- 'sca'
base_model = "14.0_base"

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
              "no_devs", #4
              "cpfv_asym", #5
              "growth_platoons", #6
              "no_added_var", #7
              "dirichlet", #8
              "mi_no_hessian") #9

model_list2 =  c("cut_rec_catch", #1
                "lambda_age", #2
                "rm_ages", #3
                "rm_coop", #4
                "add_coop_cpfv_lambda1", #5
                "rm_wcgbt", #6
                "wcgbt_index", #7
                "rm_fishery_indices") #8

model_list3 =  c("rm_ccfrp", #1
                 "rm_rov", #2
                 "rm_hkl", #3
                 "rm_hkl_ages", #4
                 "rm_hkl_len_age", #5
                 "rm_hkl_index", #6
                 "hkl_rm_2014_data", #7
                 "rm_surveys") #8

model_list <- paste0(base_model, "_", model_list)
model_list2 <- paste0(base_model, "_", model_list2)
model_list3 <- paste0(base_model, "_", model_list3)

#out.list = NULL	
base   <- SS_output( paste0(base_loc, "_forecast"), printstats = FALSE, verbose = FALSE) 

sens_1  <- SS_output( file.path(wd, model_list[1]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
sens_2  <- SS_output( file.path(wd, model_list[2]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
sens_3  <- SS_output( file.path(wd, model_list[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_4  <- SS_output( file.path(wd, model_list[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_5  <- SS_output( file.path(wd, model_list[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_6  <- SS_output( file.path(wd, model_list[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_7  <- SS_output( file.path(wd, model_list[7]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_8  <- SS_output( file.path(wd, model_list[8]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_9  <- SS_output( file.path(wd, model_list[9]), printstats = FALSE, verbose = FALSE, covar = FALSE)


sens2_1  <- SS_output( file.path(wd, model_list2[1]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_2  <- SS_output( file.path(wd, model_list2[2]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_3  <- SS_output( file.path(wd, model_list2[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_4  <- SS_output( file.path(wd, model_list2[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_5  <- SS_output( file.path(wd, model_list2[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_6  <- SS_output( file.path(wd, model_list2[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_7  <- SS_output( file.path(wd, model_list2[7]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_8  <- SS_output( file.path(wd, model_list2[8]), printstats = FALSE, verbose = FALSE, covar = FALSE)

sens3_1  <- SS_output( file.path(wd, model_list3[1]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_2  <- SS_output( file.path(wd, model_list3[2]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_3  <- SS_output( file.path(wd, model_list3[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_4  <- SS_output( file.path(wd, model_list3[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_5  <- SS_output( file.path(wd, model_list3[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_6  <- SS_output( file.path(wd, model_list3[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_7  <- SS_output( file.path(wd, model_list3[7]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens3_8  <- SS_output( file.path(wd, model_list3[8]), printstats = FALSE, verbose = FALSE, covar = FALSE)

#tune_comps(replist = sens2_2, dir = file.path(wd, model_list2[2]), 
#           option = "Francis", write = FALSE, allow_up_tuning = TRUE)


SSunavailableSpawningOutput(
  replist = sens3_3,
  print = TRUE,
  plotdir = file.path(wd, "_plots"))

modelnames <- c("Base Model",
               "Estimate M",
               "Estimate h", 
               "Estimate M & h",
               "No Rec. Devs.",
               "CPFV Selectivity Asym.",
               "Growth Platoons",
               "No Added Variance",
               "Dirichlet DW", 
               "McAllister-Ianelli DW")

modelnames11 <- c("Base Model",
                "Estimate M",
                "Estimate h", 
                "Estimate M & h",
                "No Rec. Devs.")

modelnames12 <- c("Base Model",
                "CPFV Selectivity Asym.",
                "Growth Platoons",
                "No Added Variance",
                "Dirichlet DW", 
                "McAllister-Ianelli DW")

modelnames2 <- c("Base Model",
                "Reduce PR Catch 1970-82",
                "Hist. CPFV Ages Lambda = 1",
                "Rm. All Ages",
                "Rm. Coop. Ages", 
                "Add Coop. Ages to CPFV",
                "Rm. WCGBT Ages",
                "Add WCGBT Index",
                "Rm. CPFV & PR Indices")

modelnames3 <- c("Base Model",
                 "Rm. CCFRP",
                 "Rm. CDFW ROV",
                 "Rm. NWFSC HKL All",
                 "Rm. NWFSC HKL Ages",
                 "Rm. NWFSC HKL Lens. & Ages",
                 "Rm. NWFSC HKL Index",
                 "Move NWFSC HKL Data Before 2014",
                 "Rm. All Surveys")

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_7, sens_8, sens_9))
x11 <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4))
x12 <- SSsummarize(list(base, sens_5, sens_6, sens_7, sens_8, sens_9))

x2 <- SSsummarize(list(base, sens2_1, sens2_2, sens2_3, sens2_4, sens2_5, sens2_6, sens2_7, sens2_8))
x3 <- SSsummarize(list(base, sens3_1, sens3_2, sens3_3, sens3_4, sens3_5, sens3_6, sens3_7, sens3_8))

SSplotComparisons(x11, 
                  endyrvec = 2023, 
                  legendlabels = modelnames11, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_model, "_forecast_final_1_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

SSplotComparisons(x11, 
                  endyrvec = 2023, 
                  legendlabels = modelnames11, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topleft", 
                  filenameprefix = paste0(base_model, "_forecast_final_1_"),
                  subplot = c(11), 
                  print = TRUE)

SSplotComparisons(x12, 
                  endyrvec = 2023, 
                  legendlabels = modelnames12, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_model, "_forecast_final_4_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

SSplotComparisons(x12, 
                  endyrvec = 2023, 
                  legendlabels = modelnames12, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topleft", 
                  filenameprefix = paste0(base_model, "_forecast_final_4_"),
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
                "Estimate M", #1
                "Estimate h", #2
                "Estimate M & h",#3
                #"No Rec. Devs.",#4
                "CPFV Selectivity Asym.",#5
                #"Growth Platoons",#6
                #"No Added Variance",#7
                #"Dirichlet DW", #8
                #"McAllister-Ianelli DW",#9
                "Reduce PR Catch 1970-82",#1
              #"Hist. CPFV Ages Lambda = 1",#2
              #"Rm. All Ages",#3
              "Rm. Coop. Ages", #4
              #"Add Coop. Ages to CPFV",#5
              "Rm. WCGBT Ages",#6
              #"Add WCBT Index",#7
              "Rm. CPFV & PR Indices",#8
              "Rm. CCFRP",#1
              "Rm. CDFW ROV",#2
              "Rm. NWFSC HKL All",#3
              "Rm. NWFSC HKL Ages",#4
              "Rm. NWFSC HKL Lens. & Ages",#5
              "Rm. NWFSC HKL Index",#6
              #"Move NWFSC HKL Data Before 2014",#7
              "Rm. All Surveys")#8

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_5, 
                      sens2_1, sens2_4, sens2_6, sens2_8, 
                      sens3_1, sens3_2, sens3_3, sens3_4, sens3_5, sens3_6,  sens3_8))


wd_dat <- file.path(paste0(wd,"/_plots")) 
# Sensitivity figure is something I adapted from Jason's Original that is in r4ss (SS_Sensi_plot)
# Here is where my version can be found: https://github.com/chantelwetzel-noaa/dover_sole_2021/blob/master/code/sensi_plot_Dover.R
Sensi_plot_dover(model.summaries=x,
                 dir = wd_dat,
                 current.year=2023,
                 mod.names=modelnames, #List the names of the sensitivity runs
                 likelihood.out = c(0, 1, 0),
                 Sensi.RE.out="Sensi_RE_out.DMP", #Saved file of relative errors
                 CI=0.95, #Confidence interval box based on the reference model
                 TRP.in=-1, #Target relative abundance value
                 LRP.in=-1, #Limit relative abundance value
                 sensi_xlab="Sensitivity scenarios", #X-axis label
                 ylims.in=c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1), #Y-axis label
                 plot.figs=c(1,1,1,1,1,1), #Which plots to make/save? 
                 #sensi.type.breaks=c(4.5, 6.5, 9.5), #vertical breaks that can separate out types of sensitivities
                 #anno.x=c(3, 5.5, 8, 10.5), # Vertical positioning of the sensitivity types labels
                 #anno.y=c(0.83,0.80,0.85,0.9), # Horizontal positioning of the sensitivity types labels
                 #anno.lab=c("Parameters", "Data Weighting", "Selectivity", "Index"), #Sensitivity types labels
                 horizontal = TRUE) 

# Structural

modelnames <- c("Base Model",
                "Estimate M",
                "Estimate h", 
                "Estimate M & h",
                "No Rec. Devs.",
                "CPFV Selectivity Asym.",
                "Growth Platoons",
                "No Added Variance",
                "Dirichlet DW", 
                "McAllister-Ianelli DW")

modelnames_data <- c("Base Model",
                 "Reduce PR Catch 1970-82",
                 "Hist. CPFV Ages Lambda = 1",
                 "Rm. All Ages",
                 "Rm. Coop. Ages", 
                 "Add Coop. Ages to CPFV",
                 "Rm. WCGBT Ages",
                 "Add WCBT Index",
                 "Rm. CPFV & PR Indices",
                 "Rm. CCFRP",
                 "Rm. CDFW ROV",
                 "Rm. NWFSC HKL All",
                 "Rm. NWFSC HKL Ages",
                 "Rm. NWFSC HKL Lens. & Ages",
                 "Rm. NWFSC HKL Index",
                 "Move NWFSC HKL Data Before 2014",
                 "Rm. All Surveys")

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_7, sens_8, sens_9))


wd_dat <- file.path(paste0(wd,"/_plots")) 
# Sensitivity figure is something I adapted from Jason's Original that is in r4ss (SS_Sensi_plot)
# Here is where my version can be found: https://github.com/chantelwetzel-noaa/dover_sole_2021/blob/master/code/sensi_plot_Dover.R
Sensi_plot_dover(model.summaries=x,
                 dir = wd_dat,
                 current.year=2023,
                 mod.names=modelnames, #List the names of the sensitivity runs
                 likelihood.out = c(0, 1, 0),
                 Sensi.RE.out="Sensi_RE_out.DMP", #Saved file of relative errors
                 CI=0.95, #Confidence interval box based on the reference model
                 TRP.in=-1, #Target relative abundance value
                 LRP.in=-1, #Limit relative abundance value
                 sensi_xlab="Sensitivity scenarios", #X-axis label
                 ylims.in=c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1), #Y-axis label
                 plot.figs=c(1,1,1,1,1,1), #Which plots to make/save? 
                 #sensi.type.breaks=c(4.5, 6.5, 9.5), #vertical breaks that can separate out types of sensitivities
                 #anno.x=c(3, 5.5, 8, 10.5), # Vertical positioning of the sensitivity types labels
                 #anno.y=c(0.83,0.80,0.85,0.9), # Horizontal positioning of the sensitivity types labels
                 #anno.lab=c("Parameters", "Data Weighting", "Selectivity", "Index"), #Sensitivity types labels
                 horizontal = TRUE) 


x <- SSsummarize(list(base, sens2_1, sens2_2, sens2_3, sens2_4, sens2_5, sens2_6, sens2_7, sens2_8, 
            sens3_1, sens3_2, sens3_3, sens3_4, sens3_5, sens3_6, sens3_7, sens3_8))

Sensi_plot_dover(model.summaries=x,
                 dir = wd_dat,
                 current.year=2023,
                 mod.names=modelnames_data, #List the names of the sensitivity runs
                 likelihood.out = c(0, 1, 0),
                 Sensi.RE.out="Sensi_RE_out.DMP", #Saved file of relative errors
                 CI=0.95, #Confidence interval box based on the reference model
                 TRP.in=-1, #Target relative abundance value
                 LRP.in=-1, #Limit relative abundance value
                 sensi_xlab="Sensitivity scenarios", #X-axis label
                 ylims.in=c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1), #Y-axis label
                 plot.figs=c(1,1,1,1,1,1), #Which plots to make/save? 
                 #sensi.type.breaks=c(4.5, 6.5, 9.5), #vertical breaks that can separate out types of sensitivities
                 #anno.x=c(3, 5.5, 8, 10.5), # Vertical positioning of the sensitivity types labels
                 #anno.y=c(0.83,0.80,0.85,0.9), # Horizontal positioning of the sensitivity types labels
                 #anno.lab=c("Parameters", "Data Weighting", "Selectivity", "Index"), #Sensitivity types labels
                 horizontal = TRUE) 


###################################################################################
# Create a Table of Results
###################################################################################

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_7, sens_8, sens_9))
modelnames <- c("Base Model",
                "Estimate M",
                "Estimate h", 
                "Estimate M & h",
                "No Rec. Devs.",
                "CPFV Selectivity Asym.",
                "Growth Platoons",
                "No Added Variance",
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
                       file = "C:/Assessments/2023/copper_rockfish_2023/documents/sca/tex_tables/sensitivities_1_final.tex")


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
                       file = "C:/Assessments/2023/copper_rockfish_2023/documents/sca/tex_tables/sensitivities_2_final.tex")



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
                       file = "C:/Assessments/2023/copper_rockfish_2023/documents/sca/tex_tables/sensitivities_3_final.tex")
