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
base_model = "8.7_centered_m_h_est"

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

sens_list <- c("est_m", #1
              "est_linf", #2
              "est_cv2", #3
              "recdevs", #4
              "dw_dm", #5
              "dw_mi") #6

sens_list2 =  c("com_asym", #1
                "rec_asym", #2
                "all_asym", #3
                "no_hkl_2", #4
                "recfin_cpfv_no_var_corrected", #5
                "council_cpfv", #6
                "council_cpfv_selex_both_block_4") #7

data_sens <- c("fix_growth",
               "fix_growth_rm_ages",
               "rm_hkl",
               "rm_rov",
               "rm_ccfrp",
               "rm_coop")

#model.list <- paste0(base_model, "_", sens_list)
#model.list2 <- paste0(base_model, "_", sens_list2)
model_list <- paste0(base_model, "_", data_sens)

#out.list = NULL	
base   <- SS_output( base_loc, printstats = FALSE, verbose = FALSE) 

sens_1  <- SS_output( file.path(wd, model_list[1]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
tune_comps(replist = sens_1, option = "MI", write = TRUE, dir = file.path(getwd(), model_list[1]))
sens_2  <- SS_output( file.path(wd, model_list[2]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
sens_3  <- SS_output( file.path(wd, model_list[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_4  <- SS_output( file.path(wd, model_list[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_5  <- SS_output( file.path(wd, model_list[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_6  <- SS_output( file.path(wd, model_list[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)

modelnames <- c("Base Model",
                "Fix Growth",
                "Fix Growth Remove Ages", 
                "Remove NWFSC HKL",
                "Remove ROV",
                "Remove CCFRP",
                "Remove Coop.")


#modelnames <- c("Base Model",
#                "Est. M (f)", 
#                #"Est. Linf",
#                "Est. CV Old", 
#                "Est. Rec. Devs.",
#                "DM DW",
#                "DM MI")
#
#modelnames2 <- c("Base Model",
#                 "Com. Asymptotic Selectivity", 
#                 "Rec. Asymptotic Selectivity",
#                 "Com. & Rec. Asymptotic Selectivity", 
#                 "Remove HKL",
#                 "2013 RecFIN and CPFV Indices",
#                 "Additional CPFV Lengths",
#                 "CPFV Lengths and Selectivity Blocks")

x <- SSsummarize(list(base, sens_1, sens_3, sens_4, sens_5, sens_6))
#x2 <- SSsummarize(list(base, sens_7, sens_8, sens_9, sens_10, sens_11, sens_12, sens_13))

SSplotComparisons(x, 
                  endyrvec = 2023, 
                  legendlabels = modelnames, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_model, "_final_1_"),
                  #subplot = c(2,4), 
                  print = FALSE,
                  pdf = TRUE)

#SSplotComparisons(x2, 
#                  endyrvec = 2021, 
#                  legendlabels = modelnames2, 
#                  plotdir = file.path(getwd(), '_plots'), 
#                  legendloc = "topright", 
#                  ylimAdj = 1.25,
#                  filenameprefix = paste0(base_model, "_final_2_"),
#                  subplot = c(2,4), 
#                  print = TRUE,
#                  pdf = FALSE)


###################################################################################
# Jason Style Sensitivity Figure
###################################################################################

modelnames <- c("Base",
                "Est. M (f)", 
                "Est. CV Old", 
                "Est. Rec. Devs.",
                "DM DW",
                "DM MI",
                "Com. Asymptotic Selectivity", 
                "Rec. Asymptotic Selectivity",
                "Com. & Rec. Asymptotic Selectivity", 
                "2013 RecFIN Index",
                "2013 CPFV Index")

x <- SSsummarize(list(base, sens_1, sens_3, sens_4, sens_5, sens_6,
                      sens_7, sens_8, sens_9, sens_11, sens_12))

wd_dat <- file.path(paste0(wd,"/_plots")) 
# Sensitivity figure is something I adapted from Jason's Original that is in r4ss (SS_Sensi_plot)
# Here is where my version can be found: https://github.com/chantelwetzel-noaa/dover_sole_2021/blob/master/code/sensi_plot_Dover.R
Sensi_plot_dover(model.summaries=x,
                 dir = wd_dat,
                 current.year=2021,
                 mod.names=modelnames, #List the names of the sensitivity runs
                 likelihood.out = c(0, 1, 0),
                 Sensi.RE.out="Sensi_RE_out.DMP", #Saved file of relative errors
                 CI=0.95, #Confidence interval box based on the reference model
                 TRP.in=0.40, #Target relative abundance value
                 LRP.in=0.25, #Limit relative abundance value
                 sensi_xlab="Sensitivity scenarios", #X-axis label
                 ylims.in=c(-1,1,-1,1,-1,1,-1,1,-1,1,-1,1,-1,1), #Y-axis label
                 plot.figs=c(1,1,1,1,1,1), #Which plots to make/save? 
                 sensi.type.breaks=c(4.5, 6.5, 9.5), #vertical breaks that can separate out types of sensitivities
                 anno.x=c(3, 5.5, 8, 10.5), # Vertical positioning of the sensitivity types labels
                 anno.y=c(0.83,0.80,0.85,0.9), # Horizontal positioning of the sensitivity types labels
                 anno.lab=c("Parameters", "Data Weighting", "Selectivity", "Index"), #Sensitivity types labels
                 horizontal = TRUE) 



###################################################################################
# Create a Table of Results
###################################################################################
x = mysummary
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

t = table_format(x = out,
                 caption = 'Sensitivities relative to the base model.',
                 label = 'sensitivities-1',
                 longtable = TRUE,
                 font_size = 9,
                 digits = 3,
                 landscape = TRUE,
                 col_names = modelnames)

kableExtra::save_kable(t,
                       file = "C:/Assessments/2021/copper_rockfish_2021/write_up/s_ca/tex_tables/sensitivities_1_final.tex")