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
              "dirichlet", #6
              "mi") #7

model_list2 =  c("rm_ages", #1
                "rm_coop", #2
                "rm_wcgbt", #3
                "rm_ccfrp", #4
                "rm_rov", #5
                "rm_hkl", #6
                "rm_surveys", #7
                "rm_fishery_indices") #8

model_list <- paste0(base_model, "_", model_list)
model_list2 <- paste0(base_model, "_", model_list2)

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
sens2_7  <- SS_output( file.path(wd, model_list2[7]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens2_8  <- SS_output( file.path(wd, model_list2[8]), printstats = FALSE, verbose = FALSE, covar = FALSE)


modelnames <- c("Base Model",
               "Estimate M",
               "Estimate h", 
               "Estimate M & h",
               "No Rec. Devs.",
               "CPFV Selectivity Asym.",
               "Dirichlet DW", 
               "McAllister-Ianelli DW")

modelnames2 <- c("Base Model",
                "Rm. All Ages",
                "Rm. Coop. Ages", 
                "Rm. WCGBT Ages",
                "Rm. CCFRP",
                "Rm. CDFW ROV",
                "Rm. NWFSC HKL",
                "Rm. All Surveys",
                "Rm. CPFV & PR Indices")

x <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_7))
x2 <- SSsummarize(list(base, sens2_1, sens2_2, sens2_3, sens2_4, sens2_5, sens2_6, sens2_7, sens2_8))

SSplotComparisons(x, 
                  endyrvec = 2023, 
                  legendlabels = modelnames, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_model, "_final_1_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

SSplotComparisons(x, 
                  endyrvec = 2023, 
                  legendlabels = modelnames, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topleft", 
                  filenameprefix = paste0(base_model, "_final_1_"),
                  subplot = c(11), 
                  print = TRUE)

SSplotComparisons(x2, 
                  endyrvec = 2023, 
                  legendlabels = modelnames2, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  ylimAdj = 1.25,
                  filenameprefix = paste0(base_model, "_final_2_"),
                  subplot = c(2,4), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

SSplotComparisons(x2, 
                  endyrvec = 2023, 
                  legendlabels = modelnames2, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topleft", 
                  ylimAdj = 1.25,
                  filenameprefix = paste0(base_model, "_final_2_"),
                  subplot = c(11), 
                  print = TRUE)

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
