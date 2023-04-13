########################################################
# Create Sensitivity table for Copper Rockfish 2023
# North of Pt Conception
# written by : Chantel Wetzel
########################################################

library(r4ss)
library(sa4ss)

# The script below is not yet updated to reflect the suite of 
# sensitivities that will be done with the 2023 model

area = "nca"
base_model = "0.1_init_model"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area, "_sensitivities")
setwd(wd)
out.dir <- wd

base.loc <- file.path(user_dir, area, base_model)

###################################################################################
# Pull in each model in using r4ss
###################################################################################

model.list <- c(paste0(base_model, "_no_recdevs"), #1
                paste0(base_model, "_mi"), #2
                paste0(base_model, "_dm"), #3
                paste0(base_model, "_est_linf"), #4
                paste0(base_model, "_est_cv2"), #5
                paste0(base_model, "_est_m"), #6
                paste0(base_model, "_com_asym"), #7
                paste0(base_model, "_spline"), #8
                paste0(base_model, "_no_blocks"),  #9
                paste0(base_model, "_rec_block"), #10
                paste0(base_model, "_index_cpfv"), #11
                paste0(base_model, '_no_blocks_asym'), #12
                paste0(base_model, "_no_blocks_asym_dw"), #13
                paste0(base_model, "_council_cpfv"), #14
                paste0(base_model, "_council_cpfv_all_selex_both_block_4")) #15

out.list = NULL   
base   = SS_output( base.loc, printstats = FALSE, verbose = FALSE) 
sens_1  = SS_output( file.path(wd, model.list[1]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
sens_2  = SS_output( file.path(wd, model.list[2]), printstats = FALSE, verbose = FALSE, covar = FALSE) 
sens_3  = SS_output( file.path(wd, model.list[3]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_4  = SS_output( file.path(wd, model.list[4]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_5  = SS_output( file.path(wd, model.list[5]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_6  = SS_output( file.path(wd, model.list[6]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_7  = SS_output( file.path(wd, model.list[7]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_8  = SS_output( file.path(wd, model.list[8]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_9  = SS_output( file.path(wd, model.list[9]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_10  = SS_output( file.path(wd, model.list[10]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_11  = SS_output( file.path(wd, model.list[11]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_12  = SS_output( file.path(wd, model.list[12]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_13  = SS_output( file.path(wd, model.list[13]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_14  = SS_output( file.path(wd, model.list[14]), printstats = FALSE, verbose = FALSE, covar = FALSE)
sens_15  = SS_output( file.path(wd, model.list[15]), printstats = FALSE, verbose = FALSE, covar = FALSE)


modelnames1 <- c("Base Model",
                 "No Rec. Devs.",
                 "MI DW",
                 "DM DW",
                 "Estimate Linf",
                 "Estimate CV Old",
                 "Estimate M (f)",
                 "2013 CPFV Onboard Index")

modelnames2 <- c("Base Model",
                 "Com. Asym. Select.", #7
                 "Com. Spline Select.",
                 "Com. No Blocks and Asym.",
                 "Early Block in Rec. Selectivity",
                 "Early CPFV Lengths",
                 "Early CPFV Lengths and Selectivity Blocks")

x1 <- SSsummarize(list(base, sens_1, sens_2, sens_3, sens_4, sens_5, sens_6, sens_11))
x2 <- SSsummarize(list(base, sens_7, sens_8, sens_9, sens_10, sens_14, sens_15))

SSplotComparisons(x1, endyrvec = 2023, 
                  legendlabels = modelnames1, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  filenameprefix = paste0(base_model, "_1_final_"),
                  subplot = c(2,4), 
                  print = TRUE, 
                  pdf = FALSE)

SSplotComparisons(x2, endyrvec = 2023, 
                  legendlabels = modelnames2, 
                  plotdir = file.path(getwd(), '_plots'), 
                  legendloc = "topright", 
                  ylimAdj = 1.15,
                  filenameprefix = paste0(base_model, "_2_final_"),
                  subplot = c(2,4, 12), 
                  print = TRUE, 
                  pdf = FALSE)

###################################################################################
# Create a Table of Results
###################################################################################

x = x1
modelnames = modelnames1
ii = 1:length(modelnames)
n = length(modelnames)
out<- matrix(NA, 2, max(ii))

out = rbind(
  as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]),
  #as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2021", 1:n]),
  as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2021", 1:n]), 
  as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_p_1_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_p_1_Mal_GP_1", 1:n]),
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
                  #"Age Likelihood",
                  "Recruitment Likelihood",
                  "Forecast Recruitment Likelihood",
                  "Parameter Priors Likelihood",
                  "log(R0)",
                  "SB Virgin",
                  "SB 2020",
                  "Fraction Unfished 2021",
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


write.csv(out, file = file.path(out.dir, paste0(base_model, "_sensitivities_final_1.csv")))

t = table_format(x = out,
                 caption = 'Sensitivities relative to the base model.',
                 label = 'sensitivities',
                 longtable = TRUE,
                 font_size = 9,
                 digits = 3,
                 landscape = TRUE,
                 col_names = modelnames)

kableExtra::save_kable(t,
                       file = file.path("C:/Assessments/2021/copper_rockfish_2021/write_up/n_ca/tex_tables/sensitivities_final_1.tex"))


x = x2
modelnames = modelnames2
ii = 1:length(modelnames)
n = length(modelnames)
out<- matrix(NA, 2, max(ii))

out = rbind(
  as.numeric(x$likelihoods[x$likelihoods$Label == "TOTAL",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Survey",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Length_comp",1:n]),
  #as.numeric(x$likelihoods[x$likelihoods$Label == "Age_comp",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Recruitment",1:n]), 
  as.numeric(x$likelihoods[x$likelihoods$Label == "Forecast_Recruitment",1:n]),
  as.numeric(x$likelihoods[x$likelihoods$Label == "Parm_priors",1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_LN(R0)", 1:n]), 
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_Virgin", 1:n]),
  as.numeric(x$SpawnBio[x$SpawnBio$Label == "SSB_2021", 1:n]),
  as.numeric(x$Bratio[x$Bratio$Label == "Bratio_2021", 1:n]), 
  as.numeric(x$quants[x$quants$Label == "Dead_Catch_SPR", 1:n]),
  as.numeric(x$pars[x$pars$Label == "SR_BH_steep", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_p_1_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amin_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "L_at_Amax_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "VonBert_K_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_young_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "CV_old_Fem_GP_1", 1:n]),
  as.numeric(x$pars[x$pars$Label == "NatM_p_1_Mal_GP_1", 1:n]),
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
                  #"Age Likelihood",
                  "Recruitment Likelihood",
                  "Forecast Recruitment Likelihood",
                  "Parameter Priors Likelihood",
                  "log(R0)",
                  "SB Virgin",
                  "SB 2020",
                  "Fraction Unfished 2021",
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


write.csv(out, file = file.path(out.dir, paste0(base_model, "_sensitivities_final_2.csv")))

t = table_format(x = out,
                 caption = 'Sensitivities relative to the base model. The negative log-likelihood for the Early CPFV Lengths
                 and the Early CPFV Lengths and Selectivity Blocks sensitivities are not comparable with the base model since these sensitivities include additional data.',
                 label = 'sensitivities-2',
                 longtable = TRUE,
                 font_size = 9,
                 digits = 3,
                 landscape = TRUE,
                 col_names = modelnames)

kableExtra::save_kable(t,
                       file = file.path("C:/Assessments/2021/copper_rockfish_2021/write_up/n_ca/tex_tables/sensitivities_final_2.tex"))