#######################################################################################################
# Copper Rockfish: South of Pt. Conception
# Retrospective Squid plot
#######################################################################################################

library(r4ss)

# Specify the directory
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

model_dir <- file.path(user_dir, "models", "nca")
base_name <- "5.1_non_zero_centered_devs"
retro.folder <- file.path(user_dir, "models", "nca", paste0(base_name, "_retro"))


# Read in the retrospective runs
base <- SS_output(file.path(model_dir, base_name))
retro1 = SS_output(file.path(retro.folder, "retro", "retro-1"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro2 = SS_output(file.path(retro.folder, "retro", "retro-2"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro3 = SS_output(file.path(retro.folder, "retro", "retro-3"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro4 = SS_output(file.path(retro.folder, "retro", "retro-4"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro5 = SS_output(file.path(retro.folder, "retro", "retro-5"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro6 = SS_output(file.path(retro.folder, "retro", "retro-6"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro7 = SS_output(file.path(retro.folder, "retro", "retro-7"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro8 = SS_output(file.path(retro.folder, "retro", "retro-8"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro9 = SS_output(file.path(retro.folder, "retro", "retro-9"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro10 = SS_output(file.path(retro.folder, "retro", "retro-10"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro11 = SS_output(file.path(retro.folder, "retro", "retro-11"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro12 = SS_output(file.path(retro.folder, "retro", "retro-12"), printstats = FALSE, verbose = FALSE, covar = FALSE)


modelnames <- c("Base Model", paste0("Retro -", 1:12))

# Summarize the results across models
mysummary <- SSsummarize(list(base, retro1, retro2, retro3, retro4, retro5,
                              retro6, retro7, retro8, retro9, retro10, retro11, retro12))

HandyCode::pngfun(wd = retro.folder, file = "Squid_RecDevs.png", h = 7, w = 7)
SSplotRetroRecruits(retroSummary = mysummary,
                    endyrvec = rev(2010:2023),
                    cohorts = 2000:2021,
                    ylim=NULL,uncertainty=FALSE,
                    labels=c('Recruitment deviation', 'Recruitment (billions)', 'relative to recent estimate', 'Age'),
                    main="",
                    mcmcVec=FALSE,devs=TRUE,
                    relative=FALSE,labelyears=TRUE,legend=FALSE,leg.ncols=4)
dev.off()


#SSplotComparisons(mysummary,
#                  filenameprefix = "Retrospective_",
#                  endyrvec = rev(2010:2023),
#                  legendlabels = modelnames, 	
#                  plotdir = retro.folder,
#                  pdf = TRUE)

#=====================================================================================================
# Create a table of parameter values to understand what is changing across retrospective runs
#=====================================================================================================

x <- mysummary
ii <- 1:length(modelnames)
n  <- length(modelnames)
out <- matrix(NA, 24, max(ii))

out <- rbind(
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

write.csv(out, file = file.path(retro.folder, paste0(base_name, "_retros.csv")))
