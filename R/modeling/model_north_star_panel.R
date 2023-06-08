

###############################################
# Northern Modeling STAR panel
################################################

library(r4ss)
area <- "nca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <-  "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)
setwd(wd)




#===============================================================================
# STAR panel requests
#===============================================================================
# Request #2
star_start <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch"))
star_request_2 <- SS_output(file.path(wd, "9.13_star_request_2"))
SS_plots(star_request_2)
modelnames <- c("9.12 STAR start", "Request 2 age-6 selex")
mysummary <- SSsummarize(list(star_start, star_request_2))


SSplotComparisons(mysummary,
                  filenameprefix = "START starting model_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.2,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)



#===============================================================================
# Request #4
star_start <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch"))
star_request_4 <- SS_output(file.path(wd, "9.13_star_request_4"))
SS_plots(star_request_4)
modelnames <- c("9.12 STAR start", "Request 4 CCFRP q time block")
mysummary <- SSsummarize(list(star_start, star_request_4))


SSplotComparisons(mysummary,
                  filenameprefix = "STAR request 4_",
                  legendlabels = modelnames,   
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), #,'star_request_4'), 
                  legendloc = "topright", 
              #    subplots = c(1, 3, 11, 13), 
                  pdf = TRUE)

 tune_comps(replist = star_request_4, dir = file.path(wd, "9.13_star_request_4"), 
            option = "Francis", write = TRUE, allow_up_tuning = TRUE)

###################################################################################
# Create a Table of Results
###################################################################################
x <- mysummary
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


write.csv(out, file = file.path(wd, "9.13_star_request_4", "base_request4.csv"))





# #===============================================================================
# # Exploratory remove first part of CCFRP index
# star_ccfrp2017_Start <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch_dev0"))
# SS_plots(star_ccfrp2017_Start)
# 
# #freeL2 <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch_freeL2"))
# #SS_plots(freeL2)
# 
# tune_comps(replist = star_ccfrp2017_Start, dir = file.path(wd, "9.12_revised_sebastes_2021_catch - Copy"), 
#            option = "Francis", write = TRUE, allow_up_tuning = TRUE)
# 
# 
# 
# # look at devs
# star_start <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch"))
# star_start_dev0 <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch_dev0"))
# star_start_dev0_sigma.6 <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch_dev0_sigma.6"))
# modelnames <- c("9.12 STAR start", "Dev sum 0", "Dev sum 0 sigma .6")
# mysummary <- SSsummarize(list(star_start, star_start_dev0, star_start_dev0_sigma.6))
# 
# 
# SSplotComparisons(mysummary,
#                   filenameprefix = "STAR dev 0_",
#                   legendlabels = modelnames,   
#                   ylimAdj = 1.1,
#                   plotdir = file.path(wd, '_plots'), #,'star_request_4'), 
#                   legendloc = "topright", 
#                   #    subplots = c(1, 3, 11, 13), 
#                   pdf = TRUE)
