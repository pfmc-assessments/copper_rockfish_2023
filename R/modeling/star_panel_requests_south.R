###############################################
# STAR Panel Requests: South of Point Conception
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

base <- SS_output(file.path(wd, "14.3_revised_pre-star_base"))

# STAR Panel Request 3: Sigma R
sigmar <- SS_output(file.path(wd, "_sensitivities", "14.4_base_sebastes_2021_catches_sigmaR_0.80"))

modelnames <- c("Base", "SigmaR = 0.80")
mysummary <- SSsummarize(list(base, sigmar))

SSplotComparisons(mysummary,
                  filenameprefix = "request_3_south_sigmaR_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  subplot = c(2, 4, 10, 12), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)
