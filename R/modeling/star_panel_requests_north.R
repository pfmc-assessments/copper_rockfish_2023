###############################################
# STAR Panel Requests: South of Point Conception
################################################

library(r4ss)
area <- "nca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)

base <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch"))

# STAR Panel Request 3: Sigma R
sigmar <- SS_output(file.path(wd, "_sensitivities", "9.12_revised_sebastes_2021_catch_sigmaR_1.0"))

modelnames <- c("Base", "SigmaR = 1.0")
mysummary <- SSsummarize(list(base, sigmar))

SSplotComparisons(mysummary,
                  filenameprefix = "request_3_north_sigmaR_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  subplot = c(2, 4, 10, 12), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)

growth_selex <- SS_output(file.path(wd, "9.13_star_request_2"))


modelnames <- c("Base", "Growth Selectivity age 6+ = 1")
mysummary <- SSsummarize(list(base, growth_selex))

SSplotComparisons(mysummary,
                  filenameprefix = "request_2_north_growth_selex_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.1,
                  plotdir = file.path(wd, '_plots'), 
                  legendloc = "topright", 
                  subplot = c(2, 4, 10, 12), 
                  btarg = -1,
                  minbthresh = -1,
                  print = TRUE)
