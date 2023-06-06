

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
                  plotdir = file.path(wd, '_plots','star_request_4'), 
                  legendloc = "topright", 
                  subplots = c(1, 3, 11, 13), 
                  png = TRUE)


#===============================================================================
# Exploratory remove first part of CCFRP index
star_ccfrp2017_Start <- SS_output(file.path(wd, "9.12_revised_sebastes_2021_catch_ccfrp2017"))
SS_plots(star_ccfrp2017_Start)




