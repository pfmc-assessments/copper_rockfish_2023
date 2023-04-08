###############################################
# Southern Modeling Explorations
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

growth <- SS_output(file.path(wd, "0.1_est_growth"))
SS_plots(growth)

rec_devs <- SS_output(file.path(wd, "0.2_rec_devs"))
SS_plots(rec_devs)
