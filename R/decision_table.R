

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

###################################################################################################
# ACL P* = 0.45 and sigma = 0.50 for both areas
####################################################################################################

run_name = "ACL_removals"
south_dt_loc = file.path(wd, "_decision_table")
south_name = "15.0_south_post_star_base_projection"
south_loc = file.path(wd, south_name)

fore_catch <- read.csv(file.path(south_loc, "Projection_Values.csv"))
south_forecast <- fore_catch$Removals.Model1
fleet_percents = c(0.04,	0.03,	0.72,	0.21)

years = 2025:2034
fore.catch = NULL 
fleets <- 4

for(y in 1:length(years)){
  for(f in 1:fleets){
    calc = c(years[y], 1, f, fleet_percents[f]*south_forecast[y])
    fore.catch = rbind(fore.catch, calc)
  }
}
colnames(fore.catch) = c("Year", "Seas", "Fleet", "Catch or F")
rownames(fore.catch) = NULL
write.csv(fore.catch, file.path(south_dt_loc, paste0(south_name, "_", run_name, ".csv")), row.names = FALSE)



wd <- file.path(user_dir, "models", "nca")
run_name = "ACL_removals"
north_dt_loc = file.path(wd, "_decision_table")
north_name = "10.0_south_post_star_base_projection"
north_loc = file.path(wd, north_name)
north_forecast <- fore_catch$Removals.Model2
fleet_percents = c(0.03, 0.05,	0.38, 0.54)

years = 2025:2034
fore.catch = NULL 
fleets <- 4

for(y in 1:length(years)){
  for(f in 1:fleets){
    calc = c(years[y], 1, f, fleet_percents[f]*north_forecast[y])
    fore.catch = rbind(fore.catch, calc)
  }
}
colnames(fore.catch) = c("Year", "Seas", "Fleet", "Catch or F")
rownames(fore.catch) = NULL
write.csv(fore.catch, file.path(north_dt_loc, paste0(north_name, "_", run_name, ".csv")), row.names = FALSE)



# Grab the SO and depletion from the low state of nature
south_low <- r4ss::SS_output("C:/Assessments/2023/copper_rockfish_2023/models/sca/_decision_table/15.0_south_post_star_base_SR_parm[2]_decision_table_1.15_0.277_0.125")
north_low <- r4ss::SS_output("C:/Assessments/2023/copper_rockfish_2023/models/nca/_decision_table/10.0_north_post_star_base_SR_parm[2]_decision_table_1.15_0.3_0.125")


years <- 2023:2034
sb0 <- south_low$timeseries[south_low$timeseries$Yr == 1916, "SpawnBio"] + 
  north_low$timeseries[north_low$timeseries$Yr == 1916, "SpawnBio"]
sby <- south_low$timeseries[south_low$timeseries$Yr %in% years, "SpawnBio"] +
  north_low$timeseries[north_low$timeseries$Yr %in% years, "SpawnBio"]

sb <- sby
depl <- sby / sb0
out <- cbind(years, sb, depl)
write.csv(out, file.path(south_dt_loc, "low_state_of_nature.csv"))

# Grab the SO and depletion from the high state of nature
south_hi <- r4ss::SS_output("C:/Assessments/2023/copper_rockfish_2023/models/sca/_decision_table/15.0_south_post_star_base_SR_parm[2]_decision_table_1.15_0.277_0.875")
north_hi <- r4ss::SS_output("C:/Assessments/2023/copper_rockfish_2023/models/nca/_decision_table/10.0_north_post_star_base_SR_parm[2]_decision_table_1.15_0.3_0.875")

sb0 <- south_hi$timeseries[south_hi$timeseries$Yr == 1916, "SpawnBio"] + 
  north_hi$timeseries[north_hi$timeseries$Yr == 1916, "SpawnBio"]
sby <- south_hi$timeseries[south_hi$timeseries$Yr %in% years, "SpawnBio"] +
  north_hi$timeseries[north_hi$timeseries$Yr %in% years, "SpawnBio"]

sb <- sby
depl <- sby / sb0
out <- cbind(years, sb, depl)
write.csv(out, file.path(south_dt_loc, "high_state_of_nature.csv"))

south <- r4ss::SS_output("C:/Assessments/2023/copper_rockfish_2023/models/sca/15.0_south_post_star_base_projection")
north <- r4ss::SS_output("C:/Assessments/2023/copper_rockfish_2023/models/nca/10.0_north_post_star_base_projection")
sb0 <- south$timeseries[south$timeseries$Yr == 1916, "SpawnBio"] + 
  north$timeseries[north$timeseries$Yr == 1916, "SpawnBio"]
sby <- south$timeseries[south$timeseries$Yr %in% years, "SpawnBio"] +
  north$timeseries[north$timeseries$Yr %in% years, "SpawnBio"]

sb <- sby
depl <- sby / sb0
out <- cbind(years, sb, depl)
write.csv(out, file.path(south_dt_loc, "base_state_of_nature.csv"))
