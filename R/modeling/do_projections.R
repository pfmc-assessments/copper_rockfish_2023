
do_projections <- function(
    model1_dir = NULL, 
    model2_dir = NULL,
    fore_years = 2025:2034,
    prop = c(0.50, 0.50),
    fleet1_num = 1:4,
    fleet2_num = 1:4,
    model1_fleets = c(0.10, 0.10, 0.60, 0.20),
    model2_fleets = c(0.10, 0.10, 0.30, 0.50),
    hcr = c(0.4, 0.1)
){
  
  model1_dir <- "C:/Assessments/2023/copper_rockfish_2023/models/sca/forecast_test/5.2_crfs_pr_index"
  model2_dir <- "C:/Assessments/2023/copper_rockfish_2023/models/nca/forecast_test/5.2_crfs_pr_index"

  # Turn off the estimation for both models
  starter = r4ss::SS_readstarter(file.path(model1_dir, "starter.ss"), verbose = FALSE)
  use_par1 = starter$init_values_src 
  starter$init_values_src = 1
  max_phase1 = starter$last_estimation_phase
  starter$last_estimation_phase = 0
  r4ss::SS_writestarter(starter, dir = model1_dir, overwrite = TRUE, verbose = FALSE)
  
  starter = r4ss::SS_readstarter(file.path(model2_dir, "starter.ss"), verbose = FALSE)
  use_par2 = starter$init_values_src 
  starter$init_values_src = 1
  max_phase2 = starter$last_estimation_phase
  starter$last_estimation_phase = 0
  r4ss::SS_writestarter(starter, dir = model2_dir, overwrite = TRUE, verbose = FALSE)
  
  # Turn off the harvest control rule
  fore = r4ss::SS_readforecast(file.path(model1_dir, "forecast.ss"), verbose = FALSE)
  BforconstantF <- fore$BforconstantF
  fore$BforconstantF <- 0.02
  BfornoF <- fore$BfornoF
  fore$BfornoF <- 0.01
  r4ss::SS_writeforecast(fore, dir = model1_dir, overwrite = TRUE)
  
  fore = r4ss::SS_readforecast(file.path(model2_dir, "forecast.ss"), verbose = FALSE)
  fore$BforconstantF <- 0.02
  fore$BfornoF <- 0.01
  r4ss::SS_writeforecast(fore, dir = model2_dir, overwrite = TRUE)
  
  setwd(model1_dir)
  shell("ss -nohess -maxfun 0 > output.txt 2>&1")
  setwd(model2_dir)
  shell("ss -nohess -maxfun 0 > output.txt 2>&1")
  
  model1 <- r4ss::SS_output(model1_dir, covar = FALSE, verbose = FALSE, printstats = FALSE)
  model2 <- r4ss::SS_output(model2_dir, covar = FALSE, verbose = FALSE, printstats = FALSE)
  
  model1_summary <- model2_summary <- NULL
  
  for (y in fore_years){
    # Calculate the pooled depletion
    sb0 <- model1$timeseries[model1$timeseries$Yr == startyr, "SpawnBio"] + 
      model2$timeseries[model2$timeseries$Yr == startyr, "SpawnBio"]
    sby <- model1$timeseries[model1$timeseries$Yr == y, "SpawnBio"] +
      model2$timeseries[model2$timeseries$Yr == y, "SpawnBio"]
    depl <- sby / sb0
    
    abc <- model1$derived_quants[model1$derived_quants$Label == paste0("ForeCatch_",y), "Value"] +
      model2$derived_quants[model2$derived_quants$Label == paste0("ForeCatch_",y), "Value"]
    
    acl <- abc * (hcr[1]/ (hcr[1] - hcr[2])) * (depl - hcr[2]) / depl
    
    acl1 <- prop[1] * acl
    acl2 <- prop[2] * acl
    
    # Set the forecast catch in the first model
    fore <- r4ss::SS_readforecast(file.path(model1_dir, "forecast.ss"), verbose = FALSE)
    add_catch <- cbind(y, 1, fleet1_num, round(acl1 * model1_fleets, 2))
    colnames(add_catch) <- c("Year", "Seas", "Fleet", "Catch or F")
    fore$ForeCatch <- rbind(fore$ForeCatch, add_catch)
    r4ss::SS_writeforecast(fore, dir = model1_dir, overwrite = TRUE)
    
    fore <- r4ss::SS_readforecast(file.path(model2_dir, "forecast.ss"), verbose = FALSE)
    add_catch <- cbind(y, 1, fleet2_num, round(acl2 * model2_fleets, 2))
    colnames(add_catch) <- c("Year", "Seas", "Fleet", "Catch or F")
    fore$ForeCatch <- rbind(fore$ForeCatch, add_catch)
    r4ss::SS_writeforecast(fore, dir = model2_dir, overwrite = TRUE)
    
    setwd(model1_dir)
    shell("ss -nohess -maxfun 0 > output.txt 2>&1")
    setwd(model2_dir)
    shell("ss -nohess -maxfun 0 > output.txt 2>&1")
    
    model1 <- r4ss::SS_output(model1_dir, covar = FALSE, verbose = FALSE, printstats = FALSE)
    model2 <- r4ss::SS_output(model2_dir, covar = FALSE, verbose = FALSE, printstats = FALSE)
    
    model1_summary <- rbind(model1_summary, c(y, abc, acl, depl, prop[1], sum(acl1)))
    model2_summary <- rbind(model2_summary, c(y, abc, acl, depl, prop[2], sum(acl2)))
  }
  
  # Reset the harvest control rule
  fore = r4ss::SS_readforecast(file.path(model1_dir, "forecast.ss"), verbose = FALSE)
  fore$BforconstantF <- BforconstantF
  fore$BfornoF <- BfornoF
  r4ss::SS_writeforecast(fore, dir = model1_dir, overwrite = TRUE)
  
  fore = r4ss::SS_readforecast(file.path(model2_dir, "forecast.ss"), verbose = FALSE)
  fore$BforconstantF <- BforconstantF
  fore$BfornoF <- BfornoF
  r4ss::SS_writeforecast(fore, dir = model2_dir, overwrite = TRUE)
  
  # Change the max phase back to the original value
  starter = r4ss::SS_readstarter(file.path(model1_dir, "starter.ss"))
  starter$init_values_src = use_par1
  starter$last_estimation_phase = max_phase1
  r4ss::SS_writestarter(starter, dir = model1_dir, overwrite = TRUE, verbose = FALSE)
  
  # Change the max phase back to the original value
  starter = r4ss::SS_readstarter(file.path(model2_dir, "starter.ss"))
  starter$init_values_src = use_par2
  starter$last_estimation_phase = max_phase2
  r4ss::SS_writestarter(starter, dir = model2_dir, overwrite = TRUE, verbose = FALSE)
  
  colnames(model1_summary) <- colnames(model2_summary) <- c("Year", "ABC", "ACL", "Depl.", "ACL Prop.", "Removals")
  write.csv(model1_summary, file = file.path(model1_dir, "Projection_Values.csv"), row.names = FALSE)
  write.csv(model2_summary, file = file.path(model2_dir, "Projection_Values.csv"), row.names = FALSE)
}
