
do_projections <- function(
    model1_dir = NULL, 
    model2_dir = NULL,
    sigma = c(0.50, 0.50),
    pstar = 0.45,
    fore_years = 2023:2034,
    prop = c(0.29, 0.71),
    fleet1_num = 1:4,
    fleet2_num = 1:4,
    model1_fleets = c(0.10, 0.10, 0.60, 0.20),
    model2_fleets = c(0.10, 0.10, 0.30, 0.50),
    hcr = c(0.4, 0.1)
){

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
  
  output <- NULL
  
  # Calculate buffer
  buffer1 <- PEPtools::get_buffer(years = (fore_years[1]-2):max(fore_years), sigma = sigma[1], pstar = pstar)
  buffer1 <- buffer1[3:nrow(buffer1), 2]
  buffer2 <- PEPtools::get_buffer(years = (fore_years[1]-2):max(fore_years), sigma = sigma[2], pstar = pstar)
  buffer2 <- buffer2[3:nrow(buffer2), 2]
  ind <- 1
  
  startyr <- model1$startyr 
    
  for (y in fore_years){
    # Calculate the pooled depletion
    sb01 <- model1$timeseries[model1$timeseries$Yr == startyr, "SpawnBio"] 
    sb02 <- model2$timeseries[model2$timeseries$Yr == startyr, "SpawnBio"]

    sby1 <- model1$timeseries[model1$timeseries$Yr == y, "SpawnBio"] 
    sby2 <-  model2$timeseries[model2$timeseries$Yr == y, "SpawnBio"]
    sby <- sby1 + sby2
    depl <- sby / (sb01 + sb02)
    depl1 <- sby1 / sb01
    depl2 <- sby2 / sb02
    
    ofl1 <- model1$derived_quants[model1$derived_quants$Label == paste0("OFLCatch_",y), "Value"] 
    ofl2 <- model2$derived_quants[model2$derived_quants$Label == paste0("OFLCatch_",y), "Value"] 
    ofl  <- ofl1 + ofl2
    
    abc1 <- ofl1 * buffer1[ind]
    abc2 <- ofl2 * buffer2[ind]
    abc  <- abc1 + abc2 
    
    if( depl < 0.40) {
      acl <- abc * (hcr[1]/ (hcr[1] - hcr[2])) * (depl - hcr[2]) / depl
    } else {
      acl <- abc
    }
    
    if (prop[1] < 0){
      acl1 <- ofl1/ofl * acl
      acl2 <- (1 - ofl1/ofl) * acl  
      prop1 <- ofl1/ofl; prop2 <- 1 - prop1
    } else {
      acl1 <- prop[1] * acl
      acl2 <- prop[2] * acl  
      prop1 <- prop[1]
      prop2 <- prop[2]
    }

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
    ind <- ind + 1
    
    output <- rbind(output, c(y, ofl, ofl1, ofl2, abc, abc1, abc2, acl, sby, sby1, sby2, depl, depl1, depl2, 
                              prop1, prop2, acl1, acl2))

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
  
  colnames(output) <- c("Year", "OFL", "OFL1", "OFL2", "ABC", "ABC1", "ABC2", "ACL", "SO", "SO1", "SO2", "Depl.", "Depl1", "Depl2", "ACL Prop.Model1", "ACL Prop. Model2",  "Removals Model1", "Removals Model2")
  write.csv(output, file = file.path(model1_dir, "Projection_Values.csv"), row.names = FALSE)
  write.csv(output, file = file.path(model2_dir, "Projection_Values.csv"), row.names = FALSE)
}


#model1_dir <- "C:/Assessments/2023/copper_rockfish_2023/models/sca/_decision_table/pstar_45/15.0_south_post_star_base"
#model2_dir <- "C:/Assessments/2023/copper_rockfish_2023/models/nca/_decision_table/pstar_45/10.0_north_post_star_base"
#
#
#do_projections(
#    sigma = c(0.50, 0.50),
#    pstar = 0.45,
#    model1_dir = model1_dir, 
#    model2_dir = model2_dir,
#    fore_years = 2025:2034,
#    prop = c(-1, -1),
#    fleet1_num = 1:4,
#    fleet2_num = 1:4,
#    model1_fleets = c(0.04, 0.03, 0.72, 0.21),
#    model2_fleets = c(0.03, 0.05, 0.38, 0.54),
#    hcr = c(0.4, 0.1))
