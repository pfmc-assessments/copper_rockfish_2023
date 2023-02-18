
# Round
rnd <- 1

# Virgin SSB
sb0 = format(round(model$derived_quants[model$derived_quants$Label == "SSB_Virgin", 2], rnd), scientific = FALSE, big.mark = ",") 

# Spawning Biomass at the biomass target
sb_targ = round(index$btarg * model$derived_quants[model$derived_quants$Label == "SSB_Virgin", 2], rnd)

# Final Year SSB
endyr <- model$endyr + 1
sb_final = format(round(model$derived_quants[model$derived_quants$Label == paste0("SSB_", endyr), 2], rnd), scientific = FALSE, big.mark = ",") 
low = model$derived_quants[model$derived_quants$Label == paste0("SSB_", endyr), 2] - qnorm(1-(1-0.95)/2) * model$derived_quants[model$derived_quants$Label == paste0("SSB_", endyr), 3]
hi = model$derived_quants[model$derived_quants$Label == paste0("SSB_", endyr), 2] + qnorm(1-(1-0.95)/2) * model$derived_quants[model$derived_quants$Label == paste0("SSB_", endyr), 3]
sb_final_low = format(round(low, rnd), scientific = FALSE, big.mark = ",") 
sb_final_hi  = format(round(hi, rnd),  scientific = FALSE, big.mark = ",") 

# Final Year Depletion
depl_final = round(model$derived_quants[model$derived_quants$Label == paste0("Bratio_", endyr), 2], digits = 3) 
low = model$derived_quants[model$derived_quants$Label == paste0("Bratio_", endyr), 2] - qnorm(1-(1-0.95)/2) * model$derived_quants[model$derived_quants$Label == paste0("Bratio_", endyr), 3]
hi = model$derived_quants[model$derived_quants$Label == paste0("Bratio_", endyr), 2] + qnorm(1-(1-0.95)/2) * model$derived_quants[model$derived_quants$Label == paste0("Bratio_", endyr), 3]
depl_final_low = round(low, digits = 3) 
depl_final_hi  = round(hi,  digits = 3) 

startyr <- model$startyr
ind = grep("Bratio_", model$derived_quants$Label)
all = model$derived_quants[ind, c("Label","Value")]
min = sort(all$Value, index.return = TRUE)$ix
depl_min = round(all[min[1], "Value"],2)
depl_min_yr = substring(all[min[1], "Label"], 8)


# SPR
start = which(model$derived_quants$Label == paste0("SPRratio_", startyr))
end = which(model$derived_quants$Label == paste0("SPRratio_", model$endyr))
all = model$derived_quants[start:end, c("Label","Value")]
max = sort(all$Value, index.return = TRUE, decreasing = TRUE)$ix
spr_max = round(all[max[1], "Value"],2)
spr_max_yr = substring(all[max[1], "Label"], 10)

grab = which(all$Label == paste0("SPRratio_", endyr - 10))
spr_recent_10 = round(all[grab:nrow(all), "Value"], 2)

# Exploitation
start = which(model$derived_quants$Label == paste0("F_", startyr))
end = which(model$derived_quants$Label == paste0("F_", model$endyr))
all = model$derived_quants[start:end, c("Label","Value")]
grab = which(all$Label == paste0("F_", endyr-10))
exploit_recent_10 = round(all[grab:nrow(all), "Value"],2)

OFL_sigma <- model$OFL_sigma
