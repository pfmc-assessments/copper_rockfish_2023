
pngfun <- function(doc_dir, file, w = 7, h = 7, pt = 12){
  file <- file.path(doc_dir, file)
  cat('writing PNG to',file,'\n')
  png(filename = file,
      width = w,
      height = h,
      units = 'in',
      res = 300, 
      pointsize = pt
  )
}

print.letter <- function(label="(a)",xy=c(0.1,0.925),...) {   #... means any new parameters are copied faithfully to text() below
  tmp <- par("usr")
  text.x <- tmp[1]+xy[1]*diff(tmp[1:2])   #x position, diff=difference
  text.y <- tmp[3]+xy[2]*diff(tmp[3:4])   #y position
  text(x=text.x, y=text.y, labels=label, ...)             
}

create_management_table <- function(
  management_dir = management_dir, 
  doc_dir = doc_dir, 
  doc_names = c("nca", 'sca'),
  years = 2012:2022,
  table_names = c("copper_ca_north.csv", "copper_ca_south.csv"),
  prefix = NULL) {
  
  if(!is.null(prefix)) { prefix <- paste0(prefix, "-")}
  
  # read in the management tables for north and south of 4010 in CA
  table1 <- read.csv(file.path(management_dir, table_names[1]))
  table2 <- read.csv(file.path(management_dir, table_names[2]))
  
  load(file.path(doc_dir, doc_names[1], "00mod.Rdata"))
  model1 <- model
  load(file.path(doc_dir, doc_names[2], "00mod.Rdata"))
  model2 <- model
  
  catch <- dead <- total_dead <- NULL
  for(f in 1:model1$nfishfleets){
    input_catch <- model1$timeseries[model1$timeseries$Yr %in% years, paste0("retain(B):_", f)]
    input_catch <- input_catch + model2$timeseries[model2$timeseries$Yr %in% years, paste0("retain(B):_", f)]
    catch <- cbind(catch, input_catch)
  }
  total_catch <- round(apply(catch, 1, sum), 2)
  
  ofl <- table1$OFL[table1$Year %in% years] + table2$OFL[table2$Year %in% years]
  acl <- table1$ACL[table1$Year %in% years] + table2$ACL[table2$Year %in% years]
  
  out <- data.frame(
    year = years, 
    ofl = ofl, 
    acl = acl, 
    catch = total_catch)
  col_names <- c("Year", "OFL (mt)", "ACL (mt)", "Catch (mt)")
  
  sa4ss::table_format(
    x = out,
    digits = 1,
    caption = "The species-specific Overfishing Limit (OFL) and Annual Catch Limit (ACL) allocated to California and the total catch (mt) in California waters by year.",
    label = paste0(prefix, "ca-management"),
    col_names = col_names
  )
}

create_biomass_table <- function(
    doc_dir = doc_dir, 
    prefix = NULL,
    doc_names = c("nca", 'sca'),
    years = 2013:2023) {
  
  if(!is.null(prefix)) { prefix <- paste0(prefix, "-")}
  
  load(file.path(doc_dir, doc_names[1], "00mod.Rdata"))
  model1 <- model
  load(file.path(doc_dir, doc_names[2], "00mod.Rdata"))
  model2 <- model
  
  find <- model1[["timeseries"]][["Yr"]] %in% years
  smry1 <- model1[["timeseries"]][["Bio_smry"]][find]
  tot_bio1 <- model1[["timeseries"]][["Bio_all"]][find]
  recruits1 <- model1[["timeseries"]][["Recruit_0"]][find]
  
  find <- model2[["timeseries"]][["Yr"]] %in% years
  smry2 <- model2[["timeseries"]][["Bio_smry"]][find]
  tot_bio2 <- model2[["timeseries"]][["Bio_all"]][find]
  recruits2 <- model2[["timeseries"]][["Recruit_0"]][find]
  
  sb1 <- model1$derived_quants[model1$derived_quants$Label %in% paste0("SSB_", years), "Value"]
  sb2 <- model2$derived_quants[model2$derived_quants$Label %in% paste0("SSB_", years), "Value"]
  sb0 <- model1$derived_quants[model1$derived_quants$Label == "SSB_Virgin", "Value"] +
    model2$derived_quants[model2$derived_quants$Label == "SSB_Virgin", "Value"]
  
  sb <- sb1 + sb2
  depl <- sb / sb0
  
  out <- data.frame(
    Year = years, 
    tot_bio = round(tot_bio1 + tot_bio2, 2),
    smry_bio = round(smry1 + smry2, 2),
    recr = round(recruits2 + recruits2, 2),
    SB = round(sb, 2), 
    Depl = round(depl, 3))
  col_names <- c("Year", "Total Biomass (mt)", "Total Biomass 3+ (mt)", "Age-0 Recruits", "Spawning Output", "Fraction Unfished")
  
  sa4ss::table_format(
    x = out,
    caption = "The estimated total biomass (mt), total biomass age 3+ (mt), age-0 recruits, and spawning ouput in number of billions of eggs across California and fraction unfished by year.",
    label = paste0(prefix, "ca-status"),
    col_names = col_names
    )
}

create_projection_table <- function(
    management_dir = management_dir, 
    doc_dir = doc_dir, 
    model_dir = model_dir,
    fixed_removals = c(91.53, 94.69),
    doc_names = c("nca", 'sca'),
    years = 2023:2034,
    table_names = c("copper_ca_north.csv", "copper_ca_south.csv"),
    prefix = NULL){
  
  if(!is.null(prefix)) { prefix <- paste0(prefix, "-")}
  
  load(file.path(doc_dir, doc_names[1], "00mod.Rdata"))
  model1 <- model
  load(file.path(doc_dir, doc_names[2], "00mod.Rdata"))
  model2 <- model
  
  sb1 <- model1$derived_quants[model1$derived_quants$Label %in% paste0("SSB_", years), "Value"]
  sb2 <- model2$derived_quants[model2$derived_quants$Label %in% paste0("SSB_", years), "Value"]
  sb0 <- model1$derived_quants[model1$derived_quants$Label == "SSB_Virgin", "Value"] +
    model2$derived_quants[model2$derived_quants$Label == "SSB_Virgin", "Value"]
  
  sb <- round(sb1 + sb2, 2)
  depl <- round(sb / sb0, 3)
  
  # read in the management tables for north and south of 4010 in CA
  table1 <- read.csv(file.path(management_dir, table_names[1]))
  table2 <- read.csv(file.path(management_dir, table_names[2]))
  
  ofl <- table1$OFL[table1$Year %in% 2023:2024] + table2$OFL[table2$Year %in% 2023:2024]
  acl <- table1$ACL[table1$Year %in% 2023:2024] + table2$ACL[table2$Year %in% 2023:2024]
  
  #est_ofl <- round(model1$derived_quants[model1$derived_quants$Label %in% paste0("OFLCatch_", years), "Value"] +
  #  model2$derived_quants[model2$derived_quants$Label %in% paste0("OFLCatch_", years), "Value"], 2)
  
  table_proj <- read.csv(file.path(model_dir, "Projection_Values.csv"))
  est_ofl <- table_proj$OFL
  est_abc <- round(model1$derived_quants[model1$derived_quants$Label %in% paste0("ForeCatch_", years), "Value"] +
    model2$derived_quants[model2$derived_quants$Label %in% paste0("ForeCatch_", years), "Value"], 2)[c(-1, -2)]
  
  buffer <- round(est_abc/est_ofl, 3)
  
  out <- data.frame(
    Year = years, 
    ofl_set = c(round(ofl, 1), rep("-", 10)),
    abc_set = c(round(acl, 1), rep("-", 10)),
    removals = c(fixed_removals, rep("-", 10)),
    ofl = c("-", "-", round(est_ofl, 1)),
    abc = c("-", "-", round(est_abc, 1)),
    buffer = c("-", "-", round(buffer, 3)),
    SB = round(sb, 2), 
    Depl = round(depl, 3))
  
  col_names <- c("Year", "Adopted OFL (mt)", "Adopted ABC (mt)", "Assumed Catch (mt)",
                 "OFL (mt)", "ABC (mt)", "Buffer", "Spawning Output", "Relative Spawning Ouptut")
  
  sa4ss::table_format(
    x = out,
    caption = "The estimated OFL, ABC, buffer, spawning output in number of million eggs across California, and relative spawning outut by year.",
    label = paste0(prefix, "ca-proj"),
    landscape = TRUE,
    col_names = col_names,
    custom_width = TRUE,
    col_to_adjust = 2:8, 
    width = c("1.5cm")
  )  
  
}

plot_combined <- function(
  doc_dir = doc_dir, 
  doc_names = c("nca", 'sca'),
  years = 1916:2023){
  
  load(file.path(doc_dir, doc_names[1], "00mod.Rdata"))
  model1 <- model
  load(file.path(doc_dir, doc_names[2], "00mod.Rdata"))
  model2 <- model
  
  sb1_all <- model1$derived_quants[model1$derived_quants$Label %in% paste0("SSB_", 1916:2023), "Value"]
  sb2_all <- model2$derived_quants[model2$derived_quants$Label %in% paste0("SSB_", 1916:2023), "Value"]
  sb0 <- model1$derived_quants[model1$derived_quants$Label == "SSB_Virgin", "Value"] +
    model2$derived_quants[model2$derived_quants$Label == "SSB_Virgin", "Value"]
  sb_all <- sb1_all + sb2_all
  
  pngfun(doc_dir = file.path(doc_dir, "shared_figures"), file = "spawning_output_combined.png", w = 7, h = 5, pt = 12)
  plot(1916:2023, sb_all, type = 'b', col = 'blue', yaxs = 'i', xaxs = 'i', ylim = c(0, max(sb_all)*1.10),
       ylab = "Spawning Output", xlab = "Year")
  lines(1916:2023, sb_all, lty = 1, col = 'blue')
  dev.off()
  
  pngfun(doc_dir = file.path(doc_dir, "shared_figures"), file = "depletion_combined.png", w = 7, h = 5, pt = 12)
  plot(1916:2023, sb_all / sb0, type = 'b', col = 'blue', yaxs = 'i', xaxs = 'i', ylim = c(0, 1.1),
       ylab = "Relative Spawning Output", xlab = "Year")
  lines(1916:2023, sb_all / sb0, lty = 1, col = 'blue')
  abline(h = 1.0, lty = 1, col = 'red')
  abline(h = 0.40, lty = 1, col = 'red')
  abline(h = 0.25, lty = 1, col = 'red')
  print.letter(label = "Management target", xy = c(0.16, 0.41), cex = 0.9)
  print.letter(label = "Minimum stock size threshold", xy = c(0.22, 0.26), cex = 0.9)
  dev.off()
}
  
