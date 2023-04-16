
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
    digits = 2,
    caption = "The aggregated sub-Overfishing Limit (OFL), sub-Annual Catch Limit (ACL), and catch of copper rockfish in California.",
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
  
  sb1 <- model1$derived_quants[model1$derived_quants$Label %in% paste0("SSB_", years), "Value"]
  sb2 <- model2$derived_quants[model2$derived_quants$Label %in% paste0("SSB_", years), "Value"]
  sb0 <- model1$derived_quants[model1$derived_quants$Label == "SSB_Virgin", "Value"] +
    model2$derived_quants[model2$derived_quants$Label == "SSB_Virgin", "Value"]
  
  sb <- sb1 + sb2
  depl <- sb / sb0
  
  out <- data.frame(
    Year = years, 
    SB = round(sb, 2), 
    Depl = round(depl, 3))
  col_names <- c("Year", "Spawning Biomass", "Fraction Unfished")
  
  sa4ss::table_format(
    x = out,
    caption = "The estimated spawning biomass in number of million eggs across California and fraction unfished by year.",
    label = paste0(prefix, "ca-status"),
    col_names = col_names
    )
}

create_projection_table <- function(
    management_dir = management_dir, 
    doc_dir = doc_dir, 
    fixed_removals = c(70, 70),
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
  
  est_ofl <- round(model1$derived_quants[model1$derived_quants$Label %in% paste0("OFLCatch_", years), "Value"] +
    model2$derived_quants[model2$derived_quants$Label %in% paste0("OFLCatch_", years), "Value"], 2)
  
  est_abc <- round(model1$derived_quants[model1$derived_quants$Label %in% paste0("ForeCatch_", years), "Value"] +
    model2$derived_quants[model2$derived_quants$Label %in% paste0("ForeCatch_", years), "Value"], 2)
  
  out <- data.frame(
    Year = years, 
    ofl_set = c(round(ofl, 2), rep("-", 10)),
    abc_set = c(round(acl, 2), rep("-", 10)),
    removals = c(fixed_removals, rep("-", 10)),
    ofl = c("-", "-", est_ofl[c(-1, -2)]),
    obc = c("-", "-", est_abc[c(-1, -2)]),
    SB = round(sb, 2), 
    Depl = round(depl, 3))
  
  col_names <- c("Year", "Adopted OFL (mt)", "Adopted ABC (mt)", "Assumed Catch (mt)",
                 "OFL (mt)", "ABC (mt)", "Spawning Biomass", "Fraction Unfished")
  
  sa4ss::table_format(
    x = out,
    caption = "The estimated spawning biomass in number of million eggs across California and fraction unfished by year.",
    label = paste0(prefix, "ca-proj"),
    landscape = TRUE,
    col_names = col_names
  )  
  
}
  
