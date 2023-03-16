# output indices for ss

rm(list = ls(all = TRUE))
graphics.off()


setwd("C:/Stock_Assessments/_VRML_Assessment_2021/Indices_of_Abundance")
# list of fleets with indices for north and south and their names
index_fleets <- c(4, 6, 8, 8, 9, 10, 13)
assess_folder <- c("MRFSS_dockside", "CRFS_PR_dockside", "DebWV_onboard", 
                   "NWFSC_HL", "WCGBTS", "CA_CPFV_onboard", "CCFRP")
region <- as.numeric(c(3, 3, 1, 2, 3, 3, 1))

index_info <- data.frame(index_fleets, assess_folder, region)

Index.list.north <- vector(mode = "list", sum(index_info$region %in% c(1, 3)))
Index.list.south <- vector(mode = "list", sum(index_info$region %in% c(2, 3)))

j <- 1
for (i in 1:length(index_info$region)) {
  if (index_info$region[i] != 2) {
  pathNCA <- paste0("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/", index_info$assess_folder[i], "/NCA")
  source("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/dir_recent.R")
  index_file <- dir_recent(dir = pathNCA, pattern = "2021")
  filein <- dir(index_file,
                pattern = paste0(".*\\Index.csv$"), full.names = TRUE
  )
  index <- read.csv(filein)
  index <- index %>%
    dplyr::select(Year, Index, logSD) %>%
    mutate(
      flt = index_info$index_fleets[i],
      comment = paste0("#", index_info$assess_folder[i]),
      mo = 7
    ) %>%
    rename(
      `#_yr` = Year,
      obs = Index,
      logSE = logSD
    ) %>%
    dplyr::select(`#_yr`, mo, flt, obs, logSE, comment)
  
  Index.list.north[[j]] <- index
  names(Index.list.north)[[j]] <- paste0("#", index_info$assess_folder[i])
   j <- j +1
}
}


j <- 1
for (i in 1:length(index_info$region)) {
  if (index_info$region[i] > 1) {
    pathSCA <- paste0("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/", index_info$assess_folder[i], "/SCA")
    source("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/dir_recent.R")
    index_file <- dir_recent(dir = pathSCA, pattern = "2021")
    filein <- dir(index_file,
                  pattern = paste0(".*\\Index.csv$"), full.names = TRUE
    )
    index <- read.csv(filein)
    index <- index %>%
      dplyr::select(Year, Index, logSD) %>%
      mutate(
        flt = index_info$index_fleets[i],
        comment = paste0("#", index_info$assess_folder[i]),
        mo = 7
      ) %>%
      rename(
        `#_yr` = Year,
        obs = Index,
        logSE = logSD
      ) %>%
      dplyr::select(`#_yr`, mo, flt, obs, logSE, comment)
    Index.list.south[[j]] <- index
    names(Index.list.south)[[j]] <- paste0("#", index_info$assess_folder[i])
    j <- j + 1
  }
}

sink(paste0("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/NoCA_VRML_Indices_for_SS_",Sys.Date(),".csv"))
Index.list.north
sink()

sink(paste0("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/SoCA_VRML_Indices_for_SS_",Sys.Date(),".csv"))
Index.list.south
sink()

