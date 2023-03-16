#-------------------------------------------------------------------------------
# Set model specifications for the write-ups
# set.vars definitions used as parameters in the write up
# 1 Model.number   1 for north and 2 for south
# 2 species.name  species common name used
# 3 assess.folder  assessment folder 4 characters PACFIN code
# 4 index.subfolder index name with underscores
# 5 survey.name     index or survey name with spaces

rm(list = ls(all = TRUE))
graphics.off()
Assessment_year = 2021
# MRFSS dockside
# set.vars = c(1, "vermilion", "VRML", "MRFSS_dockside", "MRFSS dockside")
# set.vars = c(2, "vermilion", "VRML", "MRFSS_dockside", "MRFSS dockside")
# set.vars = c(1, "lingcod"  , "LCOD", "MRFSS_dockside", "MRFSS dockside")

# start here and add in qq plots
# CA CPFV onboard
# set.vars = c(1, "vermilion", "VRML", "CA_CPFV_onboard", "CA CPFV onboard")
# set.vars = c(2, "vermilion", "VRML", "CA_CPFV_onboard", "CA CPFV onboard")
# set.vars = c(2, "lingcod"  , "LCOD", "CA_CPFV_onboard", "CA CPFV onboard")

# Deb WV
# set.vars = c(1, "vermilion", "VRML", "DebWV_onboard", "DebWV onboard")
# set.vars = c(1, "lingcod"  , "LCOD", "DebWV_onboard", "DebWV onboard")

# CRFS PR dockside
# set.vars = c(1, "vermilion", "VRML", "CRFS_PR_dockside", "CRFS PR dockside")
# set.vars = c(2, "vermilion", "VRML", "CRFS_PR_dockside", "CRFS PR dockside")
# set.vars = c(1, "lingcod"  , "LCOD", "CRFS_PR_dockside", "CRFS PR dockside")
# set.vars = c(2, "lingcod"  , "LCOD", "CRFS_PR_dockside", "CRFS PR dockside")

# CCFRP
set.vars = c(1, "vermilion", "VRML", "CCFRP", "CCFRP")
# set.vars = c(1, "lingcod",   "LCOD", "CCFRP", "CCFRP")

# WCGBTS - need to come back to these
# set.vars <- c(1, "vermilion", "VRML", "WCGBTS", "WCGBTS")
# set.vars = c(2, "vermilion", "VRML", "WCGBTS", "WCGBTS")

# NWFSC_HL
#set.vars <- c(2, "vermilion", "VRML", "NWFSC_HL", "NWFSC HL")
#set.vars <- c(2, "lingcod", "LCOD", "NWFSC_HL", "NWFSC HL")  


set.vars <- c("Vermilion_NCA", "MRFSS_dockside") 

#output directory
if(Include_CP==T){
  setwd(paste0(getwd(),"/",Target_spp,"/TripLevel/SM/Include_CalPoly/"))
  output.directory <- getwd()
} else {
  
  setwd(paste0(getwd(),"/",Target_spp,"/TripLevel/SM/Exclude_CalPoly/"))
  output.directory <- getwd()
}

#-------------------------------------------------------------------------------
# Set working directories and output directories
Model_region <- c("NCA", "SCA", "SCA") # second "SCA" for one instance where lingcod has different definition
set.index.folder <- paste0("C:/Stock_Assessments/Assessments_", Assessment_year, "/", set.vars[1],"/Indices/", set.vars[2])
#set.index.path <- paste0(set.index.folder, set.vars[4], "/", Model_region[as.numeric(set.vars[1])])
source(paste0(set.index.folder, "dir_recent.R"))
set.model.dir <- dir_recent(dir = set.index.path, pattern = "2021")

#-------------------------------------------------------------------------------
# General render
library(dplyr)
library(kableExtra)


rmarkdown::render(paste0(
  "C:/Stock_Assessments/Indices_of_Abundance/", set.vars[2],
  "_writeup.Rmd"
),
clean = FALSE,
params = list(
  Model.number = as.numeric(set.vars[1]),
  species.name = set.vars[2],
  assess.folder = set.vars[3],
  index.subfolder = set.vars[4],
  survey.name = set.vars[5]
),
output_dir = output.directory,
output_file = paste0(
  set.vars[2], "_", set.vars[4],
  "_writeup_",
  Model_region[as.numeric(set.vars[1])],
  ".pdf"
),
intermediates_dir = output.directory,
knit_root_dir = output.directory
)

# 
# 
# 
# 
# # output indices for ss
# # list of fleets with indices for north and south and their names
# index_fleets <- c(4, 6, 8, 8, 9, 10, 13)
# assess_folder <- c("MRFSS_dockside", "CRFS_PR_dockside", "DebWV_onboard", 
#                    "NWFSC_HL", "WCGBTS", "CA_CPFV_onboard", "CCFRP")
# region <- as.numeric(c(3, 3, 1, 2, 3, 3, 1))
# 
# index_info <- data.frame(index_fleets, assess_folder, region)
# 
# Index.list.north <- vector(mode = "list", sum(index_info$region %in% c(1, 3)))
# Index.list.south <- vector(mode = "list", sum(index_info$region %in% c(2, 3)))
# 
# for (i in 1:length(index_info$region != 2)) {
#   pathNCA <- paste0("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/", index_info$assess_folder[i], "/NCA")
#   source("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/dir_recent.R")
#   index_file <- dir_recent(dir = pathNCA, pattern = "2021")
#   filein <- dir(index_file,
#     pattern = paste0(".*\\Index.csv$"), full.names = TRUE
#   )
#   index <- read.csv(filein)
#   index <- index %>%
#     select(Year, Index, logSD) %>%
#     mutate(
#       flt = index_info$index_fleets[i],
#       comment = paste0("#", index_info$assess_folder[i]),
#       mo = 7
#     ) %>%
#     rename(
#       `#_yr` = Year,
#       obs = Index,
#       logSE = logSD
#     ) %>%
#     select(`#_yr`, mo, flt, obs, logSE, comment)
# 
#   Index.list.north[[i]] <- index
# }
# 
# j <- 1
# for (i in 1:length(index_info$region)) {
#   if (index_info$region[i] == 3) {
#     pathSCA <- paste0("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/", index_info$assess_folder[i], "/SCA")
#     source("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/dir_recent.R")
#     index_file <- dir_recent(dir = pathSCA, pattern = "2021")
#     filein <- dir(index_file,
#       pattern = paste0(".*\\Index.csv$"), full.names = TRUE
#     )
#     index <- read.csv(filein)
#     index <- index %>%
#       select(Year, Index, logSD) %>%
#       mutate(
#         flt = index_info$index_fleets[i],
#         comment = paste0("#", index_info$assess_folder[i]),
#         mo = 7
#       ) %>%
#       rename(
#         `#_yr` = Year,
#         obs = Index,
#         logSE = logSD
#       ) %>%
#       select(`#_yr`, mo, flt, obs, logSE, comment)
#     Index.list.south[[j]] <- index
#     j <- j + 1
#   }
# }
# 
# sink("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/NoCA_VRML_Indices_for_SS.csv")
# Index.list.north
# sink()
# 
# sink("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/SoCA_VRML_Indices_for_SS.csv")
# Index.list.south
# sink()
# unlink()
