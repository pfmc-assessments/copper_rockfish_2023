

pak::pkg_install("pfmc-assessments/nwfscAgeingError@TMB")
library(AgeingError)

library(here)
dir <- here("data", "ages", "ageing_error_tmb")

unformat_data <- read.csv(file = file.path(dir, "double_reads_a1=patrick_a2=tyler.csv"))

  EchoFile <- file.path(dir, "EchoTMB.out")
  write("", EchoFile)
  AprobWght <- 0.000001
  SlopeWght <- 0.01
  Species <- "Copper-Rockfish"
  Outs <- AgeingError:::CreateData(
    DataFile = file.path(dir, "double_reads_a1=patrick_a2=tyler.csv"),
    NDataSet = 1,
    verbose = FALSE
  )
  
  ModelSpecs <- AgeingError:::CreateSpecs(
    SpecsFile = file.path(dir, "ebspoll.spc"),
    DataSpecs = Outs,
    verbose = FALSE
  )
  model <- AgeingError:::DoApplyAgeError(
    Species = Species,
    DataSpecs = Outs,
    ModelSpecs = ModelSpecs,
    SaveDir = file.path(dir),
    verbose = FALSE
  )
  Output <- AgeingError:::ProcessResults(
    Species = Species,
    SaveDir = file.path(dir),
    CalcEff = FALSE,
    verbose = FALSE
  )
  
  
#  Error in grDevices::png(file.path(SaveFile, filename), width = 6.5, height = 6.5,  : 
#                            unable to start png() device
#                          In addition: Warning messages:
#                            1: In grDevices::png(file.path(SaveFile, filename), width = 6.5, height = 6.5,  :
#                                                   unable to open file 'N:/Assessments/CurrentAssessments/copper_rockfish_2023/data/ages/ageing_error_tmb/N:/Assessments/CurrentAssessments/copper_rockfish_2023/data/ages/ageing_error_tmb/Pollock-Data set-1 Reader 1 vs Reader 2.png' for writing
#                                                 2: In grDevices::png(file.path(SaveFile, filename), width = 6.5, height = 6.5,  :
#                                                                        opening device failed
