#######################################################################################################
# Copper Rockfish: South of Pt. Conception
# Retrospective Squid plot
#######################################################################################################

library(r4ss)

# Specify the directory
user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}

model_dir <- file.path(user_dir, "models", "sca")
base_name <- "2.0_mi_dw"
retro.folder <- file.path(user_dir, "models", "sca", paste0(base_name, "_retro"))


# Read in the retrospective runs
base <- SS_output(file.path(model_dir, base_name))
retro1 = SS_output(file.path(retro.folder, "retro", "retro-1"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro2 = SS_output(file.path(retro.folder, "retro", "retro-2"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro3 = SS_output(file.path(retro.folder, "retro", "retro-3"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro4 = SS_output(file.path(retro.folder, "retro", "retro-4"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro5 = SS_output(file.path(retro.folder, "retro", "retro-5"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro6 = SS_output(file.path(retro.folder, "retro", "retro-6"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro7 = SS_output(file.path(retro.folder, "retro", "retro-7"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro8 = SS_output(file.path(retro.folder, "retro", "retro-8"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro9 = SS_output(file.path(retro.folder, "retro", "retro-9"), printstats = FALSE, verbose = FALSE, covar = FALSE)
retro10 = SS_output(file.path(retro.folder, "retro", "retro-10"), printstats = FALSE, verbose = FALSE, covar = FALSE)


modelnames <- c("Base Model", paste0("Retro -", 1:10))

# Summarize the results across models
mysummary <- SSsummarize(list(base, retro1, retro2, retro3, retro4, retro5,
                              retro6, retro7, retro8, retro9, retro10))

HandyCode::pngfun(wd = retro.folder, file = "Squid_RecDevs.png", h = 7, w = 7)
SSplotRetroRecruits(retroSummary = mysummary,
                    endyrvec = rev(2013:2023),
                    cohorts = 2005:2021,
                    ylim=NULL,uncertainty=FALSE,
                    labels=c('Recruitment deviation', 'Recruitment (billions)', 'relative to recent estimate', 'Age'),
                    main="",
                    mcmcVec=FALSE,devs=TRUE,
                    relative=FALSE,labelyears=TRUE,legend=FALSE,leg.ncols=4)
dev.off()
