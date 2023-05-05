###############################################
# Northern Modeling Explorations
################################################

library(r4ss)
area <- "nca"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <-  "C:/Assessments/2023/copper_rockfish_2023"
}

wd <- file.path(user_dir, "models", area)
setwd(wd)


############################################################################
##Model 9.6 - 

selex <- SS_output(file.path(wd, "9.6_selex"))
#SS_plots(mrfss_explore)
#tune_comps(replist = mrfss_explore, 
#           dir = file.path(wd, "9.5_strip_80s_cpfv"), 
#           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

selex_rm_mrfssQ <- SS_output(file.path(wd, "9.6_selex_rm_mrfssQ"))
selex_rm_mrfssQ_ROV_selex <- SS_output(file.path(wd, "9.6_selex_rm_mrfssQ_ROV_selex"))
#SS_plots(selex_rm_mrfssQ_ROV_selex)
selex_rm_mrfssQ_ROV_selex_freeComliveselex <- SS_output(file.path(wd, 
                                "9.6_selex_rm_mrfssQ_ROV_selex_freeComliveselex"))
SS_plots(selex_rm_mrfssQ_ROV_selex_freeComliveselex)

modelnames <- c("Selex base", "-MRFSSQ", "ROV selex south", "Free Com Live Selex")
mysummary <- SSsummarize(list(selex, selex_rm_mrfssQ, selex_rm_mrfssQ_ROV_selex,
selex_rm_mrfssQ_ROV_selex_freeComliveselex))
SSplotComparisons(mysummary,
                  filenameprefix = "9.6_selex_",
                  legendlabels = modelnames, 
                  ylimAdj = 1.3,
                  plotdir = file.path(wd, "_plots"),
                  pdf = TRUE)


###### 9.6 reweight 9.4
reweight <- SS_output(file.path(wd, "9.6_reweight"))
SS_plots(reweight)
tune_comps(replist = reweight, 
           dir = file.path(wd, "9.6_reweight"), 
           option = "Francis", write = TRUE, allow_up_tuning = TRUE)

