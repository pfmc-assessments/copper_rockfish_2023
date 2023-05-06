# Create comparison plots to previous assessments

library(r4ss)

user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
fig_dir <- "C:/Assessments/2023/copper_rockfish_2023/documents/shared_figures"

south_base <- "14.0_base"
north_base <- "9.8_selex_fix"

#===============================================================================
# Start with south of Point Conception
#===============================================================================

dir_2013 = "E:/Assessments/2021/copper_rockfish_2021/models/ca_s_pt_c/_bridge"
model_2013 = SS_output(file.path(dir_2013, "1.0_single_sex_growth"))

dir_2021 = "E:/Assessments/2021/copper_rockfish_2021/models/ca_s_pt_c/12.1_base"
model_2021 <- SS_output(dir_2021)

model_2023 <- SS_output(file.path(user_dir, "models", "sca", south_base))

modelnames <- c("Base Model", "2021", "2013")
mysummary <- SSsummarize(list(model_2023, model_2021, model_2013))

SSplotComparisons(mysummary,
                  filenameprefix = "south_assess_compare_",
                  legendlabels = modelnames, 	
                  subplot = c(2, 4),
                  ylimAdj = 1.05,
                  print = TRUE,
                  pdf = FALSE,
                  btarg = -1,
                  minbthresh = -1,
                  plotdir = fig_dir)


#===============================================================================
# North of Point Conception
#===============================================================================

dir_2021 = "E:/Assessments/2021/copper_rockfish_2021/models/ca_n_pt_c/10.3_base"
model_2021 <- SS_output(dir_2021)

model_2023 <- SS_output(file.path(user_dir, "models", "nca", north_base))

modelnames <- c("Base Model", "2021")
mysummary <- SSsummarize(list(model_2023, model_2021))

SSplotComparisons(mysummary,
                  filenameprefix = "north_assess_compare_",
                  legendlabels = modelnames, 	
                  subplot = c(2, 4),
                  ylimAdj = 1.0,
                  print = TRUE,
                  pdf = FALSE,
                  btarg = -1,
                  minbthresh = -1,
                  plotdir = fig_dir)

