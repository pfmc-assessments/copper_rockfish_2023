# Create comparison plots to previous assessments

library(r4ss)

user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
fig_dir <- "C:/Assessments/2023/copper_rockfish_2023/documents/shared_figures"


south_base <- "15.0_south_post_star_base"
north_base <- "10.0_north_post_star_base"

#===============================================================================
# Start with south of Point Conception
#===============================================================================

dir_2013 = "E:/Assessments/2021/copper_rockfish_2021/models/ca_s_pt_c/_bridge"
model_2013 = SS_output(file.path(dir_2013, "1.1_single_sex_growth_fecund"))

dir_2021 = "E:/Assessments/2021/copper_rockfish_2021/models/ca_s_pt_c/12.1_base"
model_2021 <- SS_output(dir_2021)

model_2023 <- SS_output(file.path(user_dir, "models", "sca", south_base))
sens_2021 <- SS_output("E:/Assessments/2021/copper_rockfish_2021/models/ca_s_pt_c/_sensitivities/12.1_base_recdevs")

modelnames <- c("Base Model", "2021")#, "2021 w/ Rec. Devs.")#, "2013")
mysummary <- SSsummarize(list(model_2023, model_2021))#, sens_2021))#, model_2013))

SSplotComparisons(mysummary,
                  filenameprefix = "south_assess_compare_21_23_",
                  legendlabels = modelnames, 	
                  subplot = c(2, 4),
                  ylimAdj = 1.05,
                  print = TRUE,
                  pdf = FALSE,
                  btarg = -1,
                  minbthresh = -1,
                  plotdir = fig_dir)

mod <- read.csv(here::here("models", "south_assessment_comparison.csv"))

HandyCode::pngfun(wd = fig_dir, file = "south_total_biomass_comparison.png", w = 7, h = 5, pt = 12)

plot(1914:2023, mod$Bio_all_2023, type = 'b', col = 'blue', yaxs = 'i', xaxs = 'i', 
     ylim = c(0, max(mod[,2:4])*1.10),
     ylab = "Total Biomass (mt)", xlab = "Year")
lines(1914:2023, mod$Bio_all_2023, lty = 1, col = 'blue')
lines(1914:2021, mod[mod$Yr %in% 1914:2021, "Bio_all_2021"], lty = 1, col = 'red')
points(1914:2021, mod[mod$Yr %in% 1914:2021, "Bio_all_2021"], pch = 17, col = 'red')
lines(1914:2013, mod[mod$Yr %in% 1914:2013, "Bio_all_2013"], lty = 1, col = 'green')
points(1914:2013,mod[mod$Yr %in% 1914:2013, "Bio_all_2013"], pch = 18, col = 'green')
legend("topright", bty = 'n', col = c("blue", "red", "green"), lty = 1, pch = 16:18, 
       legend = c("Base Model", "2021", "2013"), cex = 1.2)
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


#===============================================================================================
# Both areas
#===============================================================================================
library(r4ss)
user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
fig_dir <- "C:/Assessments/2023/copper_rockfish_2023/documents/shared_figures"

south_base <- "15.0_south_post_star_base"
north_base <- "10.0_north_post_star_base"

dir_2021 = "E:/Assessments/2021/copper_rockfish_2021/models/ca_s_pt_c/12.1_base"
southmodel_2021 <- SS_output(dir_2021)
southmodel_2023 <- SS_output(file.path(user_dir, "models", "sca", south_base))

dir_2021 = "E:/Assessments/2021/copper_rockfish_2021/models/ca_n_pt_c/10.3_base"
northmodel_2021 <- SS_output(dir_2021)
northmodel_2023 <- SS_output(file.path(user_dir, "models", "nca", north_base))


modelnames <- c("South - 2023", "South - 2021", "North - 2023", "North - 2021")
mysummary <- SSsummarize(list(southmodel_2023, southmodel_2021, northmodel_2023, northmodel_2021))

SSplotComparisons(mysummary,
                  filenameprefix = "both_areas_compare_21_23_",
                  legendlabels = modelnames, 	
                  subplot = c(2, 4),
                  ylimAdj = 1.05,
                  print = TRUE,
                  pdf = FALSE,
                  btarg = -1,
                  minbthresh = -1,
                  plotdir = fig_dir)

