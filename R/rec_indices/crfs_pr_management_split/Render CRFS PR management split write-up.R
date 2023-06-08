rm(list = ls(all = TRUE))
graphics.off()

setwd("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/CRFS_PR_management_split")
output.directory = "C:/Stock_Assessments/VRML_Assessment_2021/GitHub/Vermilion_2021/doc/indices/"

rmarkdown::render("C:/Stock_Assessments/VRML_Assessment_2021/Indices_of_Abundance/CRFS_PR_management_split/PR_management_split_write_up.Rmd",
clean = FALSE,
output_dir = output.directory,
output_file = "Vermilion_PR_management_split_writeup_NCA.pdf",
intermediates_dir = output.directory
#knit_root_dir = output.directory
)

