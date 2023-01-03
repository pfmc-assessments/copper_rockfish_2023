# Install the package if needed
# remotes::install_github("pfmc-assessments/sa4ss")
library(sa4ss)

# Specify the directory for the document
dir <- "C:/Assessments/2023/copper_rockfish_2023"

doc_dir <- file.path(dir, "documents", "sca")
setwd(doc_dir)

model_name <- "model name here"
model_dir <- file.path(dir, "models", "sca", model)


# Compile command
if(file.exists("_main.Rmd")){
  file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book(
  "00a.Rmd", 
  clean = FALSE, 
  output_dir = getwd()
)

# Create the needed items to generate the "right" template that would be based on the inputs here:
sa4ss::draft(
  authors = c("Chantel R. Wetzel", "Melissa H. Monk", "Julia Coates"),
  species = "Copper Rockfish",
  latin = "Sebastes caurinus",
  coast = "California South of Pt. Conception U.S. West",
  type = c("sa"),
  create_dir = FALSE,
  edit = FALSE
)

#Create a model Rdata object
sa4ss::read_model(
  mod_loc = mod_dir,
  create_plots = FALSE, 
  save_loc = file.path(mod_dir, "tex_tables"),
  verbose = TRUE)

SSexecutivesummary(
  replist = mod_dir, 
  format = FALSE)

es_table_tex(
  dir = mod_dir, 
  save_loc = file.path(mod_dir, "tex_tables"), 
  csv_name = "table_labels.csv")

# Read and create tex files for tables listed in "table" folder in the doc
es_table_tex(
  dir = file.path(getwd(), 'tables'), 
  save_loc = file.path(getwd(), "tex_tables"), 
  csv_name = "all_tables.csv")




# Render the pdf
bookdown::render_book("00a.Rmd", clean = FALSE)


# Use to only render a specific section which can be quicker
bookdown::preview_chapter("01executive.Rmd", preview = TRUE, clean = FALSE)