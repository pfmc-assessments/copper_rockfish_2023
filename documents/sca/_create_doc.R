# Install the package if needed
# remotes::install_github("pfmc-assessments/sa4ss")
library(sa4ss)
library(here)

# Specify the directory for the document
model_name <- "0_ss_exe"

model_dir <- here("models", "sca", "_bridging", model_name)
doc_dir <- here("documents")
data_dir<- here("data")
r_dir <- here("R")
save(model_dir, doc_dir, data_dir, file = file.path(doc_dir, "sca", "saved_directories.Rdata"))

setwd(file.path(doc_dir, "sca"))


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
  species = "copper rockfish",
  latin = "Sebastes caurinus",
  coast = "California South of Pt. Conception U.S. West",
  type = c("sa"),
  create_dir = FALSE,
  edit = FALSE
)

#Create a model Rdata object
sa4ss::read_model(
  mod_loc = model_dir,
  create_plots = FALSE, 
  save_loc = file.path(model_dir, "tex_tables"))

model <- r4ss::SS_output(model_dir)
r4ss::SSexecutivesummary(
  replist = model, 
  format = FALSE)

sa4ss::es_table_tex(
  dir = model_dir, 
  save_loc = file.path(model_dir, "tex_tables"), 
  csv_name = "table_labels.csv")

# Read and create tex files for tables listed in "table" folder in the doc
#es_table_tex(
#  dir = file.path(getwd(), 'tables'), 
#  save_loc = file.path(getwd(), "tex_tables"), 
#  csv_name = "all_tables.csv")




# Render the pdf
bookdown::render_book("00a.Rmd", clean = FALSE)


# Use to only render a specific section which can be quicker
bookdown::preview_chapter("01executive.Rmd", preview = TRUE, clean = FALSE)