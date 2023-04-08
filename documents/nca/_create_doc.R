# Install the package if needed
# remotes::install_github("pfmc-assessments/sa4ss")
library(sa4ss)
library(here)

# Specify the directory for the document
model_name <- "5.2_crfs_pr_index"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
}
# Based on the user
doc_dir <- file.path(user_dir, "documents")

bridge_dir <- here("models", "nca", "_bridging")
model_dir <- here("models", "nca", "_bridging", model_name)
data_dir<- here("data")
r_dir <- here("R")
save(bridge_dir, model_dir, doc_dir, data_dir, file = file.path(doc_dir, "nca", "saved_directories.Rdata"))

setwd(file.path(doc_dir, "nca"))
load('saved_directories.Rdata')

#===============================================================================
# Compile command
#===============================================================================
if(file.exists("_main.Rmd")){
  file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book(
  "00a.Rmd", 
  clean = FALSE, 
  output_dir = getwd()
)

#==================================================================================================
# Initial Document Creation
# Create the needed items to generate the "right" template that would be based on the inputs here:
#==================================================================================================
sa4ss::draft(
  authors = c("Melissa H. Monk", "Chantel R. Wetzel", "Julia Coates"),
  species = "Copper Rockfish",
  latin = "Sebastes caurinus",
  coast = "California North of Pt. Conception U.S. West",
  type = c("sa"),
  create_dir = FALSE,
  edit = FALSE
)

#==================================================================================================
# Read in a new model
# Create a model Rdata object and executive summary ES tex files
#==================================================================================================
sa4ss::read_model(
  mod_loc = model_dir,
  create_plots = FALSE, 
  save_loc = file.path(model_dir, "tex_tables"))

#==================================================================================================
# Updated Read in a new model
# Create a model Rdata object and executive summary ES tex files
#==================================================================================================
sa4ss::es_table_tex(
  dir = model_dir, 
  save_loc = file.path(model_dir, "tex_tables"), 
  add_prefix = "north",
  csv_name = "table_labels.csv")

# Read and create tex files for tables listed in "table" folder in the doc
es_table_tex(
  dir = getwd(), 
  save_loc = file.path(getwd(), "tex_tables"), 
  csv_name = "all_tables.csv")

