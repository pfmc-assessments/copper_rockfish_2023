# Install the package if needed
# remotes::install_github("pfmc-assessments/sa4ss")
library(sa4ss)
library(here)

# Specify the directory for the document
model_name <- "5.5_est_m"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}
# Based on the user
doc_dir <- file.path(user_dir, "documents")
# Currently points to the network location but could be revised
bridge_dir <- here("models", "sca", "_bridging")
model_dir <- here("models", "sca", model_name)
management_dir <- here("management")
# Points to the network
data_dir<- here("data")
r_dir <- here("R")

save(model_dir, bridge_dir, doc_dir, data_dir, management_dir,
     file = file.path(doc_dir, "sca", "saved_directories.Rdata"))

setwd(file.path(doc_dir, "sca"))

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
  authors = c("Chantel R. Wetzel", "Melissa H. Monk", "Julia Coates"),
  species = "copper rockfish",
  latin = "Sebastes caurinus",
  coast = "California South of Pt. Conception U.S. West",
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

# Create a management projection table from both of the the sub-area models



# The below functions are wrapped up in the read_model function so may not need to use these
# model <- r4ss::SS_output(model_dir)
# r4ss::SSexecutivesummary(
#   replist = model, 
#   format = FALSE)
# 
# sa4ss::es_table_tex(
#   dir = model_dir, 
#   save_loc = file.path(model_dir, "tex_tables"), 
#   csv_name = "table_labels.csv")

#==================================================================================================
# Create tex tables for all files listed in the all_tables.csv
#==================================================================================================

# Read and create tex files for tables listed in "table" folder in the doc
es_table_tex(
  dir = getwd(), 
  save_loc = file.path(getwd(), "tex_tables"), 
  csv_name = "all_tables.csv")

#==================================================================================================
# Function to build a single section of the document - not recently tested
#==================================================================================================

# Render the pdf
bookdown::render_book("00a.Rmd", clean = FALSE)

# Use to only render a specific section which can be quicker
bookdown::preview_chapter("01executive.Rmd", preview = TRUE, clean = FALSE)