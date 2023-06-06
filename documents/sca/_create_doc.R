# Install the package if needed
# remotes::install_github("pfmc-assessments/sa4ss", ref ="sub-area_models")
# remotes::install_github("r4ss/r4ss", ref ="sub-area_models_es")
library(sa4ss)
library(here)
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/sa4ss")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/r4ss")

# Specify the directory for the document
south_model_name <- "14.3_revised_pre-star_base"
north_model_name <- "9.11_revised_pre-star_base"

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
sens_dir <- here("models", "sca", "_sensitivities")
retro_dir <- profile_dir <- here("models", "sca")
model_dir <- here("models", "sca", south_model_name)
north_model_dir <- here("models", "nca", north_model_name)
south_model_dir <- here("models", "sca", south_model_name)
management_dir <- here("management")
# Points to the network
data_dir<- here("data")
r_dir <- here("R")

save(model_dir, bridge_dir, doc_dir, data_dir, management_dir, north_model_dir, south_model_dir,
     sens_dir, south_model_name, north_model_name, retro_dir, profile_dir,
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



#===============================================================================
# Compile only particular sections
#===============================================================================
if(file.exists("_main.Rmd")){
  file.remove("_main.Rmd")
}
bookdown::render_book("00a.Rmd", 
                      output_dir = doc_dir, clean = FALSE, 
                      config_file = "_bookdown_south.yml")
# stopper


#==================================================================================================
# Read in a new model
# Create a model Rdata object and executive summary ES tex files
#==================================================================================================

read_model(
  mod_loc = south_model_dir,
  add_prefix = "south",
  add_text = "south of Point Conception",
  create_plots = FALSE, 
  save_loc = file.path(doc_dir, "sca", "tex_tables"))

read_model(
  mod_loc = north_model_dir,
  add_prefix = "north",
  add_text = "north of Point Conception",
  create_plots = FALSE, 
  save_loc = file.path(doc_dir, "sca",  "tex_tables"))

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
# Create combined figures
#================================================================================================= 

south <- r4ss::SS_output(south_model_dir)
SS_plots(south, btarg = -1, minbthresh = -1)
north <- r4ss::SS_output(north_model_dir)
SS_plots(north, btarg = -1, minbthresh = -1)
modelnames <- c("South of Point Conception", "North of Point Conception")
mysummary <- r4ss::SSsummarize(list(south, north))

r4ss::SSplotComparisons(mysummary,
                  legendlabels = modelnames, 	
                  subplot = c(2, 4, 6, 8, 10, 12, 15),
                  ylimAdj = 1.10,
                  print = TRUE,
                  pdf = FALSE,
                  btarg = -1,
                  minbthresh = -1,
                  plotdir = file.path(doc_dir, "shared_figures"))

# New yield figures
SSplotYield(
  replist = south, 
  subplot = 2, 
  ref = "Current", 
  print = TRUE,
  plotdir = file.path(doc_dir, "shared_figures"))

SSplotYield(
  replist = north, 
  subplot = 2, 
  ref = "Current", 
  print = TRUE,
  plotdir = file.path(doc_dir, "shared_figures"))

#==================================================================================================
# Create tex tables for all files listed in the all_tables.csv
#==================================================================================================

# Read and create tex files for tables listed in "table" folder in the doc
es_table_tex(
  dir = getwd(), 
  save_loc = file.path(getwd(), "tex_tables"), 
  csv_name = "all_tables.csv")


es_table_tex(
  dir = file.path(south_model_dir, "tables"), 
  add_prefix = "south",
  save_loc = file.path(getwd(), "tex_tables"), 
  csv_name = "table_labels.csv")

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
# Function to build a single section of the document - not recently tested
#==================================================================================================

# Render the pdf
bookdown::render_book("00a.Rmd", clean = FALSE)

# Use to only render a specific section which can be quicker
bookdown::preview_chapter("01executive.Rmd", preview = TRUE, clean = FALSE)