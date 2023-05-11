# Install the package if needed
#remotes::install_github("pfmc-assessments/sa4ss")
# devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/r4ss")
library(sa4ss)
library(here)
#devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/sa4ss")
#devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/r4ss")

# Specify the directory for the document
north_model_name <- "9.8_selex_fix_forecast"
south_model_name <- "14.0_base_forecast"

user <- Sys.getenv("USERNAME")
if( grepl("Chantel", user) ){
  user_dir <- "C:/Assessments/2023/copper_rockfish_2023"
} else {
  # Fill in Melissa's document directory below
  user_dir <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023"
}
# Based on the user
doc_dir <- file.path(user_dir, "documents")

assess_dir <- "S:/copper_rockfish_2023"
#bridging model
#bridge_dir <- here("models", "nca", "_bridging")
bridge_dir <- file.path(assess_dir, "models", "nca", "_bridging")

#point to the northern model for document
#model_dir <- here("models", "nca", north_model_name)
model_dir <- file.path(assess_dir, "models", "nca", north_model_name)

#point to both models for documentsection
#north_model_dir <- here("models", "nca", north_model_name)
#south_model_dir <- here("models", "sca", south_model_name)
north_model_dir <- file.path(assess_dir, "models", "nca", north_model_name)
south_model_dir <- file.path(assess_dir, "models", "sca", south_model_name)
mod_loc <- north_model_dir
#north model sensitivities
#sens_dir <- here("models", "nca", "_sensitivities")
sens_dir <-file.path(assess_dir, "models", "nca", "_sensitivities")

#management
#management_dir <- here("management")
management_dir <- file.path(assess_dir, "management")


#data
#data_dir<- here("data")
data_dir <- file.path(assess_dir, "data")
#r_dir <- here("R")

#save to Rdata file
save(model_dir, bridge_dir, doc_dir, data_dir, management_dir, north_model_dir, south_model_dir,
     sens_dir, south_model_name, north_model_name, mod_loc,
     file = file.path(doc_dir, "nca", "saved_directories.Rdata"))

setwd(file.path(doc_dir, "nca"))

#===============================================================================
# Compile command ----
#===============================================================================
if(file.exists("_main.Rmd")){
  file.remove("_main.Rmd")
}
# Render the pdf
bookdown::render_book(
  "00a.Rmd", 
  clean = FALSE, 
  output_dir = doc_dir)

#==================================================================================================
# Function to build a single section of the document - not recently tested
#==================================================================================================

# Render the pdf
#bookdown::render_book("00a.Rmd", clean = FALSE)

# Use to only render a specific section which can be quicker
#shared_txt <- "C:/Users/melissa.monk/Documents/GitHub/copper_rockfish_2023/documents/shared_text"
#bookdown::render_book(c("00a.Rmd",
#                        file.path(shared_txt,"1_intro_life_history_fishery_info.Rmd")),
#                        preview = TRUE, clean = FALSE)
if(file.exists("_main.Rmd")){
  file.remove("_main.Rmd")
}
bookdown::render_book("00a.Rmd", 
                      output_format ="bookdown::pdf_document2", #for a faster compile time
                      output_dir = doc_dir, 
                      clean = FALSE, 
                      config_file = "_bookdown_north.yml")

#stopper
#===============================================================================
# Read in a new model ----
# Create a model Rdata object and executive summary ES tex files
#===============================================================================
#read_model(
#  mod_loc = south_model_dir,
#  add_prefix = "south",
#  add_text = "south of Point Conception",
#  create_plots = FALSE, 
#  save_loc = file.path(doc_dir, "sca", "tex_tables"))

read_model(
  mod_loc = north_model_dir,
  add_prefix = "north",
  add_text = "north of Point Conception",
  create_plots = FALSE, 
  save_loc = file.path(doc_dir, "nca",  "tex_tables"))

#===============================================================================
# Create combined figures
#===============================================================================

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


#================================================================================================
# Updated Read in a new model ----
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

#==================================================================================================
# Initial Document Creation ----
# Create the needed items to generate the "right" template that would be based on the inputs here:
#==================================================================================================
# sa4ss::draft(
#   authors = c("Melissa H. Monk", "Chantel R. Wetzel", "Julia Coates"),
#   species = "Copper Rockfish",
#   latin = "Sebastes caurinus",
#   coast = "California North of Pt. Conception U.S. West",
#   type = c("sa"),
#   create_dir = FALSE,
#   edit = FALSE
# )