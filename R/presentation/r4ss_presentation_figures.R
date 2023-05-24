# Create presentation figures based on r4ss output


library(r4ss)
library(here)


dir <- here("models")
south_base <- "14.0_base_forecast"
north_base <- "9.8_selex_fix_forecast"
south_dir <-  file.path(dir, "sca", south_base)
north_dir <- file.path(dir, "nca", north_base)
  
south <- SS_output(file.path(dir, "sca", south_base))
north <- SS_output(file.path(dir, "nca", north_base))
dir.create(file.path(south_dir, "presentation_plots"))
dir.create(file.path(north_dir, "presentation_plots"))

fleet_names <- c("Commercial-Dead", "Commercial-Live", "Recreational-CPFV", 
                 "Recreational-PR")


SSplotComps(replist = south, subplots = 21, print = TRUE,
            kind = "LEN", 
            fleets = 1:4, 
            fleetnames = fleet_names,
            pwidth = 7, pheight = 7, 
            plotdir = file.path(south_dir, "presentation_plots"),
            cex.main = 2, 
            datonly = TRUE, showeffN = FALSE)

SSplotComps(replist = north, subplots = 21, print = TRUE,
            kind = "LEN", 
            fleets = 1:4, 
            fleetnames = fleet_names,
            pwidth = 7, pheight = 7, 
            plotdir = file.path(north_dir, "presentation_plots"),
            cex.main = 2, 
            datonly = TRUE, showeffN = FALSE)


SS_plots(south,
         png = TRUE,
         html = FALSE,
         datplot = TRUE,
         uncertainty = TRUE,
         maxrows = 4, 
         maxcols = 4, 
         maxrows2 = 4, 
         maxcols2 = 4, 
         printfolder = 'bubble_size_data_plots',
         bub.scale.dat= 6)

SS_plots(north,
         png = TRUE,
         html = FALSE,
         datplot = TRUE,
         uncertainty = TRUE,
         maxrows = 4, 
         maxcols = 4, 
         maxrows2 = 4, 
         maxcols2 = 4, 
         printfolder = 'bubble_size_data_plots',
         bub.scale.dat= 6)

SSplotComps(replist = south, subplots = 24, print = TRUE,
            kind = "LEN", 
            fleets = 3:4, 
            fleetnames = fleet_names,
            pwidth = 7, pheight = 7, 
            plotdir = file.path(south_dir, "presentation_plots"),
            cex.main = 4, 
            bub.scale.dat= 10,
            #scalebubbles = TRUE,
            datonly = TRUE, showeffN = FALSE)

SSplotComps(replist = north, subplots = 24, print = TRUE,
            kind = "LEN", 
            fleets = 3:4, 
            fleetnames = fleet_names,
            pwidth = 7, pheight = 7, 
            plotdir = file.path(north_dir, "presentation_plots"),
            cex.main = 2, 
            datonly = TRUE, showeffN = FALSE)

SSplotComps(replist = north, subplots = 21, print = TRUE,
            kind = "AGE", 
            fleets = 1:4, 
            fleetnames = fleet_names,
            pwidth = 7, pheight = 7, 
            plotdir = file.path(north_dir, "presentation_plots"),
            cex.main = 2, 
            datonly = TRUE, showeffN = FALSE)