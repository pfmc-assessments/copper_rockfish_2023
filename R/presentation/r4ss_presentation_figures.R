# Create presentation figures based on r4ss output


library(r4ss)
library(here)


dir <- here("models")
south_base <- "14.3_revised_pre-star_base" # "14.3_revised_pre-star_base"
north_base <- "9.11_revised_pre-star_base"
south_dir <-  file.path(dir, "sca", south_base)
north_dir <- file.path(dir, "nca", north_base)
  
south <- SS_output(file.path(dir, "sca", south_base))
north <- SS_output(file.path(dir, "nca", north_base))
dir.create(file.path(south_dir, "presentation_plots"))
dir.create(file.path(north_dir, "presentation_plots"))

fleet_names <- c("Commercial-Dead", "Commercial-Live", "Recreational-CPFV", 
                 "Recreational-PR")
fleet_names <- c("CCFRP Hook and Line", "NWFSC Hook and Line", "NWFSC WCGBT", "Growth")

SSplotComps(replist = south, subplots = 21, print = TRUE,
            kind = "LEN", 
            fleets = 9, 
            #fleetnames = "",#fleet_names,
            pwidth = 7, pheight = 7, 
            plotdir = file.path(wd, south_base, "plots"),
            cex.main = 2, 
            datonly = TRUE, showeffN = TRUE)

SSplotComps(replist = north, subplots = 21, print = TRUE,
            kind = "LEN", 
            fleets = 5, 
            #fleetnames = fleet_names,
            pwidth = 7, pheight = 7, 
            plotdir = file.path(north_dir, "presentation_plots"),
            cex.main = 2, 
            showeffN = TRUE)


SS_plots(south,
         plot = c(15, 18), #c(13, 14, 16, 18),
         html = TRUE,
         datplot = TRUE,
         uncertainty = TRUE,
         maxrows = 2, 
         maxcols = 4, 
         maxrows2 = 2, 
         maxcols2 = 4, 
         printfolder = 'presentation_plots')

SS_plots(north,
         png = TRUE,
         plot = c(18), #c(13, 14, 16, 18),
         html = TRUE,
         datplot = TRUE,
         uncertainty = TRUE,
         maxrows = 2, 
         maxcols = 4, 
         maxrows2 = 2, 
         maxcols2 = 4, 
         printfolder = 'presentation_plots')

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