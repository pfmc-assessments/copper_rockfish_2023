devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/indexwc")
devtools::load_all("C:/Users/Chantel.Wetzel/Documents/GitHub/nwfscSurvey")

library(sdmTMB)
library(ggplot2)

dir <- "C:/Assessments/2023/copper_rockfish_2023/data/wcgbt/south/index"
dir_main <- "C:/Assessments/2023/copper_rockfish_2023/data/wcgbt"

species <- 'copper_rockfish'
obs <- 'lognormal'
knots <- 250

#### Get species, survey, and strata info
info <- nwfscSurvey::GetSpp.fn(species)

# Change here to remove the selection of only the WCGBT survey
survey_list <- rev(grep("Tri|Combo|Slope",
  nwfscSurvey::createMatrix()[, 1], value = TRUE))[2]

strata_limits <- VASTWestCoast::convert_strata4vast(
	overridedepth = TRUE,
  strata = GetStrata.fn(area = 'ptconception')
)

sppdir <- dir
dir.create(sppdir, recursive = TRUE, showWarnings = FALSE)

survey <- 'NWFSC.Combo'

# Loop over selected surveys
dir.create(file.path(sppdir, "data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(sppdir, "index"), recursive = TRUE, showWarnings = FALSE)

# modify for other surveys & whether to include vessel effects
# without RE
formula = list(
	Catch_mt ~ 0 + as.factor(Year) + pass_scaled,
  Catch_mt ~ 0 + as.factor(Year) + pass_scaled)

spp <- paste(survey, info[, "scientific_name"], sep = "_")

# Pull survey data
catch = pull_catch(
    dir = dir, 
    common_name = "copper rockfish",
    survey = "NWFSC.Combo",
    convert  = TRUE)
catch$Total_catch_wt_kg <- catch$total_catch_wt_kg
# Filter down to California data only
catch <- catch[catch$Latitude_dd < 34.5, ]
catch_data <- format_data(data = catch)
catch_data$area <- "south_pt_conception"

# Rounding is dealing with a WCGBT tow at 31.99861 latitude that was getting dropped
if(dim(strata_limits)[1] > 1) {
	for (a in 2:dim(strata_limits)[1]) {
		ind <- which(round(catch_data$Lat, 2) < strata_limits[a, "north_border"] & 
			round(catch_data$Lat, 2) >= strata_limits[a, "south_border"])
		catch_data$area[ind] <- strata_limits[a, "STRATA"] 
	}
}

# Summarize tows by year and area
cw_tows <- aggregate(Tow ~ Year, catch_data, length, drop = FALSE)
area_tows <- aggregate(Tow ~ Year + area, catch_data, length, drop = FALSE)

# Summarize available data
surveyspp <- VASTWestCoast::get_spp(input = spp)
data_years <- sort(unique(catch_data$Year))

# create subdirectory folder
dir.create(file.path(sppdir, "index", obs), recursive = TRUE, showWarnings = FALSE)

if (obs == "gamma"){
	family = sdmTMB::delta_gamma()
} 
if (obs == "lognormal"){
	family = sdmTMB::delta_lognormal()
} 
if (!obs %in% c("gamma", "lognormal")){
	# Add error statement and stop run
}

# Create mesh
mesh_survey <- VASTWestCoast::convert_survey4vast(
	survey = surveyspp["survey"])

mesh <- VASTWestCoast::VAST_mesh(
	data = catch_data,
  survey = mesh_survey,
  numknots = knots,
  savedir = file.path(sppdir,"index", obs))
data_mesh <- mesh$mesh$data.inner
				
# Create prediction grid
grid <- VASTWestCoast::get_inputgrid(survey = mesh_survey)
grid <- grid[grid$Area_km2 > 0, ]
grid <- sdmTMB::add_utm_columns(grid, c("Lon", "Lat"), utm_crs = 32610) # UTM 10
grid$pass_scaled <- 0
grid$vessel_scaled <- data_mesh$vessel_scaled[1]
sub_grid <- dplyr::select(grid, X, Y, Area_km2, pass_scaled, vessel_scaled, Lat, Lon)
		
year_grid <- purrr::map_dfr(data_years, function(yr) {
  sub_grid$Year <- yr
  sub_grid
})

mesh <- sdmTMB::make_mesh(
	data = data_mesh,
	xy_cols = c("X", "Y"),
	n_knots = knots
)

# plot and save the mesh
grDevices::png(filename = file.path(sppdir,"index", obs, "mesh.png"),
  width = 7, height = 7, units = "in", res = 300, pointsize = 12)
plot(mesh)
dev.off()

data_mesh$vessel_scaled <- as.factor(data_mesh$vessel_scaled)
		
# Run model
fit_delta_spatial_spatiotemporal <- sdmTMB::sdmTMB(
	formula = formula,
	time = "Year",
	offset = log(data_mesh$Area_swept_km2),
	data = data_mesh,
	mesh = mesh,
	family = family,
	spatial = "on",
	spatiotemporal = list("off", "off"),
	anisotropy = TRUE,
	silent = TRUE,
	control = sdmTMB::sdmTMBcontrol(
	  newton_loops = 1L,
	  map = list(ln_H_input = factor(c(1, 2, 1, 2))) # <- force sdmTMB to share anisotropy parameters across the two delta models
	),
	do_index = TRUE,
	predict_args = list(newdata = year_grid, re_form_iid = NA),
	index_args = list(area = year_grid$Area_km2)
)

fit_delta_spatial <- sdmTMB::sdmTMB(
	formula = formula,
	time = "Year",
	offset = log(data_mesh$Area_swept_km2),
	data = data_mesh,
	mesh = mesh,
	family = family,
	spatial = "on",
	spatiotemporal = list("off", "off"),
	silent = TRUE,
	do_index = TRUE,
	predict_args = list(newdata = year_grid),
	index_args = list(area = year_grid$Area_km2)
)

index <- sdmTMB::get_index(
	fit_delta_spatial, 
	bias_correct = FALSE # TRUE
)
index$area <- "south_pt_conception" 

get_diagnostics(
	dir = file.path(sppdir, "index", obs), 
	fit = fit_delta_spatial, 
	prediction_grid = year_grid
)

# Need to fix this for plotting
all_indices <- rbind(index, area_indices)
index$area = "coastwide"
# Plot the index
plot_indices(data = index, #all_indices, 
	plot_info = info, 
	save_loc = file.path(sppdir, "index", obs), 
	ymax = NULL)

save(data_mesh, mesh, year_grid, index, fit, area_indices, all_indices,
	loglike, aic, catch_data, #plot_info,
	file = file.path(sppdir, survey, "index", obs, "sdmTMB_save.RData")
)


fit_delta <- sdmTMB::sdmTMB(
	formula = formula,
	time = "Year",
	offset = log(data_mesh$Area_swept_km2),
	data = data_mesh,
	mesh = mesh,
	family = family,
	spatial = "off",
	spatiotemporal = list("off", "off"),
	silent = TRUE,
	do_index = TRUE,
	predict_args = list(newdata = year_grid),
	index_args = list(area = year_grid$Area_km2)
)

index <- sdmTMB::get_index(
	fit_delta, 
	bias_correct = TRUE
)
index$area <- "coastwide" 

fit <- sdmTMB::sdmTMB(
	formula = Catch_mt ~ 0 + as.factor(Year) + pass_scaled,
	time = "Year",
	offset = log(data_mesh$Area_swept_km2),
	data = data_mesh,
	mesh = mesh,
	family = tweedie(link = "log"),
	spatial = "off",
	spatiotemporal = list("off", "off"),
	silent = TRUE,
	do_index = TRUE,
	predict_args = list(newdata = year_grid),
	index_args = list(area = year_grid$Area_km2)
)

index_glmm <- sdmTMB::get_index(
	fit, 
	bias_correct = TRUE
)
index_glmm$area = 'coastwide'

data <- index_glmm
plot_info = info
save_loc = file.path(sppdir, "index", obs)
ymax <-35000

  colors <- viridis::viridis(4)
  years <- sort(unique(data$Year))
  hi <- data[data$area == "coastwide", "upr"]
  lo <- data[data$area == "coastwide", "lwr"]
  est <- data[data$area == "coastwide", "est"]

  out_file = file.path(save_loc, paste0(plot_info$common_name, "_", plot_info$survey, "fit_glmm_Index.png"))
  grDevices::png(filename = out_file,
    width = 10, height = 7, units = "in", res = 300, pointsize = 12)

  cex.axis = 1.25
  cex.lab = 1.20

  plot(0, type = "n",
      xlim = range(years),
      ylim = c(0, ymax),
      xlab = "", ylab = "", yaxs = "i",
      main = "", cex.axis = cex.axis)

  graphics::mtext(side = 1 , "Year", cex = cex.lab, line = 3)
  graphics::mtext(side = 2, "Index (mt)", cex = cex.lab, line = 2.5)
  graphics::mtext(side = 3, text = stringr::str_to_title(plot_info$common),
    font = 2, cex = cex.lab, line = 0.25)
  graphics::mtext(side = 3, text = "",
    font = 2, cex = cex.lab, line = -1.75)
  graphics::arrows(x0 = years, y0 = lo, x1 = years, y1 = hi, col = colors[1],
    angle = 90, code = 3, length = 0.01)
  graphics::points(years, est, pch = 17, bg = 1, cex = 1.6, col = colors[1])
  graphics::lines(years,  est,cex = 1, lwd = 2, col = colors[1])

  dev.off()