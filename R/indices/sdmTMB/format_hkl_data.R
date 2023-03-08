#' 
#' @param common_name species names as given in the hook and line data set
#' @param data raw hook and line data
#' @param area option to retain only the original sites (Orig121), the sites in 
#' the CCA, or to keep All sites
#'
format_hkl_data <- function(
  common_name = "Bocaccio", 
  data, 
  area = c("Orig121", "CCA", "ALL")[3]) {

  data$number_caught <- as.numeric(data$common_name %in% common_name)  
  data$common_name <- common_name  
  data$crew <- paste0(data$angler_first_name, data$angler_last_name)
  # Change Phillip Ebert's name to 'AAAPhillipEbert'
  # so that the Aggressor also has the crew name that gets set to zero
  data$crew[data$crew %in% "PhillipEbert"] <- "AAAPhillipEbert"  

  total_sample_by_site <- 
    data.frame(
        site_number  = dimnames(table(data$site_number, data$year))[[1]], 
	      samples_across_years = apply(matrix(as.numeric(as.logical(table(data$site_number, data$year))), 
        ncol = ncol(table(data$site_number, data$year))), 1, sum))

  # need to find way to do this without JWToolBox
  data <- match.f(data, total_sample_by_site, "site_number", "site_number", "samples_across_years")

  samples_across_years <- aggregate(
    list(total_caught_by_site = data$number_caught), 
    list(site_number = data$site_number), sum)
    
  data <- data[data$site_number %in% 
      samples_across_years[samples_across_years$total_caught_by_site != 0, "site_number"] & 
      data$samples_across_years >= 2, 
      c("common_name", "number_caught", "year", "site_number", "vessel",
      "drop_latitude_degrees", "drop_longitude_degrees",
      "drop_number", "hook_number", "angler_number", "sex", "crew", 
      "fishing_time_seconds",
      "drop_depth_meters", "swell_height_m", "wave_height_m", 
      "fishing_time_seconds",
      "moon_phase_r", 
      "moon_proportion_fullness_r", 
      "drop_time_proportion_of_solar_day", 
      "weight_kg", "length_cm", "X", "Y")]

  data$hook_number <- as.character(data$hook_number)
  data$hook_number[data$hook_number %in% "1"] <- "1_Bottom"
  data$hook_number[data$hook_number %in% "5"] <- "5_Top"
  
  data$angler_number <- as.character(data$angler_number)
  data$angler_number[data$angler_number %in% "1"] <- "1_Bow"
  data$angler_number[data$angler_number %in% "2"] <- "2_Midship"
  data$angler_number[data$angler_number %in% "3"] <- "3_Stern"
  
  data$year <- factor(as.character(data$year))
  data$site_number <- factor(as.character(data$site_number))
  data$vessel <- factor(as.character(data$vessel))
  data$drop_number <- factor(as.character(data$drop_number))
  data$angler_number <- factor(as.character(data$angler_number))
  data$crew <- factor(as.character(data$crew))
  data$hook_number <- factor(as.character(data$hook_number))
  data$sex <- factor(as.character(data$sex))
  data$moon_phase <- factor(data$moon_phase_r)
  data$swell_height_m <- factor(data$swell_height_m)
  
  # Create Minor Angler Groups
  if(area == "Orig121") {
    data <- data[as.numeric(as.character(data$site_number)) < 500, ]  }
  if(area == "CCA") {
     data <- data[as.numeric(as.character(data$site_number)) > 500, ] }  
  
  mean_crew <- aggregate(list(Mean = data$number_caught), list(crew = data$crew), mean)   
  data <- refactor(data[!data$crew %in% mean_crew[mean_crew$Mean %in% 0, 'crew'], ])    
  data$crew <- as.character(data$crew)  
  data$crew <- factor(data$crew)

  data[,'crew_scaled'] = as.numeric(data[,'crew'], as.is = FALSE) - 1
  data[,'hook_scaled'] = as.numeric(data[,'hook_number'], as.is = FALSE) 
  data[,'drop_scaled'] = as.numeric(data[,'drop_number'], as.is = FALSE) 
  data[,'angler_scaled'] = as.numeric(data[,'angler_number'], as.is = FALSE) 
  data[, "moon_phase"] = as.numeric(data[,"moon_phase"], as.is = FALSE) 
  return(data)
     
}

