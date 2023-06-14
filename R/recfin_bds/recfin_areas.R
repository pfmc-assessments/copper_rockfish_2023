#' Create standarized field for RecFIN data
#'
#' @param data read in RecFIN data
#' @param area_grouping list of area names in data source
#' @param area_names user area names
#' @param column_name name to do the grouping with (default is based on recfin)
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'
#' @examples
#' recfin_areas(data = recfin,
#' area_grouping = list(c("CHANNEL", "SOUTH"), c("BAY AREA", "WINE", "CENTRAL", "REDWOOD", "NOT KNOWN")),
#' area_names = c("south_pt_concep", "north_pt_concep")
#' )
#'
#'
recfin_areas <- function(data, area_grouping, area_names, column_name){

	data$area <- NA
	for (a in 1:length(area_grouping)){

		get <- paste(area_grouping[[a]], collapse = "|")
		find = grep(get, data[,column_name], ignore.case = TRUE)
		data$area[find] = area_names[a]
	}
	
	return(data)
}