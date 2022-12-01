#' Create standarized field for RecFIN data
#'
#' @param data read in RecFIN data
#' @param mode_grouping list of area names in data source
#' @param modee_names user area names
#' @param column_name name to do the grouping with (default is based on recfin)
#'
#' @return A data frame 
#'
#' @author Chantel Wetzel
#' @export
#'

#'
#'
recfin_modes <- function(
	data, 
	mode_grouping = list(c("BEACH/BANK","MAN-MADE/JETTY"), "PARTY/CHARTER BOATS", "PRIVATE/RENTAL BOATS", "UNKNOWN"), 
	mode_names = c("shoreside", "cpfv", "private", "unknown"), 
	column_name = NULL){
	
	if(is.null(column_name)) {
		column_name = which(colnames(data) %in% c("RecFIN.Mode.Name", "RECFIN_MODE_NAME", "boat_mode_code"))
	}

	data$mode <- NA
	for (a in 1:length(mode_grouping)){
		get <- paste(mode_grouping[[a]], collapse = "|")
		find <- grep(get, data[, column_name], ignore.case = TRUE)
		data$mode[find] <- mode_names[a]
	}
	data$mode[is.na(data$mode)] <- "unknown"

	return(data)
} 