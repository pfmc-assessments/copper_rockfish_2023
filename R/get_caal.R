
get_caal <- function(data, len_bins, age_bins, month = 7, fleet = "fill", ageing_error = NA, partition = 0){

len_col <- colnames(data)[grep("length", colnames(data), ignore.case = TRUE)][1]  
age_col <- colnames(data)[grep("age", colnames(data), ignore.case = TRUE)][1] 
sex_col <- colnames(data)[grep("sex", colnames(data), ignore.case = TRUE)][1]
year_col <- colnames(data)[grep("year", colnames(data), ignore.case = TRUE)][1]
    
# Start matrix to save results
data$allLs <- len_bins[findInterval(data[, len_col], len_bins, all.inside = TRUE)]
data$allAs <- age_bins[findInterval(data[, age_col], age_bins, all.inside = TRUE)]

Results = NULL
sex_loop <- unique(data[, sex_col])
  
  #Loop across F then M
  for(s in sex_loop){
    # Loop across years
    year_loop <- sort(unique(data[, year_col][data[, sex_col] == s]))
    for(y in year_loop){
      ########## CONDITIONAL
      # Loop across Length-bins
      for(l in len_bins){
        # Identify relevant rows
        find = which(data[, sex_col] == s & data[, year_col] == y & data[,'allLs'] == l)
        # Skip this year unless there are rows
        if(length(find) > 0){
          # Format reference stuff
          Row = c(
            'year' = y, 
            'month' = month, 
            'fleet'= fleet, 
            'sex' = ifelse(s == "F", 1, ifelse(s == "M", 2, 0)), 
            'partition' = partition, 
            'age_error' = ageing_error, 
            'lbin_lo' = l, 
            'lbin_hi' = l, 
            'input_n'= NA)
          # Loop across age bins
          for(a in age_bins){
            # Subset to relevant rows
            if(a == min(age_bins)) {
              find2 = find[which(data[find, age_col] %in% c(-999, a))]
            } 
            if(a == max(age_bins)){
              find2 = find[which(data[find, age_col] %in% c(Inf, a))]
            }  
            if(!a %in% c(min(age_bins), max(age_bins))){
              find2 = which(data[find,'allAs'] == a)
            } 
            Row = c(Row, length(find2))
          } # End Age loop
          # Add to results matrix
          Row['input_n'] <- sum(as.numeric(Row[10:length(Row)]))
          Results = rbind(Results, Row)
        } # length(Which)
      } # End Length loop
    } # End Year loop
  } # End Sex loop


# Add headers
Results <-  data.frame(Results)
out <- cbind(Results, Results[, 10:ncol(Results)])
colnames(out)[-c(1:9)] = c(paste("A-", age_bins, sep=""), paste("A-", age_bins, sep=""))
  
return(out)
} 