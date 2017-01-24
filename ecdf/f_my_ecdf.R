
my_ecdf = function (x){
  
  n = length(x)
  
  ecdf_data = matrix(NA, nrow = n, ncol=6)
  colnames(ecdf_data) = c("values_sorted","rank","bad_percentile","duplicate","final_percentile","fdc_percentile")
  
  ecdf_data[,1] = sort(x)
  ecdf_data[,2] = 1:n
  ecdf_data[,3] = ecdf_data[,2]/(n+1)
  
  # there will probably be ties (values with same value in column 1, 
  # but increasing rank & percentile [,3] )
  # adjust ties to the max probability
  
  
  # flag duplicate values with TRUE (1). This first instance
  # isn't flagged, only the 2nd, 3rd, etc.
  # 'fromLast=TRUE' makes this occur from the bottom - the way I want.
  ecdf_data[,4] = duplicated(ecdf_data[,1], fromLast = TRUE)
  
  
  # Fill in the "revised" percentile in the case of ties
  for (i in n:1){
    #13, 12, 11, 10, 9
    
    if(ecdf_data[i, 4] == 0){
      ecdf_data[i, 5] = ecdf_data[i, 3]
    }else {
      ecdf_data[i, 5] = ecdf_data[(i+1), 5]
    }
    
  }
  
  # Make the last column flow duration percentiles (1- col 5)
  # as opposed to CDF percentiles
  ecdf_data[, 6] = 1-ecdf_data[, 5]
  

  NOTE_ON_FUNCTION_OUTPUT = c("The original values sorted", "Rank of the sorted values", 
                              "Original (bad) CDF Percentile", "Check for Duplicate Values", 
                              "Final CDF Percentile", "Flow Duration Curve Percentiles",
                              " ")

  Output <- data.frame(NOTE_ON_FUNCTION_OUTPUT, row.names = c("Columm 1", "Columm 2", 
                                                              "Columm 3", "Columm 4",
                                                              "Column 5", "Column 6",
                                                              " "))
  print(Output, justify = c("left"))
  
  ecdf_data
  
}
  
