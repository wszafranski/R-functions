
# This function calculates the empirical CDF of a given dataset.
# The function needs just one sequence/vector of data of any length.
# It will give a matrix of values with n rows and 5 columns
# showing how the calculations were performed.

my_ecdf=function(x)
{
  #x = floor(runif(100, min=0, max=75))
  n = length(x)
  
  ecdf_data = matrix(NA, nrow = n, ncol=5)
  colnames(ecdf_data) = c("values sorted","rank","percentile","rank-check","rev-percentile")
  
  ecdf_data[,1] = sort(x)
  ecdf_data[,2] = 1:n
  ecdf_data[,3] = ecdf_data[,2]/(n+1)
  
  #there will probably be ties (values with same value [,1] but increasing rank & percentile [,3] )
  #adjust ties to the max probability
  
  for (i in 1:n){
    
    # if ranks are tied, then give placeholder 0
    # if not, give placeholder of 1
    # give the last value a 1 (no tie or it is the max value if it is tied)
    if (i == n){
      ecdf_data[i,4] = 1
      
    }else if (ecdf_data[i,1] == ecdf_data[(i+1),1]){
      ecdf_data[i,4] = 0
      
    }else {ecdf_data[i,4] = 1}
  }
  
  # Subset the max values (ecdf_data[,4]==1)
  maxtievalues = subset(ecdf_data, ecdf_data[,4]==1)
  nmaxtie = length(maxtievalues[,1])
  temp4 = matrix(NA, nrow=1, ncol=5)
  
  
  # Adjust the tied percentiles to the max percentile
  for (i in 1:nmaxtie){
    temp3 = subset(ecdf_data, ecdf_data[,1] == maxtievalues[i,1])
    temp3[,5] = maxtievalues[i,3]
    temp4 = rbind(temp4, temp3)
  }
  
  # save in a new data frame
  ecdf_data2 = temp4[2:length(temp4[,1]),]
  
  rm(ecdf_data, maxtievalues, nmaxtie, temp3, temp4)
  
  #ecdf_data2
  return(ecdf_data2)
  
}
