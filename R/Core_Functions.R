#Get adjacent values
Get_adjacent_values_past=function(variable_vector,index, bin_size){
  adjacent_values = numeric();cnt=0;j=index
  while(cnt<bin_size && j>1){
    j=j-1
    if(!is.na(variable_vector[j])){
      adjacent_values= append(adjacent_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  if (j > length(variable_vector)){
    stop("The index is out of range")
  }
  
  if (length(adjacent_values)<bin_size){
    warning('There are not enough past, non-missing values. Additional values are filled using the mean of all values in the variable column')
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(adjacent_values))
    adjacent_values=append(adjacent_values, mean_seq)
  }
  return(adjacent_values)
}

Get_adjacent_values_future=function(variable_vector,index, bin_size){
  adjacent_values = numeric();cnt=0;j=index
  while(cnt<bin_size && j<length(variable_vector)){
    j=j+1
    if(!is.na(variable_vector[j])){
      adjacent_values= append(adjacent_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  if (j > length(variable_vector)){
    stop("The index is out of range")
  }
  
  if (length(adjacent_values)<bin_size){
    warning('There is not enough future, non-missing values. Returned value is the mean of all values in the variable vector')
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(adjacent_values))
    adjacent_values=append(adjacent_values, mean_seq)
  }
  return(adjacent_values)
}



#######Output adjacent, same day of week values######
Get_DoW_values_past=function(variable_vector,index, bin_size){
  dow_values = numeric();cnt=0;j=index

  while(cnt<bin_size && j>7){
    j=j-7
    if(!is.na(variable_vector[j])){
      dow_values=append(dow_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  #Try to avoid where j < 0
  if (j > length(variable_vector)){
    stop("The index is out of range")
  }
  
  if (length(dow_values)<bin_size){
    warning('There is not enough past, non-missing values. Returned value is the mean of all values in the variable vector')
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(dow_values))
    dow_values=append(dow_values, mean_seq)
  }
  return(dow_values)
}

Get_DoW_values_future=function(variable_vector,index, bin_size){
  dow_values = numeric();cnt=0;j=index
  #Try to avoid where j < 0
  while(cnt<bin_size && j<=length(variable_vector)-7){
    j=j+7
    if(!is.na(variable_vector[j])){
      dow_values=append(dow_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  
  if (j > length(variable_vector)){
    stop("The index is out of range")
  }
  
  if (length(dow_values) < bin_size){
    warning('There is not enough future, non-missing values. Returned value is the mean of all values in the variable vector')
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(dow_values))
    dow_values=append(dow_values, mean_seq)
  }
  return(dow_values)
}
