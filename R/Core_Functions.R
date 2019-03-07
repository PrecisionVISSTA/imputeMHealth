#Get adjacent values
Get_adjacent_values_past=function(variable_vector,index, bin_size){
  adjacent_values = numeric();cnt=0;j=index
  # if(index < bin_size){print("index is smaller than the bin size"); break}
  while(cnt<bin_size && j>1){
    j=j-1
    if(!is.na(variable_vector[j])){
      adjacent_values= append(adjacent_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  if (length(adjacent_values)<bin_size){
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(adjacent_values))
    adjacent_values=append(adjacent_values, mean_seq)
  }
  return(adjacent_values)
}

Get_adjacent_values_future=function(variable_vector,index, bin_size){
  adjacent_values = numeric();cnt=0;j=index #j=11
  # if(index < bin_size){print("index is smaller than the bin size"); break}
  while(cnt<bin_size && j<length(variable_vector)){
    j=j+1
    if(!is.na(variable_vector[j])){
      adjacent_values= append(adjacent_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  if (length(adjacent_values)<bin_size){
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(adjacent_values))
    adjacent_values=append(adjacent_values, mean_seq)
  }
  return(adjacent_values)
}



#######Output adjacent, same day of week values######
Get_DoW_values_past=function(variable_vector,index, bin_size){
  dow_values = numeric();cnt=0;j=index
  #Try to avoid where j < 0
  #if(index < bin_size*2*7){print("index is smaller than the bin size"); dow_values=rep(NA,bin_size)}
  while(cnt<bin_size && j>7){
    j=j-7
    if(!is.na(variable_vector[j])){
      dow_values=append(dow_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  if (length(dow_values)<bin_size){
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(dow_values))
    dow_values=append(dow_values, mean_seq)
  }
  return(dow_values)
}

Get_DoW_values_future=function(variable_vector,index, bin_size){
  dow_values = numeric();cnt=0;j=index
  #Try to avoid where j < 0
  #if(index < bin_size*2*7){print("index is smaller than the bin size"); dow_values=rep(NA,bin_size)}
  while(cnt<bin_size && j<=length(variable_vector)-7){
    j=j+7
    if(!is.na(variable_vector[j])){
      dow_values=append(dow_values, variable_vector[j])
      cnt=cnt+1
    }
  }
  if (length(dow_values) < bin_size){
    mean_seq=rep(mean(variable_vector, na.rm=TRUE), bin_size-length(dow_values))
    dow_values=append(dow_values, mean_seq)
  }
  return(dow_values)
}
