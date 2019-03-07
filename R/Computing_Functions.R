#########IMPORTED FROM Core_Functions.R file#############
######################################################

Compute_adjacent_average <- function(data, var_name, bin_past=0, bin_future=0){

  var_to_impute=data[,var_name]
  missing_index=which(is.na(var_to_impute))
  #Iterate through missing values
  for (i in missing_index){
    #past adjacent values
    past_values=Get_adjacent_values_past(var_to_impute, i, bin_past)
    #future adjacent values
    future_values=Get_adjacent_values_future(var_to_impute, i, bin_future)

    #mean of past and future adjacent values
    average=sum(past_values, future_values)/(bin_past+bin_future)
    #replace the missing value with the average
    var_to_impute[i]=average
  }
  return(var_to_impute)
}


Compute_DoW_average <- function(data, var_name, bin_past=0, bin_future=0){

  var_to_impute=data[,var_name]
  missing_index=which(is.na(var_to_impute))
  #Iterate through missing values
  for (i in missing_index){
    #past adjacent values
    past_values=Get_DoW_values_past(var_to_impute, i, bin_past)
    #future adjacent values
    future_values=Get_DoW_values_future(var_to_impute, i, bin_future)
    #mean of past and future adjacent values
    average=sum(past_values, future_values)/(bin_past+bin_future)
    #replace the missing value with the average
    var_to_impute[i]=average
  }
  return(var_to_impute)
}


Compute_linear_fitted <- function(data, var_name, bin_size){

  lm_df=data.frame(matrix(0, ncol = bin_size*4, nrow = nrow(data)))
  lm_df$var_to_impute=data[,var_name]

  missing_index=which(is.na(lm_df$var_to_impute))
  for (i in 1:nrow(lm_df)){
    #print('missing index:');print(missing_index)
    Adjacent_values=append(Get_adjacent_values_past(lm_df$var_to_impute, i, bin_size), Get_adjacent_values_future(lm_df$var_to_impute, i, bin_size))
    DoW_values=append(Get_DoW_values_past(lm_df$var_to_impute, i, bin_size), Get_DoW_values_future(lm_df$var_to_impute, i, bin_size))
    lm_df[i,1:(bin_size*4)]=c(Adjacent_values, DoW_values)
  }

  Missing_df=lm_df[missing_index,]
  Nonmissing_df=lm_df[-missing_index,]
  lmMod <- lm(var_to_impute ~ ., data=Nonmissing_df)  # build the model
  Pred <- predict(lmMod, Missing_df)

  lm_df$var_to_impute[missing_index]=Pred

  return(lm_df$var_to_impute)
}
