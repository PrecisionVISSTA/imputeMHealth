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
    #initialize lm data frame
  lm_df=data.frame(matrix(0, ncol = bin_size*4, nrow = nrow(data)))
  lm_df$var_to_impute=data[,var_name]
  
  #Iterate through missing index
  missing_index=which(is.na(lm_df$var_to_impute))
  for (i in 1:nrow(lm_df)){
    #"Grab" equal number of past and future adjacent values
    Adjacent_values=append(Get_adjacent_values_past(lm_df$var_to_impute, i, bin_size), Get_adjacent_values_future(lm_df$var_to_impute, i, bin_size))
    #Grab equal number of past and future same-day-of-week values
    DoW_values=append(Get_DoW_values_past(lm_df$var_to_impute, i, bin_size), Get_DoW_values_future(lm_df$var_to_impute, i, bin_size))
    #Fill lm data frame with adjacent and DoW values
    lm_df[i,1:(bin_size*4)]=c(Adjacent_values, DoW_values)
  }
  #Train linear model using non-missing data--Outcome: variable to impute
  Missing_df=lm_df[missing_index,]
  Nonmissing_df=lm_df[-missing_index,]
  lmMod <- lm(var_to_impute ~ ., data=Nonmissing_df)  # build the model
  #Use the trained model to predict missing data
  Pred <- predict(lmMod, Missing_df)

  lm_df$var_to_impute[missing_index]=Pred

  return(lm_df$var_to_impute)
}
