###########################################################################
###Heatmap function###
###########################################################################
# Input: A health variable, date variable in POSIXct format, plot_title
# Output: returns a heatmap with x-axis as week number and y-axis as day of week
Heatmap_function <- function(variable_vector, date_vector=NULL,plot_title){
  #Index of first and last Monday in the data
  first_Monday_index=head(which(wday(date_vector)==1), n=1)
  last_Sunday_index=tail(which(wday(date_vector)==7), n=1)
  
  #Convert dataframe into matrix
  heatmap_dat=matrix(variable_vector[first_Monday_index:last_Sunday_index], nrow=7)
  #Set row names as Day of Week
  row.names(heatmap_dat) = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  heatmap_dat <- heatmap_dat[nrow(heatmap_dat):1, ]
  #Convert zeros to NAs
  heatmap_dat[heatmap_dat == 0] <- NA
  #Draw heatmap
  my_palette <- colorRampPalette(c("light blue", "dark blue"))(n = 299)
  heatmap(heatmap_dat, Rowv=NA, Colv=NA, col=my_palette, main=plot_title)
}

###########################################################################
###Fucntion--Percent of Ourliters###
###########################################################################
# Input: a column in data
# Output: returns the percentage of statistically calculated (outside of 1.5*IQR) outliers

outliers_percent <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  length(which(x < (qnt[1] - H)| x > (qnt[2] + H))) / length(x)
}

