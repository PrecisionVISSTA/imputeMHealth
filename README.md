Install the imputeMHealth package by following codes:

install.packages("devtools")
library(devtools)
install_github("jmybhm/imputeMHealth", force = TRUE)

Code Details
•    Expected data format: matrix format, where each row is a person+brand, and each column is a date. Each row and column is unique.
•    Missing values should be NA, not numbers like 9999, 0, or -1.
•    Each element of input variable has to be matched to consecutive dates. The function operates under the assumption that the vector is sorted from oldest to the most recent date, and that there is no absent date.
•    By default, the package functions starts imputing from the oldest to the most recent missing value. This could result in different imputed values from imputing in the reverse order (from recent to the oldest). If the user would like to impute from the most recent to the oldest date, reverse the order of the input variable.
•    By default, the missing values at the start and end of input variable vector, if they do not have enough prior or post values, will be replaced by the mean of the variable.
•    The built-in functions will technically work with chunks of missing data, but they will end up filling the large block with same values, so it is less meaningful to use this package for big blocks of missing data.

