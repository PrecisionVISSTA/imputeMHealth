\name{Compute_linear_fitted}
\alias{Compute_linear_fitted}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to impute missing values by fitting linear model with prior and post dates}
\description{
A function to impute missing values by fitting linear with from the combination of most adjacent dates and the same-day-of week dates. e.g. If March 10th, Monday, is missing, fit linear model with March 3th (Monday in the prior week) ,9th, 11th,and 17th (Monday in the week after).}
\usage{
Compute_linear_fitted(data, var_name, bin_size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Name is data frame
}
  \item{var_name}{
Name of the missing variable, of which we are interested in missing values of}
  \item{bin_size}{
If bin_size=1, grab one date from before, one date ahead, one same-day-of-week in the prior week, and one same-day-of-week in the week after.}
}
\details{
Each element of input variable has to be matched to consecute dates. The function operates under the assumption that the vector is sorted from oldest to the most recent date, and that there is no absent date.

By default the function starts imputing from the oldest to the most recent missing value. This may result in different imputed values from imputing in the reverse order (from recent to the oldest), since the later-imputed missing values may use earlier-imputed values as input for imputation. If you would like to impute from the most recent to the oldest date, reverse the order of the input variable.

By default, the missing values at the start and end of input variable vector, if they do not have enough prior or post values to impute using Compute_DoW_average, will be replaced by the mean of the variable.
}
\value{
Variable, in vector format, with missing values imputed. The non-missing values are same as the original input vector.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Jimin Lauren Yoo
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
X=c(1,2,NA,NA,NA,6,7,8,3,0,4,7,7,5,6,0,2,4,1,4,NA,9,0)
data=data.frame("ID" = 1:23, "X" = X)
Compute_linear_fitted(data, "X", bin_size=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
