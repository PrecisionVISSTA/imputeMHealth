\name{Get_DoW_values_future}
\alias{Get_DoW_values_future}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Documentation of Get_DoW_values_future
}
\description{
The function that grabs the non-missing, same day-of-week values that are ahead of the missing value we are trying to impute. e.g. If it is Monday that is missing, grab Monday values of the week after, 2 weeks after, etc.
}
\usage{
Get_DoW_values_future(variable_vector, index, bin_size)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable_vector}{Variable, in vector format, of which we would like to impute missing values}
  \item{index}{Index of missing value we would like to impute}
  \item{bin_size}{Number of non-missing future values to grab, which will be used for imputation}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Vector of non-missing same day-of-week values, as many as the indicated bin size, ahead of the missing value to impute
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
\examples{
temp=c(1,2,3,4,NA,NA,5,8,9,10,11,12,13,14, 15,16,17,18,19,20,21)
Get_DoW_values_future(temp, index=20, bin_size=3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
