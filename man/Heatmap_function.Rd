\name{Heatmap_function}
\alias{Heatmap_function}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Documentation of the Heatmap_function
}
\description{
Heatmap_function draws heatmap in which each row is a week with horizontal bar as day of week.
}
\usage{
Heatmap_function(variable_vector, plot_title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{variable_vector}{Variable, in vector format, that we want to draw heatmap of weekly pattern}
  \item{date_vector}{Date variable in POSIXct format. Needs to be of the same length as the variable vector}
  \item{plot_title}{Title of the plot}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
None
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (variable_vector, plot_title)
{
    mod7 = length(variable_vector)\%\%7
    end = length(variable_vector) - mod7
    heatmap_dat = matrix(variable_vector[0:end], nrow = 7)
    row.names(heatmap_dat) = c("Sunday", "Monday", "Tuesday",
        "Wednesday", "Thursday", "Friday", "Saturday")
    heatmap_dat <- heatmap_dat[nrow(heatmap_dat):1, ]
    heatmap_dat[heatmap_dat == 0] <- NA
    my_palette <- colorRampPalette(c("light blue", "dark blue"))(n = 299)
    heatmap(heatmap_dat, Rowv = NA, Colv = NA, col = my_palette,
        srtCol = 45, main = plot_title)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
