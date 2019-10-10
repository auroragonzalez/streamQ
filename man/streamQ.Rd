\name{streamQ}
\alias{streamQ}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
streamQ
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function receives a numerical data stream and computes its outliers. The Quality of such stream is defined as Q = 1 - percentage of outliers.
}
\usage{
streamQ(datastream)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datastream}{
%%     ~~Describe \code{datastream} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (datastream)
{
    df <- jsonlite::stream_in(url(datastream))
    dfjson <- jsonlite::fromJSON(jsonlite::toJSON(df))
    mydata <- dfjson$nameValuePairs
    columnnames <- colnames(mydata)
    x <- data.frame(mydata$inDateTime$nameValuePairs)
    tempout_index <- grep(columnnames[grepl("temperature_indoor",
        columnnames)], colnames(mydata))
    xy <- data.frame(x, mydata[tempout_index])
    xy <- na.omit(xy)
    colnames(xy) <- c("time", "tempout")
    summary(xy)
    xy = xy[1:3, ]
    dat.ts <- ts(xy$tempout, frequency = 1)
    data.ts.outliers <- tso(dat.ts, types = c("AO", "LS", "TC",
        "IO", "SLS"))
    Q = 1 - nrow(data.ts.outliers$outliers)/nrow(xy)
    return(Q)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
