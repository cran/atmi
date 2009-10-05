\name{naivpredict}
\Rdversion{1.1}
\alias{naivpredict}

\title{
Time series forecasting using Holt exponential smoothing
}
\description{
Time series forecasting using Holt exponential smoothing. In this model it is assumed that the given time series contains a trend, however, it shows no seasonal variations.
}
\usage{
naivpredict(symbol = FALSE, datamatrix = list(NA, NA), period = c(FALSE, FALSE), nAhead = 5, plot = TRUE, stats = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{symbol}{
a unique series of letters assigned to a security for trading purposes. You can get the symbols by using the function \code{\link{getsymbols}} or from \code{http://finance.yahoo.com/}. If FALSE, a datamatrix is needed.
}
  \item{datamatrix}{
a list of two vectors. The first one is a vector of dates, the second one is a vector of prices.
}
  \item{period}{
is a vector of start and end dates, in "YYYY-MM-DD" format. If the start date is FALSE, all available data until the end date will be used. If the end date is FALSE, all available data from the start date will be used. If both are FALSE, all available data will be used. Prediction starts from the end date.
}
  \item{nAhead}{
forecast horizon
}
  \item{plot}{
logical; if TRUE,  the series for the given period as well as prediction will be plotted.
}
  \item{stats}{
if stats=TRUE, statistics like \code{summary} in linear models  as well as residual diagnostic plots will be shown.
}
}
\details{
The function \code{\link{naivpredict}} is based on the function \code{\link{HoltWinters}} with \code{gamma=FALSE}.\cr
For the optimal choice of smoothing factors alpha and beta, the method of minimizing the squared one-step prediction error is used.\cr
}
\value{
  \item{pred }{predicted time series values.}
}
\references{
[1] Charles C. Holt (2007). Forecasting seasonals and trends by exponentially weighted moving averages. International Journal of Forecasting, 20(1), 7\cr
[2] Francis X. Diebold (2007). Elements of forecasting. Thomson Learning, London, 315, 316\cr
[3] Winfried Stier (2001). Methoden der Zeitreihenanalyse. Springer, Berlin [u.a.], 24
}
\author{
Waldemar Kemler, Peter Schaffner
}
\section{Warning }{To run the function, an active internet connection is required.}
\seealso{
\code{\link{atminaiv}}, \code{\link{HoltWinters}}
}
\examples{
#Prediction of Price of "AACC" for the next 5 days.
naivpredict("AACC", nAhead=5)
}
\keyword{ ts }
