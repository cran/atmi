\name{atminaiv}
\Rdversion{1.1}
\alias{atminaiv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Naiv trading rules
}
\description{
Analysis and usage of the trading rules, which are based on the double exponential smoothing from Holt.
}
\usage{
atminaiv(symbol = FALSE, trade = FALSE, datamatrix = list(NA, NA), period = c(FALSE, FALSE, FALSE), nAhead = 1, nFit = FALSE, startcap = 1000, sellfront = 0, transcost = FALSE, interest = FALSE, ticks = 1, plot = TRUE)
}

\arguments{
  \item{symbol}{
a unique series of letters assigned to a security for trading purposes. You can get the symbols by using the function \code{\link{getsymbols}} or from \code{http://finance.yahoo.com/}. If FALSE, a datamatrix is needed.
}
  \item{trade}{
logical; if TRUE, on the display appears as an output a trading recommendation as well as the predicted performance for the next day. The trading recommendation can be BUY, SELL or HOLD. If FALSE, the trading strategy will be applied for the given time period based on historical data.
}
  \item{datamatrix}{
a list of two vectors. The first one is a vector of dates, the second one is a vector of prices.
}
  \item{period}{
is a vector of start, end dates and start of prediction, in "YYYY-MM-DD" format. If the start date is FALSE, all available data until the end date will be used. If the end date is FALSE, all available data from the start date will be used. If both are FALSE, all available data will be used. If start of prediction is not FALSE, the prediction will start at this point in time.
}
  \item{nAhead}{
forecast horizon.
}
  \item{nFit}{
period, which is used for fitting of the Holt model parameters alpha and beta.
}
  \item{startcap}{
initial assets. It can be a number greater than zero. Default 1000 monetary units.
}
  \item{sellfront}{
lower limit for the predicted performance. The signal sell occurs when the predicted performace under a given  \code{sellfront} fails.
}
  \item{transcost}{
transaction costs. It can be a number either greater then zero or FALSE.
}
  \item{interest}{
interest of the non invested capital. It can be a number either greater then zero or FALSE.
}
  \item{ticks}{
data frequency. For example if ticks=1, every observation will be used in the calculation. If ticks=5, every fifth observation will be used in the calculation.
}
  \item{plot}{
logical; if TRUE, series with trading signals (buy=green, sell=red) will be plotted.
}

}
\details{
For the optimal choice of smoothing factors alpha and beta, which get daily fitted, the method of minimizing the squared one-step prediction error is used.\cr
If \code{trade} is TRUE, the arguments \code{interest} and \code{period} will be automaticaly set to FALSE.
}
\value{
  \item{buy }{a vector of indices of a data vector, where the buy signals occurred.}
  \item{sell }{a vector of indices of a data vector, where the sell signals occurred.}
  \item{fin.cap }{final capital.}
}
\note{
The following tradig rules are used to calculate buy and sell signals.\cr
BUY: \eqn{r_(t+1)>(2*transcost)/(cap_(t))}\cr
SELL: \eqn{r_(t+1)<sellfront}\cr
with \eqn{r_(t+1)=C_(t+1)/C_(t)-1}\cr
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
\code{\link{HoltWinters}}, \code{\link{naivpredict}}
}
\examples{
#Example 1: Atminaiv strategy with standard settings.
atminaiv(symbol="ABMC")
#Example 2: Atminaiv strategy for "AANB". The data is available for the period from 2007-01-01 through 2008-12-31, but the prediction starts on 2008-01-01.
atminaiv(symbol="AANB", period=c("2007-01-01", "2008-12-31", "2008-01-01"), transcost=5.9, interest=0.025, startcap=10000)
#On the display appears as an output of the call the final capital as well as the performance of the strategy.
#Example 3: The call of the following command, provides a trading recommendation for the next day based on the atminaiv strategy.
atminaiv(symbol="AANB", trade=TRUE, transcost=5.9, startcap=1000)
}
\keyword{ ts }
