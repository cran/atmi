\name{atmirsi}
\Rdversion{1.1}
\alias{atmirsi}

\title{
Relative Strength Index indicator trading rules
}
\description{
Trading rules based on the Relative Strength Index (RSI). RSI is the normalized ratio between the average of the upward and downward closing prices of the last n days of a security. It was introduced in 1978 by Welles Wilder Jr. and builds on the momentum indicator, where he corrected two of the momentum inefficiencies. On the one hand, the RSI has a fixed value between 0 and 100, which makes it possible to directly compare the indicator values of two securities. On the other hand the calculation of the ratio between the average downward and upward movements leads to smooth indicators, so that it is no longer as sensitive as the momentum to respond to extreme price movements.

}
\usage{
atmirsi(symbol = FALSE, datamatrix = list(NA, NA), period = c(FALSE, FALSE), n = 14, top = 70, bottom = 30, ticks = 1, plot = TRUE, stopl = FALSE, interest = FALSE, transcost = FALSE, startcap = 1000, standard = TRUE, conf.level = 0.95)
}

\arguments{
  \item{symbol}{
a unique series of letters assigned to a security for trading purposes. You can get the symbols by using the function \code{\link{getsymbols}} or from \code{http://finance.yahoo.com/}. If FALSE, a datamatrix is needed.
}
  \item{datamatrix}{
a list of two vectors. The first one is a vector of dates, the second one is a vector of prices.
}
  \item{period}{
is a vector of start and end dates, in \code{"YYYY-MM-DD"} format. If the start date is FALSE, all available data until the end date will be used. If the end date is FALSE, all available data from the start date will be used. If both are FALSE, all available data will be used.
}
  \item{n}{
number of observations for calculation of the average of UP and DOWN closes in the  computation of the indicator value.
}
  \item{top}{
top limit for calculation of the sell signals. It can be between 0 and 100.
}
  \item{bottom}{
bottom limit for calculation of the buy signals. It can be between 0 and 100.
}
  \item{ticks}{
data frequency. For example if ticks=1, every observation will be used in the calculation. If ticks=5, every fifth observation will be used in the calculation.
}
  \item{plot}{
logical; if TRUE, the series with trading signals (buy=green, sell=red) and the indicator will be plotted.
}
  \item{stopl}{
a vector of two arguments for the stop-loss hedging strategy. The first argument can be "s" for static or "d" for dynamic stop-loss hedging. The second argument is the stop-loss limit, it can be between 0 and 1. If stopl=FALSE, no hedging occurs.
}
  \item{interest}{
interest of the non invested capital. It can be a number either greater then zero or FALSE.
}
  \item{transcost}{
transaction costs. It can be a number either greater then zero or FALSE.
}
  \item{startcap}{
initial assets. It can be a number greater than zero. Default 1000 monetary units.
}
  \item{standard}{
logical; If FALSE, the buy and sell signals are successively arranged, where the first trade signal is a signal to buy and the last one is a signal to sell. If TRUE, original trading signals, as they are supplied by the trading rule are returned.
}
  \item{conf.level}{
confidence level for the one-sided confidence interval of number of "hits". "Hits" are signal pairs (a signal to buy and a subsequent signal to sell) which have led to a positive return. The remaining signal pairs are "nonhits".
}
}
\details{
Buy signal occurs, if the RSI line crosses the bottom limit from above down. Sell signal occurs, if the RSI line crosses the top limit from below upwards. To get a trading recommendation for today, the \code{standard} has to be TRUE. Otherwise, in case that after the last Buy signal no Sell signal (based on indicator rule) occurred, the last day  will be used as a Sell signal.
}
\value{
  \item{signale }{matrix with 2 columns of buy and sell signals (indices of data, where signals are occurred).}
  \item{numEqual }{signals occurr on the intersection of two sequences or a sequence and a horizontal line. numEqual is a number of points, where a signal happened and on the day before the first sequence was exactly equal to the second sequence or the horizontal line.}
  \item{settings }{settings, which were used for the calculation of the strategy.}
  \item{result }{Resuts of the strategy as:}
\tabular{ll}{
performance \tab performance of the strategy for the given period.\cr
num. signals \tab number of signal pairs of the strategy for the given period.\cr
hits \tab number of "hits"\cr
leftfront \tab left front of the confidence interval with the confidence level of \code{conf.level}.\cr
performance BAH \tab performance of the buy and hold strategy for the given period.\cr
}
Results are only available, if standard is FALSE.
}
\references{
[1] J. Welles. Wilder. (1978), New concepts in technical trading systems. Trend Research, Greensboro, N.C, 64-70.\cr
[2] Otto. Loistl. (1992), Computergestuetztes Wertpapiermanagement. R. Oldenbourg, Muenchen, 125.\cr
[3] John J. Murphy. (1999), Technical Analysis of the Financial Markets: A Comprehensive Guide to Trading Methods and Applications (New York Institute of Finance), Prentice Hall Press.
}
\author{
Waldemar Kemler, Peter Schaffner
}

\seealso{
\code{\link{atmimomentum}}, \code{\link{atmimacd}}, \code{\link{atminaiv}}, \code{\link{performance}}
}

\examples{
#Example 1: RSI strategy of "AACC" with standard settings (n=14, top=70, bottom=30).
atmirsi("AACC")
#Example 2: RSI strategy of "AACC" with following settings: n=14, top=70, bottom=30, startcapital=1000. With interest of non invested capital (2.5%), trasaction costs (5.9 monetary units) and standard=FALSE.
atmirsi("AACC", standard=FALSE, interest=0.025, transcost=5.9)
#Example 3: RSI strategy of "AACC" for the period from 2004-01-01 through 2005-12-31 with following settings: n=7, top=80, bottom=20, startcapital=1000. With interest of invested capital (2.5%), trasaction costs (5.9 monetary units), standard=FALSE and dynamic stop-loss-hedging (10%).
atmirsi("AACC", period=c("2004-01-01","2005-12-31"), n=7, top=80, bottom=20, standard=FALSE, interest=0.025, transcost=5.9, stopl=c("d",0.1))
}

\keyword{ ts }
