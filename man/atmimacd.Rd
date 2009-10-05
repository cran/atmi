\name{atmimacd}
\Rdversion{1.1}
\alias{atmimacd}

\title{
Moving Average Convergence Divergence indicator trading rules
}
\description{
Trading rules are based on the Moving average Convergence/Divergence (MACD), which were introduced by Gerald Appel in 1979. MACD is based on the concept of exponential moving average and today is one of the most well-known and most widely used indicators. The specific feature of this indicator is, that it can be used depending on the interpretation as a trend follower as well as an oscillator.
}
\usage{
atmimacd(symbol = FALSE, datamatrix = list(NA, NA), period = c(FALSE, FALSE), nfast = 12, nslow = 26, trigger = 9, rule = 1, ticks = 1,
 plot = TRUE, stopl = FALSE, interest = FALSE, transcost = FALSE, startcap = 1000, standard = TRUE, conf.level = 0.95)
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
  \item{nfast}{
number of dates for the fast moving average.
}
  \item{nslow}{
number of dates for the slow moving average.
}
  \item{trigger}{
number of dates for the signal moving average.
}
  \item{rule}{
can be 1 or 2. If equal to 1, then the buy signal occurs, if the MACD line crosses the signal line from below. The sell signal occurs, if the MACD line crosses the signal line from above. The rule 2 is the same as 1, except that the buy/sell signals occur only below/above the zero line.
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
To get a trading recommendation for today, the \code{standard} has to be TRUE. Otherwise, in case that after the last buy signal no sell signal (based on indicator rule) occurred, the last day  will be used as a sell signal.
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
[1] Otto. Loistl. (1992), Computergestuetztes Wertpapiermanagement. R. Oldenbourg, Muenchen.\cr
[2] John J. Murphy. (1999), Technical Analysis of the Financial Markets: A Comprehensive Guide to Trading Methods and Applications (New York Institute of Finance), Prentice Hall Press, 254.
}
\author{
Waldemar Kemler, Peter Schaffner
}

\seealso{
\code{\link{atmimomentum}}, \code{\link{atmirsi}}, \code{\link{atminaiv}}, \code{\link{performance}}
}
\examples{
#Example 1: MACD strategy of "AACC" with standard settings (rule=1, nfast=12, nslow=26, trigger=9).
atmimacd("AACC")
#Example 2: MACD strategy of "AACC" with rule 2 and following settings: nfast=12, nslow=26, trigger=9, startcapital=1000. With interest of invested capital (2.5%), trasaction costs (5.9 monetary units) and standard=FALSE.
atmimacd("AACC", rule=2, standard=FALSE, interest=0.025, transcost=5.9)
#Example 3: MACD strategy of "AACC" for the period from 2004-01-01 through 2005-12-31 with rule 2 and following settings: nfast=15, nslow=35, trigger=10, startcapital=1000. With interest of non invested capital (2.5%), trasaction costs (5.9 monetary units), standard=FALSE and dynamic stop-loss-hedging (10%).
atmimacd("AACC", period=c("2004-01-01","2005-12-31"), nfast=15, nslow=35, trigger=10, rule=2, standard=FALSE, interest=0.025, transcost=5.9, stopl=c("d",0.1))
}

\keyword{ ts }
