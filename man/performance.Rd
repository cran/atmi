\name{performance}
\Rdversion{1.1}
\alias{performance}
\title{
performace of a trading strategy
}
\description{
Calculates a performance of a trading strategy with the given signals to buy and sell. If necessary, with consideration of transaction costs and/or interest.
}
\usage{
performance(datavec = NA, datevec = NA, buy = NA, sell = NA, startcap = 1000, interest = 0.025, transcost = 5.9)
}

\arguments{
  \item{datavec}{
a vector of security prices.
}
  \item{datevec}{
a vector of dates.
}
  \item{buy}{
a vector of indices of data vector, where the  buy signals occurred.
}
  \item{sell}{
a vector of indices of data vector, where the sell signals occurred.
}
  \item{startcap}{
initial assets. It can be a number greater than zero. Default is 1000 monetary units.
}
  \item{interest}{
the annual interest rate. It can be a number either greater than zero or FALSE.
}
  \item{transcost}{
transaction costs. It can be a number either greater than zero or FALSE.
}
}
\details{
The function requires  successively buy and sell signals. This can be done by using the function \code{\link{superfluous_filter}}. In the calculation of the performace any divisibility of securities and a steady, deterministic interest rate is assumed. It is also assumed, that the opening price of a security is equal to the closing price of the day before and that it is always traded at the opening price. An additional assumption is that a year has 360 days.
}
\value{
  \item{return.strategy }{performance of the given strategy, which is calculated with the given buy and sell signals.}
  \item{returns }{single returns of the given strategy, which are calculated with the buy and sell signals.}
  \item{return.buy.and.hold }{performance of the buy and hold strategy for the given security and period. Usually it is used as a benchmark.}
}

\note{
The performance of the buy and hold strategy \code{return.buy.and.hold} will be calculated between the first and the last date independent from the buy and sell signals.\cr
In consideration of transaction costs they will be checked before every purchase, whether sufficient funding is available to finance a commercial transaction (purchase and sale of a security), so that the reference account must show an account balance of more than \code{2 x transcost} monetary units before every purchase. A purchase fee of \code{transcost} monetary units will be deducted directly at the purchase from this amount. The remaining amount of the \code{2 x transcost} monetary units remains up to the sale as a security deposit on the account. It is made to ensure, that the sale fee can be paid in any case.\cr
If the payment of interest is considered in the analysis, the non invested capital as well as the security amount (sale fee) will also lead to interest.
}

\author{
Waldemar Kemler, Peter Schaffner
}

\seealso{
\code{\link{stoploss}}, \code{\link{superfluous_filter}}
}
\examples{
# Initial situation:
datavec<-c(20,25,30,27,22,18,18,24,25,27)
datevec<-seq(as.Date("2009-01-01"), as.Date("2009-01-10"), by = "day")
# Example 1: Performance witout interest and transaction costs.
per<-performance(datavec=datavec,datevec=datevec, buy=c(2,5), sell=c(4,9), startcap=1000, interest=FALSE, transcost=FALSE)
# Example 2: Performance with consideration of interest (2.5 per cent) and transaction costs (5.9 monetary unit).
per<-performance(datavec=datavec,datevec=datevec, buy=c(2,6), sell=c(5,8), startcap=1000, interest=0.025, transcost=5.9)
}

\keyword{ ts }