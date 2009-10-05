\name{stoploss}
\Rdversion{1.1}
\alias{stoploss}
\title{
stop-loss hedging
}
\description{
Dynamic or static stop-loss hedging strategy.
}
\usage{
stoploss(datavec, buy, sell, stopl=c("s",0.1))
}
\arguments{
  \item{datavec}{
a vector of security prices.
}
  \item{buy}{
a vector of indices of a data vector, where buy signals occurred.
}
  \item{sell}{
a vector of indices of a data vector, where sell signals occurred.
}
  \item{stopl}{
a vector of two arguments for the stop-loss hedging strategy. The first argument can be "s" for static or "d" for dynamic stop-loss hedging. The second one is the stop-loss limit, it can be between 0 and 1. If \code{stopl=FALSE}, no hedging occurrs.
}
}
\details{
In a static stop-loss, the securities are sold when the stock price falls below  a certain percentage of the purchase price. In a dynamic stop-loss, the basis for the stop-loss calculation is not the last purchase price, but the highest security price after the buy signal.\cr
It is not necessary, that the vectors of buy and sell signals are in an ordered form. They can also contain NAs.
}
\value{
  \item{sell }{new sell signals, after stop-loss hedging.}
}
\references{
[1] Johannes Welcker. (1994), Technische Aktienanalyse, volume 7. Verlag Moderne Industrie, Zuerich.
}
\author{
Waldemar Kemler, Peter Schaffner
}
\seealso{
\code{\link{atmimacd}}, \code{\link{atmirsi}}, \code{\link{atmimomentum}}, \code{\link{performance}}
}
\examples{
datavec<-c(20,25,32,27,22,18,18,24,25,27)
#Example 1: static stop-loss hedging with 10% limit.
sell<-stoploss(datavec, buy=c(2), sell=c(8), stopl=c("s",0.1))
sell
#Example 2: dynamic stop-loss hedging with 10% limit.
sell<-stoploss(datavec, buy=c(2), sell=c(8), stopl=c("d",0.1))
sell
#Example 3: dynamic stop-loss hedging with 10% limit.
sell<-stoploss(datavec, buy=c(5,NA,2), sell=c(8,NA), stopl=c("d",0.1))
sell
}
\keyword{ ts }