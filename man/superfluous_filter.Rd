\name{superfluous_filter}
\Rdversion{1.1}
\alias{superfluous_filter}

\title{
 arrange buy and sell signals
}
\description{
Successively arranges buy and sell signals of a trading strategy.
}
\usage{
superfluous_filter(datavec, buy, sell)
}

\arguments{
  \item{datavec}{
a vector of security prices.
}
  \item{buy}{
a vector of indices of a data vector, where the buy signals occurred.
}
  \item{sell}{
a vector of indices of a data vector, where the sell signals occurred.
}
}
\details{
It is not allowed, that a buy and a sell signal occur on the same day.
}
\value{
  \item{buy }{vector of the buy signals.}
  \item{sell }{vector of the sell signals.}
}

\author{
Waldemar Kemler, Peter Schaffner
}

\seealso{
\code{\link{atmirsi}}, \code{\link{atmimacd}}, \code{\link{atmimomentum}}
}
\examples{
datavec<-c(20,25,30,27,22,18,18,24,25,27)
superfluous_filter(datavec, buy=c(NA,2,NA,6), sell=c(26,1,8,9,15,18,NA))
}

\keyword{ ts }