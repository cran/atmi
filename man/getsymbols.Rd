\name{getsymbols}
\Rdversion{1.1}
\alias{getsymbols}
\title{
get stock symbols
}
\description{
Get a list of security symbols of a stock market index from \code{http://finance.yahoo.com/}
}
\usage{
getsymbols(indexsymbol = "GDAXI")
}

\arguments{
  \item{indexsymbol}{
finance.yahoo.com symbol of a stock market index.
}
}
\details{
Examples for indexsymbol:
  \tabular{ll}{
    \kbd{DJI} \tab Dow Jones Industrial Average\cr
    \kbd{NDX} \tab NASDAQ-100, 100 non-financial companies listed on the NASDAQ\cr
    \kbd{GSPC} \tab S&P 500, Standard & Poor 500\cr
    \kbd{IXIC} \tab NASDAQ Composite\cr
    \kbd{GDAXI} \tab German stock market index\cr
    \kbd{TECDAX} \tab 30 largerst German companies from the technology sector
  }
}
\value{
  \item{symbols}{a list of security symbols of a stock market index.}

}

\author{
Waldemar Kemler, Peter Schaffner
}

\section{Warning }{To run the function, an active internet connection is required.}

\seealso{
\code{\link{getdata}}
}

\examples{
# Getting list of symbols for the Dow Jones Industrial Average
dji<-getsymbols("DJI")
dji
}

\keyword{ ts }
