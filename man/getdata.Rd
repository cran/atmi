\name{getdata}
\Rdversion{1.1}
\alias{getdata}
\title{
getdata
}
\description{
Get the daily finance data for the desired period from \code{http://finance.yahoo.com/}.
}
\usage{
getdata(symbol = FALSE, period = c(startdate = FALSE, enddate = FALSE))
}
\arguments{
  \item{symbol}{
a unique series of letters assigned to a security for trading purposes. You can get the symbols by using the function \code{\link{getsymbols}} or from \code{http://finance.yahoo.com/}. If FALSE, a datamatrix is needed.
}
  \item{period}{
is a vector of start and end dates, in \code{"YYYY-MM-DD"} format. If the \code{startdate} is FALSE, all available data until the \code{enddate} will be used. If the \code{enddate} is FALSE, all available data from the \code{startdate} will be used. If both are FALSE, all available data will be used.
}
}
\value{
  \item{datamatrix}{a matrix object with the following columns::
    \tabular{ll}{
    \kbd{Date } \tab trade date in "YYYY-MM-DD" format\cr
    \kbd{Open } \tab open price\cr
    \kbd{High } \tab the highest price of the day\cr
    \kbd{Low } \tab the lowest price of the day\cr
    \kbd{Close } \tab closing price\cr
    \kbd{Volume } \tab average volume\cr
    \kbd{Adj.Close } \tab adjusted closing price
}	}
}
\note{
The last column \code{Adj.Close} of the returned matrix \code{datamatrix} provides the closing price adjusted for all applicable splits and dividend distributions.
}

\author{
Waldemar Kemler, Peter Schaffner
}

\section{Warning }{To run the function, an active Internet connection is required.}

\seealso{
\code{\link{getsymbols}}
}

\examples{
# Returns data for "AACC" for whole available period.
datamat<-getdata("AACC")
# Returns data for "AACC" for the period from 2008-01-01 through 2008-12-31.
datamat<-getdata("AACC", period=c("2008-01-01","2008-12-31"))
# Returns all available data for "AACC" until 12/31/08.
datamat<-getdata("AACC", period=c(FALSE,"2008-12-31"))
# Returns all available data from 01/01/08. With the usage of function "getsymbols".
symbol<-getsymbols("DJI")
datamat<-getdata(symbol[1], period=c("2008-01-01",FALSE))
datamat
}
\keyword{ ts }
