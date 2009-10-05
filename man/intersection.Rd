\name{intersection}
\Rdversion{1.1}
\alias{intersection}
\title{
intersection points
}
\description{
Identifies intersection points of two sequences or a sequence and a horizontal line.
}
\usage{
intersection(sequenceInd = NA, sequenceSig = NA, hLine = NA, plot = TRUE)
}
\arguments{
  \item{sequenceInd}{
the first sequence (the first sequence (a data vector).).
}
  \item{sequenceSig}{
the second sequence (a data vector).
}
  \item{hLine}{
the horizontal line (a number).
}
  \item{plot}{
logical; if TRUE, the intersection will be plotted.
}
}
\details{
The used sequences can contain NAs only at the beginning. The function was written for the analysis of technical indicators, which are based on closing prices. Therefore an intersection point can only be identified, when the intersection has already happened. For example if an intersection occurred between day 5 and 6, the 6th day is an intersection day (see example 1).
}
\value{
  \item{upIntersection }{vector of indices, where the \code{sequenceInd} crossed the \code{sequenceSig} or the \code{hLine} from below.}
  \item{downIntersection }{vector of indices, where the sequenceInd crossed the sequenceSig or the hLine from above.}
  \item{numEqualUp }{number of points, where an \code{UpIntersection} happened and on the day before the sequenceInd was exactly equal to sequenceSig  or the hLine.}
  \item{numEqualDown }{number of points, where an \code{DownIntersection} happened and on the day before the \code{sequenceInd} was exactly equal to \code{sequenceSig}  or the \code{hLine} (see example 3).}
}

\author{
Waldemar Kemler, Peter Schaffner
}

\examples{
# Example 1: sequenceInd crosses sequenceSig from below upwards between day 5 and 6.
intersection(sequenceInd=c(1:10),sequenceSig=c(10:1),plot=TRUE)
# Example 2: sequenceInd crosses a hLine.
intersection(sequenceInd=c(1:10),hLine=5,plot=TRUE)
# Example 3: sequenceInd crosses sequenceSig from above and on the day before the sequenceInd is exactly equal to sequenceSig. Furthermore sequenceInd contains NAs at the beginning.
intersection(sequenceInd=c(NA,NA,7:1), sequenceSig=c(1:10),plot=TRUE)
# Example 4:
intersection(sequenceInd=c(1,2,4,4,4,7,6,6,5), sequenceSig=c(6,5,4,4,4,4,3,2,2),plot=TRUE)
}

\keyword{ ts }