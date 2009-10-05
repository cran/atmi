getsymbols <-
function(indexsymbol="GDAXI")
{
  link<-paste("http://de.old.finance.yahoo.com/d/quotes.csv?s=@%5E", indexsymbol, "&f=sl1d1t1c1ohgv&e=.csv", sep="")
  testLink<-try(read.csv(link))
  if(substring(testLink[1],1,5)=="Error"){stop("No internet connection or wrong indexsymbol. Indexsymbol must be like: GDAXI, TEXDAX, NDX. For more information type ?getsymbols.")}
  symbList<-read.csv(link, sep=";", header=FALSE)
  symbList<-symbList[, 1]
  return(symbList)
}

