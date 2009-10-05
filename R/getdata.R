getdata <-
function(symbol=FALSE, period=c(startdate=FALSE, enddate=FALSE))
{
    url_teil1<-"http://ichart.yahoo.com/table.csv?s="
    url_teil2<-"&g=d&ignore=.csv"
    urlLink<-sub("$", symbol, url_teil1)
    urlLink<-sub("$", url_teil2, urlLink)
    testLink<-try(read.csv(urlLink))
    if(substring(testLink[1],1,5)=="Error"){stop("No internet connection or wrong symbol. Symbol must be like: AACC, MAN.DE, ALV.DE. For more information type ?getdata.")}
    datamatrix<-read.csv(urlLink)
    date_iso<-strptime(datamatrix$Date, format="%Y-%m-%d")
    if(period[1]==FALSE & period[2]==FALSE)
    {
    	datamatrix<-datamatrix[length(datamatrix[, 1]):1, ]
	return(datamatrix)
    }
    if(period[1]==FALSE)
    {
      date_end<-strptime(period[2], "%Y-%m-%d")
      date_start<-strptime("0001-01-01", "%Y-%m-%d")
    }
    if(period[2]==FALSE)
    {
      date_end<-strptime("9999-12-31", "%Y-%m-%d")
      date_start<-strptime(period[1], "%Y-%m-%d")
    }
    if(period[1]!=FALSE & period[2]!=FALSE)
    {
      date_end<-strptime(period[2], "%Y-%m-%d")
      date_start<-strptime(period[1], "%Y-%m-%d")
    }
    index_date<-which(date_start<=date_iso & date_end>=date_iso )
    if (length(index_date)!=0)
    {
	datamatrix<-datamatrix[index_date, ]
    }else{stop("No data for this period avaible")}
    datamatrix<-datamatrix[length(datamatrix[, 1]):1, ]
    return(datamatrix)
}

