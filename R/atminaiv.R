atminaiv <-
function(symbol=FALSE, trade=FALSE, datamatrix=list(NA, NA), period=c(FALSE, FALSE, FALSE), nAhead=1, nFit=FALSE, startcap=1000, sellfront=0, transcost=FALSE, interest=FALSE, ticks=1, plot=TRUE)
{
    if(trade==TRUE){period=c(FALSE, FALSE, FALSE);interest=FALSE;plot=FALSE}
  if(length(period)==1){period<-c(period, FALSE, FALSE)}
  if(length(period)==2){period<-c(period[1], period[2], FALSE)}
  if(!is.numeric(startcap) | startcap<0){stop("The initial assets startcap has to be numeric and positive.")}
  datevec<-datamatrix[[1]]
  datavec<-datamatrix[[2]]
  datavec<-datavec[!is.na(datevec)]
  datevec<-datevec[!is.na(datevec)]
  datevec<-datevec[!is.na(datavec)]
  datavec<-datavec[!is.na(datavec)]
  if(length(datevec)==0 & length(datavec)==0 & symbol==FALSE){stop("Please enter a symbol or a datamatrix. Type ?naiv.strateg for more information.")}
  date_start<-strptime(period[1], "%Y-%m-%d")
  date_end<-strptime(period[2], "%Y-%m-%d")
  startpred<-strptime(period[3], "%Y-%m-%d")
  if(symbol==FALSE)
  {
    date_iso<-strptime(datevec, format="%Y-%m-%d")
    if(period[1]==FALSE & period[2]!=FALSE)
    {
      if(length(datavec[date_iso<date_end])>0)
      {
	datavec<-datavec[1:length(datavec[date_iso<=date_end])]
	datevec<-datevec[1:length(datavec[date_iso<=date_end])]
      }else{stop("No data for this period avaible")}
    }
    if(period[1]!=FALSE & period[2]==FALSE & trade==FALSE)
    {
      if(length(date_iso>date_start)>0)
      {
	datavec<-datavec[1:length(datavec[date_iso>date_start])]
	datevec<-datevec[1:length(datavec[date_iso>date_start])]
      }else{stop("No data for this period avaible")}
    }
    if(period[1]!=FALSE & period[2]!=FALSE & trade==FALSE)
    {
    index_date<-which(date_start<=date_iso & date_end>=date_iso )
    if (length(index_date)!=0)
    {
        datavec<-datavec[index_date]
        datevec<-datevec[index_date]
    }else{stop("No data for this period avaible")}
    }
  }
  if(symbol!=FALSE)
  {
      if(period[1]==FALSE){period[1]<-as.vector("0001-01-01")}
      if(period[2]==FALSE){period[2]<-as.vector("9999-12-31")}
      datamatrix<-getdata(symbol, c(period[1], period[2]))
      datevec<-datamatrix$Date
      datavec<-datamatrix$Adj.Close
  }
  if(ticks>1)
  {
      index<-seq(1, length(datavec), ticks+1)
      index<-(length(datavec)+1)-index
      datavec<-rev(datavec[index])
      datevec<-rev(datevec[index])
  }
  if(period[3]!=FALSE & trade==FALSE)
  {
    date_iso<-strptime(datevec, format="%Y-%m-%d")
    leftside<-length(datevec[date_iso<startpred])
    if(length(as.numeric(date_iso))<3){stop("No data for this period avaible")}
    if(date_iso[1]>startpred | startpred>date_iso[length(as.numeric(date_iso))]){stop(paste("The start of prediction has to be between ", date_iso[1], " and ", date_iso[length(as.numeric(date_iso))], sep=""))}
  }else
  {
    leftside<-0
  }
  if(length(datavec)<4){stop("No data for this period avaible")}
  if(period[3]!=FALSE & nFit!=FALSE & nFit>leftside){stop("Not enough data availible for this nFit settings")}
  startcap.ini<-startcap
  pred.rend<-NA
  inter.eff<-NA
  buy<-NA
  sell<-NA
  bos<-1
  i<-1
  if(nFit==FALSE)
  {
    minus<-if(leftside>4){leftside+1}else{4}
    left<-1
  }else
  {
    if(nFit<3){stop("nFit has to be at least 3 or FALSE")}
    minus<-if((nFit+1)>leftside){nFit+1}else{leftside+1}
  }
  if(interest!=FALSE)
  {
    startcap<-startcap*(1+interest)^(3/360)
    inter.eff<-0
    inter.eff<-inter.eff+startcap*(1+interest)^(3/360)-startcap
  }
  h<-0
    if(trade==TRUE){h<-(length(datavec)-minus)}
  for(j in h:(length(datavec)-minus))
  {
  if(nFit==FALSE){if((3+j)>(leftside+j)){right<-(3+j)}else{right<-(leftside+j)}}else
  {
    left<-(leftside-nFit+1+j)
    if((nFit+j)>(leftside+j)){right<-(nFit+j)}else{right<-(leftside+j)}

  }
    status<-paste("done: ", round((j+1)/(length(datavec)-minus+1)*100, 2), "%", sep="")
    if(startcap<=(transcost*2)){break}
    if(trade==TRUE){right<-length(datavec)}else{print(status)}
    pPrices<-naivpredict(datamatrix=list(datevec, datavec[left:right]), nAhead=nAhead, plot=FALSE, stats=FALSE)$pred
    hRend<-(pPrices[length(pPrices)]/datavec[right])-1
    pred.rend[right+1]<-hRend

    if(bos==1 & interest!=FALSE & trade==FALSE)
    {
      startcap<-startcap*(1+interest)^(1/360)
      inter.eff<-inter.eff+startcap*(1+interest)^(1/360)-startcap
    }
    costQ<-(transcost*2)/startcap
    if(hRend>costQ & bos==1 & trade==FALSE)
    {
      buy[i]<-right
      i<-i+1
      bos<-0
      buy<-buy[!is.na(buy)]
      startcap<-startcap-(2*transcost)
    }
    if(hRend<sellfront & bos==0 & trade==FALSE)
    {
      sell[i]<-right
      i<-i+1
      bos<-1
      sell<-sell[!is.na(sell)]
      startcap<-startcap*(datavec[sell[length(sell)]]/datavec[buy[length(buy)]])
    }
    if(sellfront>costQ & trade==TRUE){stop(paste("sellfront must be < as ",costQ))}
    if(hRend>costQ & trade==TRUE){return(paste("BUY / predicted performance:",round(100*hRend,4),"%"))}
    if(hRend<sellfront & trade==TRUE){return(paste("SELL / predicted performance:",round(100*hRend,4),"%"))}
    if(hRend>sellfront & hRend<costQ & trade==TRUE){return(paste("HOLD / predicted performance:",round(100*hRend,4),"%"))}
    {

    }
  }
  if(bos==1 & interest!=FALSE)
  {
    startcap<-startcap*(1+interest)^(1/360)
    inter.eff<-inter.eff+startcap*(1+interest)^(1/360)-startcap
  }
  if(length(buy)>length(sell))
  {
    sell<-c(sell, length(datavec))
    startcap<-startcap*(datavec[sell[length(sell)]]/datavec[buy[length(buy)]])
  }
  print(paste("Final Assets: ", round(startcap, 2), ", Performance: ", round((startcap/startcap.ini-1)*100, 2), "%", sep=""))
  if(plot==TRUE)
  {
    par(mfrow=c(1, 1), mar=.1+c(2, 1.7, 1, 1), xpd=TRUE, font=11, lwd=0.7, cex.axis=0.7, cex.lab=0.7)
    if(period[3]!=FALSE & leftside>250){gio<-leftside-250}else{gio<-1}
    plot.ts(datavec, xlab="TRADING DAYS", main="", ylab="STOCK PRICE", bty="l", tcl=-0.3, mgp=c(1, 0.3, 0), frame.plot=FALSE, xlim=c(gio, length(datavec)), ylim=c(0, max(datavec[gio:length(datavec)])))
    usr <- par("usr")
    rect(gio, usr[3], leftside, usr[4], border=NA, col="lemonchiffon")
    rect(leftside, usr[3], usr[2], usr[4], border=NA, col="lavender")
    lines(datavec)
    if(length(buy[!is.na(buy)])>0){points(buy, datavec[buy], col="green3", cex=0.75, pch=16)}
    if(length(sell[!is.na(sell)])>0){points(sell, datavec[sell], col="red", cex=0.75, pch=16)}
    panel.first= grid(nx=NULL, ny=NULL, lwd=0.3, col="black")
    if(period[3]==FALSE){period[3]<-""}
    description<-paste(symbol, " Periode: ", datevec[1], " / ", period[3], " / ", datevec[length(datevec)], sep="")
    legend("bottomleft", description, bty="n", cex=0.85)
  }
  invisible(list(buy=buy, sell=sell, fin.cap=startcap))
}

