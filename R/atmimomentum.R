atmimomentum <-
function(symbol=FALSE, datamatrix=list(NA, NA), period=c(FALSE, FALSE), n=10, sma=10, rule=1, ticks=1, plot=TRUE, stopl=FALSE, interest=FALSE, transcost=FALSE, startcap=1000, standard=TRUE, conf.level=0.95)
{
    if(length(period)==1){period<-c(period, FALSE)}
    name_1<-"Momentum"
    datevec<-datamatrix[[1]]
    datavec<-datamatrix[[2]]
    datavec<-datavec[!is.na(datevec)]
    datevec<-datevec[!is.na(datevec)]
    datevec<-datevec[!is.na(datavec)]
    datavec<-datavec[!is.na(datavec)]
    if(length(datevec)==0 & length(datavec)==0 & is.na(symbol)){stop("Please enter a symbol or a datamatrix. Type ?atmimacd for more information.")}
    if(symbol==FALSE & period[1]!=FALSE & period[2]!=FALSE)
    {
      date_iso<-strptime(datevec, format="%Y-%m-%d")
      date_start<-strptime(period[1], "%Y-%m-%d")
      date_end<-strptime(period[2], "%Y-%m-%d")
      index_date<-which(date_start<=date_iso & date_end>=date_iso )
      if (length(index_date)!=0)
      {
	  datavec<-datavec[index_date]
	  datevec<-datevec[index_date]
      }else{stop("No data for this period avaible")}
    }
    if(symbol!=FALSE)
    {
    	datamatrix<-getdata(symbol, period)
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
    if (length(datavec)>(n+1))
    {
        momentum_q<-momentum(datavec, n=n)
	sma_momentum<-SMA(momentum_q, sma)
	if (rule==1){tradePoints<-intersection(sequenceInd=round(momentum_q, 100), sequenceSig=NA, hLine=0, plot=FALSE)}
	if (rule==2){tradePoints<-intersection(sequenceInd=round(momentum_q, 100), sequenceSig=round(sma_momentum, 100), hLine=NA, plot=FALSE)}
	buy<-tradePoints$upIntersection
	sell<-tradePoints$downIntersection
	numEqual<-tradePoints$numEqualUp+tradePoints$numEqualDown
	numEqualOther<-tradePoints$numEqualOther
        buy_o<-buy
        sell_o<-sell
	if(stopl[1]=="s" | stopl[1]=="d")
	{
	    sell<-stoploss(datavec=datavec, buy=buy, sell=sell, stopl=stopl)
	    sellDraw<-sell
	    l<-1
	    while (l<=length(sellDraw))
	    {
		r<-1
		while (r<=length(sell_o))
		{
		    if(sellDraw[l]==sell_o[r] & !is.na(sellDraw[l]) & !is.na(sell_o[r]))
		    {
			sellDraw[l]<-NA
			r<-length(sell_o)
		    }
		    r<-r+1
		}
		l=l+1
	    }
	    sellDraw<-sellDraw[!is.na(sellDraw)]
	}
	if(standard==FALSE)
	{
	werte_s<-superfluous_filter(datavec=datavec, buy=buy, sell=sell)
	buy<-werte_s$buy
	sellAll<-werte_s$sell
	}else{sellAll<-sell}
    }else
    {
	buy<-NA
	sell<-NA
	sellAll<-NA
	numEqual<-NA
	numEqualOther<-NA
	stop(paste("Not enough data availible. The period with this settings should be longer then ", (n+1)*ticks, " days", sep=""))
    }
    if (plot==TRUE)
    {
    	draw(datavec=datavec, datevec=datevec, symbolName=symbol, name_1=name_1, buy=buy, sell=sellDraw, buy_o=buy_o, stopl=stopl, sell_o=sell_o, n=n, sma=sma, momentum_q=momentum_q, sma_momentum=sma_momentum, rule=rule, sellAll=sellAll, standard=standard)
    }
    signale<-matrix(data=NA, nrow=length(datavec), ncol=2)
    colnames(signale)<-c("buy", "sell")
    buy<-buy[!is.na(buy)]
    sellAll<-sellAll[!is.na(sellAll)]
    signale[buy, 1]<-buy
    signale[sellAll, 2]<-sellAll
    out<-output(symbol=symbol, period=period, datavec=datavec, datevec=datevec, name_1=name_1, n=n, sma=sma, rule=rule, ticks=ticks, standard=standard, stopl=stopl, interest=interest, transcost=transcost, startcap=startcap, signale=signale, conf.level=conf.level)
    invisible(list(signale=signale, numEqual=numEqual, settings=out$settings, result=out$result))
}

