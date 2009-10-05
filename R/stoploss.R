stoploss <-
function(datavec, buy, sell, stopl=c("s",0.1))
{
    buyTemp<-c(NA)
    sellTemp<-c(NA)
    noSell<-0
    buy<-sort(buy[!is.na(buy)])
    sell<-sort(sell[!is.na(sell)])
    lengthBuy<-length(buy[!is.na(buy)])
    noData<-0
    spur<-0
    while(lengthBuy!=0)
    {
	buyTemp[length(buyTemp)+spur]<-buy[1]
	if(length(sell[!is.na(sell)])!=0){sellTemp[length(sellTemp)+spur]<-sell[1]}else {noSell<-1}
	if(spur==1)
	{
	    if(noSell==1){go<-sellTemp[length(sellTemp)]}else{go<-sellTemp[length (sellTemp)-1]}
	    while(buy[1]< go & length(buy[!is.na(buy)])!=0){buy<-buy[-1]}
	    if(!is.na(buy[1])){buyTemp[length(buyTemp)]<-buy[1]}else
	    {
	    	buyTemp<-buyTemp[-length(buyTemp)]
	    	break
	    }
	}
	while(sell[1]<buyTemp[length(buyTemp)] & noSell!=1)
	{
	    sell<-sell[-1]
	    if(length(sell[!is.na(sell)])==0)
	    {
		sellTemp<-sellTemp[-length(sellTemp)]
		noSell<-1
	    }
	}
	if(noSell!=1){sellTemp[length(sellTemp)]<-sell[1]}
	spur<-1
	stop<-0
	g<-buyTemp[length(buyTemp)]
	basis<-datavec[g]
	if(noSell!=1){go2<-sellTemp[length(sellTemp)]}else{go2<-length(datavec)}
	while (stop==0 & g<go2)
	{
	    rend<-(datavec[g+1]/basis)-1
	    if (stopl[1]=="d")
	    {
		if (rend>0 & datavec[g+1]>basis){basis<-datavec[g+1]}
	    }
	    if (rend<=-as.numeric(stopl[2]))
	    {
		sellTemp[length(sellTemp)+if(noSell==1){1}else{0}]<-(g+1)
		stop<-1
	    }
	    g<-g+1
	}
	buy<-buy[-1]
	if(stop!=1)
	{
	    if(length(sell[!is.na(sell)])==0){noData<-1}
	    sell<-sell[-1]
	    if(length(sell[!is.na(sell)])==0){noSell<-1}
	}
	if(noData==0){lengthBuy<-length(buy[!is.na(buy)])}else{lengthBuy<-0}
    }
    buy<-buyTemp
    sell<-sellTemp
    return(sell)
}

