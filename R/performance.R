performance <-
function(datavec=NA, datevec=NA, buy=NA, sell=NA, startcap=1000, interest=0.025, transcost=5.9)
{
    if(any(!is.na(datevec)))
    {
      datavec<-datavec[!is.na(datevec)]
      datevec<-datevec[!is.na(datevec)]
      datevec<-datevec[!is.na(datavec)]
      datavec<-datavec[!is.na(datavec)]
    }else{datavec<-datavec[!is.na(datavec)]}
    if(length(datevec)==0 & interest!=FALSE){stop("To use the interest, there has to be a date vector.")}
    if(length(datavec[!is.na(datavec)])>1)
    {
	if(transcost!=FALSE){tCost<-transcost}else{tCost<-0}
	tempCapital<-startcap-2*tCost
	tempCapital<-(tempCapital/datavec[1])*datavec[length(datavec)]
	if(is.na(datevec[1]))
	{
	    datevec<-1:length(datavec)
	    pufferDate<-datevec[length(datavec)]-datevec[1]
	}else
	{
	    datevec<-as.Date(datevec)
	    pufferDate<-datevec[length(datavec)]-datevec[1]
	    pufferDate<-pufferDate[[1]]
	}
	if(interest!=FALSE)
	{
	    tempCapital<-tCost*((1+interest)^(pufferDate/360)-1)+tempCapital
	}
	bahPerformance<-tempCapital/startcap-1
	if(length(buy[!is.na(buy)])>0)
	{
	    buy<-buy[!is.na(buy)]
	    sell<-sell[!is.na(sell)]
	    if(interest!=FALSE)
	    {
		pufferDate<-c(NA)
		buyAndSell<-sort(c(1, buy, sell, length(datavec)))
		if(is.na(datevec[1]))
		{
		    pufferDate<-as.vector(datevec[buyAndSell[2:length(buyAndSell)]]- datevec[buyAndSell[1:(length(buyAndSell)-1)]])
		    pufferDate[1]<-pufferDate[1]+1
		}else
		{
		   for(i in 2:length(buyAndSell)){pufferDate[i-1]<-datevec[buyAndSell[i]]- datevec[buyAndSell[i-1]]}
		   pufferDate[1]<-pufferDate[1]+1
		}
		tempCapital<-startcap*(1+interest)^(pufferDate[1]/360)
	    }else
	    {
		 tempCapital<-startcap
	    }
	    i<-1
	    s<-2
	    noMoney<-c(NA)
	    returns<-c(NA)
	    while(i<=length(buy))
	    {
		if(tempCapital<=tCost*2)
		{
		    noMoney<-i
		    break
		}
		tempCapital<-tempCapital-2*tCost
		returns[i]<-(datavec[sell[i]]/datavec[buy[i]])-1
		tempCapital<-(tempCapital/datavec[buy[i]])*datavec[sell[i]]
		if(interest!=FALSE)
		{
		    tempCapital<-tCost*((1+interest)^(pufferDate[s]/360)-1)+tempCapital
		    if(i==length(buy)){break}else{tempCapital<-tempCapital*(1+interest)^( pufferDate[s+1]/360)}
		}
	    i<-i+1
	    s<-s+2
	    }
	    if(interest!=FALSE)
	    {
		if(!is.na(noMoney))
		{
		    tempCapital<-tempCapital*(1+interest)^(sum(pufferDate[(i+1):length (pufferDate)])/360)
		}else
		{
		    tempCapital<-tempCapital*(1+interest)^(pufferDate[length(pufferDate)] /360)
		}
	    }
	strategyPerformance<-(tempCapital/startcap)-1
	}else
	{
	    if(is.na(datevec[1]))
	    {
		datevec<-1:length(datavec)
		pufferDate<-datevec[length(datavec)]-datevec[1]
	    }else
	    {
		datevec<-as.Date(datevec)
		pufferDate<-datevec[length(datavec)]-datevec[1];pufferDate<-pufferDate[[1]]
	    }
	    if(interest!=FALSE)
	    {
		tempCapital<-startcap*(1+interest)^(pufferDate/360)
		strategyPerformance<-tempCapital/startcap-1
		returns<-c(NA)
	    }else{strategyPerformance<-0;returns<-c(NA)}
	}
    }else
    {
	strategyPerformance<-c(NA)
	bahPerformance<-c(NA)
	returns<-c(NA)
    }
    invisible(list("return.strategy"=strategyPerformance, "returns"=returns, "return.buy.and.hold"=bahPerformance))
}

