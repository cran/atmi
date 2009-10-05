superfluous_filter <-
function(datavec, buy, sell)
{
  buy<-sort(buy[!is.na(buy)])
  sell<-sort(sell[!is.na(sell)])
  if(length(buy[!is.na(buy)])>0)
  {
      l<-1
      while (l<=length(sell))
      {
	  r<-1
	  while (r<=length(buy))
	  {
	      if(sell[l]==buy[r] & !is.na(sell[l]) & !is.na(buy[r]))
	      {
		  sell[l]<-NA
		  buy[r]<-NA
		  r<-length(buy)
	      }
	      r<-r+1
	  }
	  l=l+1
      }
      sell<-sell[!is.na(sell)]
      buy<-buy[!is.na(buy)]
      buyTemp<-NA
      sellTemp<-NA
      indBuy<-1
      indSell<-0
      for (sprinter in 1:length(datavec))
      {
	  if(any(buy==sprinter) & length(buy[!is.na(buy)])>0 & indBuy==1){buyTemp[length(buyTemp)+1]<-sprinter; indBuy<-0; indSell<-1}
	  if(any(sell==sprinter) & length(sell[!is.na(sell)])>0 & indSell==1){sellTemp[length(sellTemp)+1]<-sprinter; indBuy<-1; indSell<-0}
      }
      sellAll<-sellTemp[!is.na(sellTemp)]
      buy<-buyTemp[!is.na(buyTemp)]
      if(length(sellAll)==0 & length(buy)>0 | length(sellAll)>0 & length(buy)>0 & sellAll[length(sellAll)]<buy[length(buy)]){sellAll[length(sellAll)+1]<- length(datavec)}
  }else{sellAll<-NA}
  return(list(buy=buy, sell=sellAll))
}

