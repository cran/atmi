draw <-
function(datavec=datavec, datevec=datevec, symbolName=symbolName, name_1=name_1, buy=buy, buy_o=buy_o, sell=sell, stopl=stopl, sell_o=sell_o, standard=standard, sellAll=sellAll, macd=NA, trigger=NA, nfast=NA, nslow=NA, n=NA, sma=NA, momentum_q=NA, sma_momentum, rsi_q=NA, top, bottom, rule=NA)
{
    buyColor<-"green3";sellColor<-"red";stoplColor<-"red";helpLinesColor="gold"
    buyCex<-0.75;sellCex=0.75;stoplCex<-0.75
    signalPch<-16;stoplPch<-4
    par(mfrow=c(2, 1), mar=.1+c(2, 1.7, 1, 1), xpd=TRUE, font=11)
    if(name_1=="MACD")
    {
	indicator<-macd[, 1]
	signals<-parse(text="if(length(buy_o[!is.na(buy_o)])>0){points(buy_o, macd[buy_o, 1], col=buyColor, pch=signalPch, cex=buyCex)}
			     if(length(sell_o[!is.na(sell_o)])>0){points(sell_o, macd[sell_o, 1], col=sellColor, pch=signalPch, cex=sellCex)}")
	name<-paste("MACD (", nfast, ", ", nslow, ")", sep="")
	helpLines<-parse(text="lines(macd[, 2], col=helpLinesColor); abline(h=0)")
	sigVal<-paste("Signal (", trigger, ")", sep="")
	legendTop<-parse(text="legend(\"bottomleft\", c(name, sigVal), lty=c(1, 1), lwd=2, col=c(\"black\", helpLinesColor), bty=\"n\", cex=1)")
    }
    if(name_1=="Momentum")
    {
	indicator<-momentum_q
	if(rule==1)
	{
	    name<-paste("Momentum (", n, ")", sep="")
	    helpLines<-parse(text="abline(h=0)")
	    legendTop<-parse(text="legend(\"bottomleft\", name, bty=\"n\", cex=1)")
	}
	if(rule==2)
	{
	    name<-paste("Momentum (", n, ")", sep="")
	    helpLines<-parse(text="lines(sma_momentum, col=helpLinesColor)")
	    sigVal<-paste("SMA (", sma, ")", sep="")
	    legendTop<-parse(text="legend(\"bottomleft\", c(name, sigVal), lty=c(1, 1), lwd=2, col=c(\"black\", helpLinesColor), bty=\"n\", cex=0.85)")
	}
	signals<-parse(text="if(length(buy_o[!is.na(buy_o)])>0){points(buy_o, momentum_q[buy_o], col=buyColor, pch=signalPch, cex=buyCex)}
			     if(length(sell_o[!is.na(sell_o)])>0){points(sell_o, momentum_q[sell_o], col=sellColor, pch=signalPch, cex=sellCex)}")
    }
    if(name_1=="RSI")
    {
	indicator<-rsi_q
	signals<-parse(text="if(length(buy_o[!is.na(buy_o)])>0){points(buy_o, rsi_q[buy_o], col=buyColor, pch=signalPch, cex=buyCex)}
			     if(length(sell_o[!is.na(sell_o)])>0){points(sell_o, rsi_q[sell_o], col=sellColor, pch=signalPch, cex=sellCex)}")
	name<-paste("RSI (", n, ")", sep="")
	helpLines<-parse(text="abline(h=top);abline(h=bottom)")
	legendTop<-parse(text="legend(\"bottomleft\", name, bty=\"n\", cex=1)")
    }
    plot(indicator, tcl=-0.3, mgp=c(1, 0.3, 0), cex.axis=0.7, xlab="", ylab="", main="", xaxt="n", frame.plot=FALSE, type="l", xlim=c(0, length(datavec)))
    panel.first= grid(nx=NULL, ny=NULL, lwd=1, col="black")
    eval(helpLines);eval(legendTop);eval(signals)
    plot(datavec, tcl=-0.3, mgp=c(1, 0.3, 0), cex.axis=0.7, xlab="TRADING DAYS", ylab="STOCK PRICE", cex.lab=0.7, main="", bty="l", type="l", xlim=c(0, length(datavec)))
    panel.first= grid(nx=NULL, ny=NULL, lwd=1, col="black")
    if(length(buy[!is.na(buy)])>0){points(buy, datavec[buy], col=buyColor, pch=signalPch, cex=buyCex)}
    if(stopl[[1]]=="d" | stopl[[1]]=="s")
    {
	if (standard==TRUE)
	{
	    if(length(sell[!is.na(sell)])>0){points(sell, datavec[sell], col=stoplColor, pch=stoplPch, cex=stoplCex)}
	    if(length(sell_o[!is.na(sell_o)])>0){points(sell_o, datavec[sell_o], col=sellColor, pch=signalPch, cex=sellCex)}
	}
    }
    if(standard==FALSE)
    {
	if(length(sellAll[!is.na(sellAll)])>0){points(sellAll, datavec[sellAll], col=sellColor, pch=signalPch, cex=sellCex)}
    }
    if(standard==TRUE)
    {
	if(length(sell_o[!is.na(sell_o)])>0){points(sell_o, datavec[sell_o], col=sellColor, pch=signalPch, cex=sellCex)}
    }
    datevecVon<-datevec[length(datevec)-length(datavec)+1]
    datevecBis<-datevec[length(datevec)]
    description<-paste(symbolName, " Periode: ", datevecVon, " - ", datevecBis, sep="")
    legend("bottomleft", description, bty="n", cex=0.85)
}

