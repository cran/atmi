naivpredict <-
function(symbol=FALSE, datamatrix=list(NA, NA), period=c(FALSE, FALSE), nAhead=5, plot=TRUE, stats=TRUE)
{
  datevec<-datamatrix[[1]]
  datavec<-datamatrix[[2]]
  datavec<-datavec[!is.na(datevec)]
  datevec<-datevec[!is.na(datevec)]
  datevec<-datevec[!is.na(datavec)]
  datavec<-datavec[!is.na(datavec)]
  if(!is.na(symbol) & length(datevec)==0 & length(datavec)==0 & is.na(symbol)){stop("Please enter a symbol or a datamatrix. Type ?naivpredict for more information.")}
  if(is.na(symbol) & !is.na(period[1]) & !is.na(period[2]))
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
  daten<-ts(datavec)

  fit<-HoltWinters(daten, gamma=FALSE)
  if(fit$alpha[[1]]<0){fit$alpha[[1]]<-0};if(fit$alpha[[1]]>1){fit$alpha[[1]]<-1}
  if(fit$beta[[1]]<0){fit$beta[[1]]<-0};if(fit$beta[[1]]>1){fit$beta[[1]]<-1}
  pred<-predict(fit, n.ahead=nAhead)

  if(plot==TRUE)
  {
    if(length(daten)>100){gio<-length(daten)-100}else{gio<-1}
    h<-c(daten[gio:length(daten)], pred)
    par(mfrow=c(1, 1), mar=.1+c(2, 1.7, 1, 1), xpd=TRUE, font=11, lwd=0.7, cex.lab=0.7, cex.axis=0.7)
    plot.ts(daten, xlab="TRADING DAYS", main="", ylab="STOCK PRICE", bty="l", tcl=-0.3, mgp=c(1, 0.3, 0), frame.plot=FALSE, xlim=c(gio, (length(daten)+nAhead)), ylim=c(min(h), max(h)))
    usr <- par("usr");rect(usr[1], usr[3], length(daten), usr[4], border=NA, col="lemonchiffon");rect(length(daten), usr[3], usr[2], usr[4], border=NA, col="lavender")
    lines(daten)
    lines(c(length(daten):(length(daten)+nAhead)), c(daten[length(daten)], pred), col="red")
    description<-paste(symbol, " Periode: ", datevec[1], " / ", datevec[length(datevec)], sep="")
    legend("bottomleft", description, bty="n", cex=0.85)
    panel.first= grid(nx=NULL, ny=NULL, lwd=0.3, col="black")
  }

  if(stats==TRUE)
  {
    trend<-fitted(fit)[, 3]
    level<-fitted(fit)[, 2]
    holtModel<-lm(daten[3:length(daten)]~level+trend)
    sums<-summary(holtModel)
    print(sums)
    pValue<-NA
    if(length(residuals(holtModel))<25){nLags<-length(residuals(holtModel))}else{nLags<-25}
    for(i in 1:nLags)
    {
      boxLjung<-Box.test(residuals(holtModel), type="Ljung-Box", lag=i)
      pValue[i]<-boxLjung$p.value
    }
    if(plot==TRUE){x11()}
    par(mfrow=c(4, 1), mar=0.1+c(3.5, 2.5, 2.6, 1), oma=c(0, 0, 0, 0), xpd=TRUE, font=11, tcl=-0.3, mgp=c(1, 0.3, 0), bg="white")
    plot(residuals(holtModel), type="l", main="Residuals", ylab="", xlab="TIME")
    abline(h=0)
    plot.acf<-acf(residuals(holtModel), plot=FALSE)
    plot(plot.acf, main="ACF of Residuals")
    plot.pacf<-pacf(residuals(holtModel), plot=FALSE)
    plot(plot.pacf, main="PACF of Residuals")
    plot(pValue, type="h", ylim=c(0, 1), main="p-values for Ljung-Box statistic", ylab="P-VALUES", xlab="Lag")
    abline(h=0.05, col="blue", lty=2)
  }
  invisible(list(pred=pred))
}

