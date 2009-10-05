output <-
function(symbol, period, datavec, datevec, name_1, nslow=NA, nfast=NA, trigger=NA, rule=NA, n=NA, sma=NA, top=NA, bottom=NA, ticks, standard, stopl, interest, transcost, startcap, signale, conf.level)
{
  header<-data.frame(NAME=NA, VALUE=NA)
  settings<-data.frame(NAME=NA, VALUE=NA)
  result<-data.frame(NAME=NA, VALUE=NA)

  header[1, 1]<-"symbol";header[1, 2]<-symbol
  if(period[1]==FALSE | period[2]==FALSE)
  {
    period[1]<-as.vector(datevec[1])
    period[2]<-as.vector(datevec[length(datevec)])
  }
  header[2, 1]<-"period";header[2, 2]<-paste(period[1], " - ", period[2], ", trading days:", length(datavec), sep="")
  header[3, ]<-""

  if(name_1=="MACD")
  {settings[1, 1]<-paste(name_1, ", rule: ", rule, "   ", sep="");settings[1, 2]<-paste("nslow=", nslow, ", nfast=", nfast, ", trigger=", trigger)}
  if(name_1=="Momentum")
  {
    if(rule==1){settings[1, 1]<-paste(name_1, ", rule: ", rule, "   ", sep="");settings[1, 2]<-paste("n=", n, sep="")}
    if(rule==2){settings[1, 1]<-paste(name_1, ", rule: ", rule, "   ", sep="");settings[1, 2]<-paste("n=", n, " sma=", sma, sep="")}
  }
  if(name_1=="RSI"){settings[1, 1]<-name_1;settings[1, 2]<-paste("n=", n, ", top=", top, ", bottom=", bottom, sep="")}

  settings[2, 1]<-"ticks";settings[2, 2]<-ticks
  settings[3, 1]<-"standard";settings[3, 2]<-standard
  if(stopl[1]==FALSE){nam<-"no"}
  if(stopl[1]=="s"){nam<-paste("static, ", stopl[2], sep="")}
  if(stopl[1]=="d"){nam<-paste("dynamic, ", stopl[2], sep="")}
  settings[4, 1]<-"stoploss";settings[4, 2]<-nam
  if(interest==FALSE){nam<-"no"}
  if(interest!=FALSE){nam<-interest}
  settings[5, 1]<-"interest";settings[5, 2]<-nam
  if(transcost==FALSE){nam<-"no"}
  if(transcost!=FALSE){nam<-transcost}
  settings[6, 1]<-"transcost";settings[6, 2]<-nam
  settings[7, ]<-""
  print.data.frame(header, row.names=FALSE, right=FALSE)
  print.data.frame(settings, row.names=FALSE, right=FALSE)
  if(standard==FALSE)
  {
    perf<-performance(datavec=datavec, datevec=datevec, startcap=startcap, buy=signale[, 1], sell=signale[, 2], interest=interest, transcost=transcost)
    result[1, 1]<-"performace";result[1, 2]<-round(perf$return.strategy, 6)
    result[2, 1]<-"num. signals";result[2, 2]<-length(na.omit(perf$returns))
    result[3, 1]<-"hits";result[3, 2]<-length(na.omit(perf$returns[perf$returns>0]))
    result[4, 1]<-"leftfront";result[4, 2]<-if(length(na.omit(perf$returns))>0)
    {paste(round(leftfront(result[3, 2], result[2, 2]-result[3, 2], conf.level), 6), " conf.level: ", conf.level, sep="")}else
    {"NA"}
    result[5, 1]<-"performance BAH ";result[5, 2]<-round(perf$return.buy.and.hold, 6)
    print.data.frame(result, row.names=FALSE, right=FALSE)
  }else{print("results are not availible for standard=TRUE")}
  invisible(list(settings=settings, result=result))
}

