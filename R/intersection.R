intersection <-
function(sequenceInd=NA, sequenceSig=NA, hLine=NA, plot=TRUE)
{
    if(length(sequenceInd[!is.na(sequenceInd)])>0 & length(hLine[!is.na(hLine)])>0 & length(sequenceSig[!is.na(sequenceSig)])>0)
    {stop("Please, use only the following combinations to calculate intersections: sequenceInd and sequenceSig or sequenceInd and hLine.")}
    numEqualUp<-0;numEqualDown<-0;numEqualOther<-0
    if(length(sequenceInd[!is.na(sequenceInd)])>0 & length(hLine)>0 | length(sequenceSig[!is.na(sequenceSig)])>0 & length(hLine)>0 | length(sequenceInd[!is.na(sequenceInd)])>0 & length(sequenceSig[!is.na(sequenceSig)])>0)
    {
	if(length(sequenceInd[is.na(sequenceInd)])>length(sequenceSig[is.na(sequenceSig) ])){lengthNA<-length(sequenceInd[is.na(sequenceInd)])}
	if(length(sequenceInd[is.na(sequenceInd)])<length(sequenceSig[is.na(sequenceSig) ])){lengthNA<-length(sequenceSig[is.na(sequenceSig)])}
	if(length(sequenceInd[is.na(sequenceInd)])==0 & length(sequenceSig[is.na(sequenceSig)])==0){lengthNA<-0}
	if(length(sequenceInd[is.na(sequenceInd)])==length(sequenceSig[is.na(sequenceSig) ])){lengthNA<-length(sequenceSig[is.na(sequenceSig)])}
	if(length(hLine[!is.na(hLine)])>0){sequenceSig[1:length(sequenceInd)]<-hLine}
	i<-(2+lengthNA);t<-1
	if(any(is.na(sequenceInd[i:length(sequenceInd)])) | any(is.na(sequenceSig[i:length(sequenceSig)])))
	{stop("NA's are only at the begining of sequences allowed.")}
	downIntersection<-c(NA);upIntersection<-c(NA)
	while(i<=length(sequenceInd) & i<=length(sequenceSig))
	{
	    if ((sequenceInd[i-1]<sequenceSig[i-1]) & (sequenceInd[i]>sequenceSig[i]))
	    {
		upIntersection[t]<-i
		t<-t+1
	    }
	    if ((sequenceInd[i-1]>sequenceSig[i-1]) & (sequenceInd[i]<sequenceSig[i]))
	    {
		downIntersection[t]<-i
		t<-t+1
	    }
	    zero<-1
	    if(sequenceInd[i-zero]==sequenceSig[i-zero])
	    {
		while((i-zero)>lengthNA)
		{
		    if(sequenceInd[i-zero]<sequenceSig[i-zero] & sequenceInd[i]>sequenceSig[i])
		    {
			upIntersection[t]<-i
			numEqualUp<-numEqualUp+1
			t<-t+1
			break
		    }
		    if(sequenceInd[i-zero]>sequenceSig[i-zero] & sequenceInd[i]<sequenceSig[i])
		    {
			downIntersection[t]<-i
			numEqualDown<-numEqualDown+1
			t<-t+1
			break
		    }
		    if(zero>1 & sequenceInd[i-zero]!=sequenceSig[i-zero]){numEqualOther<- numEqualOther+1; break}
		    zero<-zero+1
		}
	    }
	    i<-i+1
	}
	if(max(sequenceInd, na.rm=TRUE)>max(sequenceSig, na.rm=TRUE)){maxSeq<-max(sequenceInd, na.rm=TRUE)}else{maxSeq<-max(sequenceSig, na.rm=TRUE)}
	if(min(sequenceInd, na.rm=TRUE)<min(sequenceSig, na.rm=TRUE)){minSeq<-min(sequenceInd, na.rm=TRUE)}else{minSeq<-min(sequenceSig, na.rm=TRUE)}
	if(plot==TRUE)
	{
	  plot(sequenceInd, ylim=c(minSeq, maxSeq), ylab="", main="Intersection points", col="blue", pch=19, cex=0.7)
	  if(is.na(hLine)){points(1:length(sequenceSig), sequenceSig, col="brown", pch=19, cex=0.5)}else{abline(h=hLine, col="brown", pch=19, cex=0.5)}
	  legend("bottomleft", c("sequenceInd", if(is.na(hLine)){"sequenceSig"}else{"hLine"}, "upIntersection", "downIntersection"), col=c("blue", "brown", "green", "red"), lty=c(-1, if(is.na(hLine)){-1}else{1}, -1, -1), pch=c(19, if(is.na(hLine)){19}else{-1}, 19, 19), lwd=0.7, bty="n", cex=0.7)
	  if(length(upIntersection[!is.na(upIntersection)]>0)){upIntersection<- upIntersection[!is.na(upIntersection)]}
	  if(length(downIntersection[!is.na(downIntersection)]>0)){downIntersection<- downIntersection[!is.na(downIntersection)]}
	  if(length(upIntersection[!is.na(upIntersection)])>0){points(upIntersection, sequenceInd[upIntersection], col="green", pch=19, cex=0.7)}
	  if(length(downIntersection[!is.na(downIntersection)])>0){points(downIntersection, sequenceInd[downIntersection], col="red", pch=19, cex=0.7)}
	}
    }else{upIntersection<-NA; downIntersection<-NA}
    return(list(upIntersection=upIntersection, downIntersection=downIntersection, numEqualUp=numEqualUp, numEqualDown=numEqualDown))
}

