leftfront <-
function(hitplus=NA, hitminus=NA, conf.level=0.9)
{
    if (hitplus=="NA" | hitminus=="NA"){leftfront<-NA}
    else{
	    leftfront<-binom.test(x=hitplus, n=hitplus+hitminus, alternative="greater", conf.level=conf.level)$conf.int[1]
    	}
    return(leftfront)
}

