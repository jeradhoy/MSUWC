###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:
#
#Sourced into streamNetwork.r file to do actual routing
#
makeHydrographs <- function(flowData, gauges, edges=NULL, precip=NULL, spack=NULL, saveGraphs=F, plotTogether=F, interact=T, plotStats=T, plotSnowpack=F, plotPrecip=F){


    if(length(gauges) >= 1){

	if(plotTogether){
	    par(mfrow=c(length(gauges), 1))
	} else {
	    par(.pardefault)
	}
	for(i in 1:length(gauges)){
	    if(saveGraphs){
		png(paste(plotDir, "/",gsub(" ", "_", gsub("[[:punct:]]", "", gaugesInBounds@data[1, "SITENAME"])), ".png", sep=""))
	    }

	    if(timeStep == "month"){
		dates <- as.Date(as.yearmon(rownames(flow$qOut)))
	    } else {

		dates <- as.Date(rownames(flow$qOut))
	    }
	    plot(dates, flowData$qOut[, names(gauges)[i]], type="l", col="red", xlab="",  ylab="Flow (m/s)")
	    lines(as.Date(gauges[[i]][,1]), gauges[[i]][,2])
	    if(plotSnowpack){
		par(new=T)
		plot(dates, snowpack, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
	    }
	    if(plotPrecip){
		par(new=T)
		plot(dates, precip, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
	    }


	    abline(0, 0)
	    title(gaugesInBounds@data[gaugesInBounds@data[,8] == as.numeric(names(gauges[i])),"SITENAME"])
	    legend("topleft", col=c("red", "black"), legend=c("Routed LPJ-Guess Runoff", "Gauge Data"), lty=1)

	    if(plotStats){
		gofs <- CalcGOFs(flowData, gauges[i])
		legend("topright", legend=paste(rownames(gofs), gofs), ncol=2, cex=.7)
	    }

	    if(saveGraphs){
		dev.off()
	    }
	    if(interact){
		if(i < length(gauges)){
		    answer <- readline("Press Enter To Continue")
		    if(answer == ""){
			next
		    } else {
			break
		    }
		}
	    }
		
	}
    } else {
	print("No gauges to plot!")
	print("Enter edgeIDs to plot! If plotting multiple, seperate by spaces.")
	edgeIdList <- readline()
	if(edgeIdList != ""){
	    edgeIdList <- strsplit(edgeIdList, " ")[[1]]
	    if(plotTogether){
		par(mfrow=c(length(edgeIdList), 1))
	    } else {
		par(.pardefault)
	    }
	    for(i in 1:length(edgeIdList)){
		    if(saveGraphs){
			png(paste(plotDir, "/",edgeIdList[i], ".png", sep=""))
		    }

		    if(timeStep == "month"){
			dates <- as.Date(as.yearmon(rownames(flow$qOut)))
		    } else {
			dates <- as.Date(rownames(flow$qOut))
		    }

		    plot(dates, flowData$qOut[, edgeIdList[i]], type="l", col="red", xlab="",  ylab="Flow (m/s)")
		    abline(0, 0)

		    title(paste("Edge", edgeIdList[i]))

		    if(saveGraphs){
			dev.off()
		    }
		    if(interact){
			if(i < length(edgeIdList)){
			    answer <- readline("Press Enter To Continue")
			    if(answer == ""){
				next
			    } else {
				break
			    }
			}
		    }
			
		}
	    }
    }
}

CalcGOFs <- function(flowData, gauges){
    
     
    if(nrow(flowData$qOut) != nrow(gauges[[1]])){
	for(g in 1:length(gauges)){
	    daysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
	    out <- c()
	    for(row in 1:(nrow(flow$qOut)-1)){
		out <- c(out, seq(flow$qOut[row, col], flow$qOut[row+1, col], length.out=daysInMonth[as.numeric(format(as.yearmon(rownames(flow$qOut)[row]), "%m"))]))
	    }
		out <- c(out, seq(flow$qOut[row, col], 0, length.out=daysInMonth[as.numeric(format(as.yearmon(rownames(flow$qOut)[row]), "%m"))]+1))
	}
    }

    d <- c()
    for(i in 1:length(gauges)){
	d <- cbind(d,gof(flowData$qOut[, names(gauges)[i]], gauges[[i]][,2], na.rm=T))
    }
    colnames(d) <- names(gauges)
    return(d)
}


makeTaylorDiagrams <- function(flowData, gauges, saveGraphs=F, interact=F){
	if(saveGraphs){
	    png(paste(plotDir, "/",gsub(" ", "_", gsub("[[:punct:]]", "", gaugesInBounds@data[1, "SITENAME"])), ".png", sep=""))
	}
colors <- rainbow(length(gauges))
	taylor.diagram(flowData$qOut[, names(gauges)[1]], gauges[[1]][,2], col=colors[1], pch=1)
	for(j in 2:length(gauges)){
	    print("plotting point")
	    taylor.diagram(flowData$qOut[, names(gauges)[j]], gauges[[j]][,2], add=T, col=colors[j], pch=length(gauges)%%25)
	}

	#title(gaugesInBounds@data[gaugesInBounds@data[,8] == as.numeric(names(gauges[i])),"SITENAME"])

	if(saveGraphs){
	    dev.off()
	}

}

