###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:
#
#Sourced into streamNetwork.r file to do actual routing
#
makeHydrographs <- function(flowData, gauges, precip=NULL, spack=NULL, saveGraphs=F, plotTogether=F, interact=T, plotStats=T, plotSnowpack=F, plotPrecip=F, plotSeason=NULL, plotAnnual=F){


    if(length(gauges) >= 1){

	if(plotTogether){
	    par(mfrow=c(length(gauges), 1))
	} else {
	    #par(.pardefault)
	}
	for(i in 1:length(gauges)){
	    if(saveGraphs){
		png(paste(plotDir, "/",gsub(" ", "_", gsub("[[:punct:]]", "", gaugesInBounds@data[1, "SITENAME"])), ".png", sep=""))
	    }

	    if(timeStep == "month"){
		dates <- as.Date(as.yearmon(rownames(flowData$qOut)))
	    } else {
		dates <- as.Date(rownames(flowData$qOut))
	    }
            if(!is.null(plotSeason)){

		print("Plotting Seasonal")
		flow1 <- sumSeasonal(data.frame(flowData$qOut[, names(gauges)[i]]))
		flow1 <- flow1[seq(plotSeason, to=length(flow1), by=4)]
		gaugeFlow <- sumSeasonal(data.frame(gauges[[i]][,2], row.names=gauges[[i]][,1]))
		gaugeFlow <- gaugeFlow[seq(plotSeason, to=length(gaugeFlow), by=4)]

		print("Plotting Seasonal")
                plot(seq(as.Date(simStartDate), length.out=length(flow1), by="year"), flow1, type="l", col="red", xlab="",  ylab="Flow (m/s)", ylim=c(0, max(c(max(flow1, na.rm=T), max(gaugeFlow, na.rm=T)))))
		print("Plotting Seasonal")
                lines(seq(as.Date(simStartDate), length.out=length(flow1), by="year"), gaugeFlow)
		print("Plotting Seasonal")
		#gofs <- CalcGOFs(flow1, gaugeFlow)
		#legend("topright", legend=paste(rownames(gofs), gofs), ncol=2, cex=.7)
		print("Plotting Seasonal")

            } else if(plotAnnual){

		print("Plotting Annual")
		flow1 <- data.frame(flowData$qOut[, names(gauges)[i]])
		flow1 <- aggregate(flow1, list(substring(rownames(flow1), 4)), FUN=sum)
		gaugeFlow <- data.frame(gauges[[i]][,2], row.names=gauges[[i]][,1])
		gaugeFlow <- aggregate(gaugeFlow, list(substring(rownames(gaugeFlow), 4)), FUN=sum)
		print("got here")
		print(dim(flow1))
                plot(flow1, type="l", col="red", xlab="",  ylab="Flow (m/s)", ylim=c(0, max(flow1[,2])*1.05))
		print("got here")
                lines(gaugeFlow)
		print("got here")

	    } else {    
                plot(dates[1:12], flowData$qOut[, names(gauges)[i]][1:12], type="l", col="red", xlab="",  ylab="Flow (m/s)")
                lines(as.Date(gauges[[i]][,1]), gauges[[i]][,2])
            }
	    if(plotSnowpack){
		par(new=T)
		plot(dates, snowpack, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
	    }
	    if(plotPrecip){
		par(new=T)
		plot(dates, precip, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
	    }

	    abline(0, 0)
	    if(!is.null(plotSeason)){
		title(paste(c("MAM", "JJA", "SON", "DJF")[plotSeason], "for",  gaugesInBounds@data[gaugesInBounds@data[,8] == as.numeric(names(gauges[i])),"SITENAME"][1]))
	    #legend("topleft", col=c("red", "black"), legend=c("Simulation", "Gauge"), lty=1)
	    } else {
		title(gaugesInBounds@data[gaugesInBounds@data[,8] == as.numeric(names(gauges[i])),"SITENAME"][1])
	    legend("topleft", col=c("red", "black"), legend=c("Routed LPJ-Guess Runoff", "Gauge Data"), lty=1)
	    }

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
			dates <- as.Date(as.yearmon(rownames(flowData$qOut)))
		    } else {
			dates <- as.Date(rownames(flowData$qOut))
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

plotHydroVar <- function(flowData, gauges, hydroVar, saveGraphs=F, plotTogether=F, interact=T){


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
		dates <- as.Date(as.yearmon(rownames(flowData$qOut)))
	    } else {
		dates <- as.Date(rownames(flowData$qOut))
	    }
                plot(dates, flowData[[hydroVar]][, names(gauges)[i]], type="l", col="red", xlab="",  ylab=hydroVar)

	    abline(0, 0)
	    title(gaugesInBounds@data[gaugesInBounds@data[,8] == as.numeric(names(gauges[i])),"SITENAME"])
	    legend("topleft", col=c("red", "black"), legend=c("Routed LPJ-Guess Runoff", "Gauge Data"), lty=1)

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
			dates <- as.Date(as.yearmon(rownames(flowData$qOut)))
		    } else {
			dates <- as.Date(rownames(flowData$qOut))
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

sumSeasonal <- function(dat){
    i <- 1
    sumdData <- c()
        while(i <= (nrow(dat)-2)){
	    if(as.numeric(format(as.yearmon(rownames(dat)[i]), "%m")) %in% c(3,6,9,12)){
		sumdData <- c(sumdData, sum(dat[i:(i+2),1]))
		i <- i + 3
	    } else {
		i <- i + 1
	    }
    }
    return(sumdData)
}

