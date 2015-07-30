###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:
#
#Sourced into streamNetwork.r file to do actual routing
#


# Generates matrix of runoff by catchment and time step
# Builds raster brick and uses "extract" function to accumulate runoff
# writes to text file if specified
AggregateRunoff <- function(ncFile, catchmentPolygons, runoffVar, startDate=NULL, leapDays=F, by="day", fname=NULL){

    brick <- brick(ncFile, varname=runoffVar)
    print("finished building brick")
    runoff <- data.frame(t(extract(brick, catchmentPolygons, na.rm=T, fun=sum)))
    print("finished extracting data")

    if(by == "day"){
      # convert from mm/m2/day to m3/sec
      runoff <- runoff/1000*1000000/(24*60*60)

      if(!is.null(startDate)){
	dates <- seq(as.Date(startDate), by="day", length.out=nrow(runoff))
	if(leapDays == F){
	  dates <- dates[c(-grep("02-29", dates))]
	}
	rownames(runoff) <- dates
      }

    }

    if(by == "month"){

      if(!is.null(startDate)){
	dates <- as.yearmon(seq(as.Date(startDate), by="month", length.out=nrow(runoff)))
	rownames(runoff) <- dates
      }

	# Starts with december so that 12%%12 returns 1
	daysInMonth <- c(31, 31,28,31,30,31,30,31,31,30,31,30)

	for(month in 1:nrow(runoff)){
      	  runoff[month,] <- runoff[month,]/1000*1000000/(24*60*60*daysInMonth[as.integer(format(as.yearmon(rownames(runoff[month,])), "%m"))%%12 + 1])
	}
    }

    colnames(runoff) <- catchmentPolygons$HydroID


    if(!is.null(fname)){
    	write.table(runoff, paste(fname, ".txt", sep=""))
    }
    return(runoff)
}

CorrectEdgeSlopes <- function(edges){
    edges <- edges[order(edges$RiverOrder),]
    edges$SLOPE2 <- edges$SLOPE/120000 
    
    for(i in 1:nrow(edges)){ 
        #if(edges[i,]$RiverOrder == 1){
        #    edges[i,"ContribArea"] <- edges[i,]$Shape_Ar_1
        #} else {
            #edges[i,"SLOPE2"] <- (sum(edges[edges$NextDownID == edges[i,]$DrainID,]$SLOPE)/2 + edges[edges$DrainID == edges[i,]$NextDownID,]$SLOPE)/2
            edges[i,"SLOPE2"] <- (sm(edges[edges$NextDownID == edges[i,]$DrainID,]$SLOPE)/2 + edges[edges$DrainID == edges[i,]$NextDownID,]$SLOPE)/2
        #}
        
    }
    return(edges)
}




AssignContribArea <- function(edges){
    edges <- edges[order(edges$RiverOrder),]
    edges$ContribArea <- NA
    
    for(i in 1:nrow(edges)){
        
        if(edges[i,]$RiverOrder == 1){
            edges[i,"ContribArea"] <- edges[i,]$Shape_Ar_1
        } else {
            edges[i,"ContribArea"] <- sum(edges[edges$NextDownID == edges[i,]$DrainID,]$ContribArea) + edges[i,]$Shape_Ar_1 
        }
        
    }
    return(edges)
}


AssignBfDepth <- function(edges, a, b){

    edges$bfDepth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfDepth"] <- a*(edges[i,]$ContribArea)^b
    }
    return(edges)
}


AssignBfWidth <- function(edges, a, b){

    edges$bfWidth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfWidth"] <- a*(edges[i,]$ContribArea)^b
    }
    return(edges)
}

AssignAcoeff <- function(edges, coeff){

    edges$aCoeff <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"aCoeff"] <- (edges[i,]$Shape_Ar_1)*coeff
    }
    return(edges)
}


GetCatchInBounds <- function(catchments, hucCodes){
    catchmentCodes <- catchments$HUC10
    catchmentCodes[is.na(catchmentCodes)] <- 0
    catchments$HUC10 <- catchmentCodes
    return(catchments[catchments$HUC10 %in% hucCodes,])
}




RouteWater <- function(edges, Rsurf, Rsub, debugMode=F, by="day"){
    
    #Order edges by Shreve order so calculation is in right order
    edges <- edges[order(edges$RiverOrder),]

    timeLength <- nrow(Rsurf) 
    seedMatrix <- matrix(0, nrow=timeLength, ncol=ncol(Rsurf),
        dimnames=list(rownames(Rsurf), edges$DrainID))

    manningN <- .05

    results <- list(
    	qIn = seedMatrix,
    	qOut = seedMatrix,
    	sRiv = seedMatrix,
    	sSub = seedMatrix
	#qSub = seedMatrix, 
    	#v = seedMatrix,
    	#h = seedMatrix 

    )
    
    if(by == "day"){
	vConvFactor <- 60*60*24 
    }
    
    daysInMonth <- c(31, 31,28,31,30,31,30,31,31,30,31,30)


    for(timeStep in 1:timeLength){
	if(timeStep == 1){
	    start <- Sys.time()
	}

        if(by == "month"){
            vConvFactor <- 60*60*24*daysInMonth[as.integer(format(as.yearmon(rownames(Rsurf[timeStep,])), "%m"))%%12 + 1]/100
        }

	
        for(i in 1:nrow(edges)){

            hydroID <- as.character(edges[i,]$DrainID)
            
            if(edges[i,]$RiverOrder == 1){
                qIn <- 0
            } else {
                qIn <- sum(results$qOut[timeStep,
                as.character(
                edges[edges$NextDownID == edges[i,]$DrainID,]$DrainID)])
                results$qIn[timeStep, hydroID] <- qIn #sum(results$qIn[timeStep,c(as.character(parentEdges$DrainID))])
            }




	    rS <- Rsurf[timeStep, hydroID]
	    rSub <- Rsub[timeStep, hydroID]
	    len <- edges$LengthKM[i]
	    width <- edges$bfWidth[i]

	    if(timeStep == 1){
		sRiv <- 0
		height <- 1
		sSub <- 0
	    } else {
		sRiv <- results$sRiv[timeStep-1, hydroID]
		height <- (sRiv * vConvFactor*1000)/(len * 1000 * width)
		sSub <- results$sSub[timeStep-1, hydroID]
	    }


	    v <- (((height*width)/(2*height+width))^(2/3) * edges$SLOPE2[i]^.5)/manningN 
	    v <- ifelse(v >= 3.0, 3.0, v)	
	    v <- ifelse(v <= .25, .25, v)	

	    v <- v*vConvFactor #to convert to km per timeStep
	    
            qSub <- (sSub/edges$aCoeff[i])^(1/.5)
	    
	    #Assumes that l/v <= 1, if not, need different routing scheme 
	    qOut <- sRiv + (1-len/v)*qIn + (1 - len/(2*v))*(rS + qSub)


	    #results$v[timeStep, hydroID] <- v/vConvFactor
            #results$h[timeStep, hydroID] <- height
            results$qOut[timeStep, hydroID] <- qOut
            #results$qSub[timeStep, hydroID] <- qSub
            results$sRiv[timeStep, hydroID] <- sRiv + rS + qIn + qSub - qOut
            results$sSub[timeStep, hydroID] <- sSub + rSub - qSub


	    if(debugMode){
		print(" ")
	    	print(paste("Day:",timeStep, "Edge:", hydroID, "Order:", edges[i,]$RiverOrder))
	    	print(paste("qIn =", qIn))
	    	print(paste("qOut=",qOut))
	    	print(paste("qSub=",qSub))
	    	print(paste("sRiv =",sRiv))
	    	print(paste("rS =", rS))
	    	print(paste("rSub =",rSub))
	    	print(paste("sSub =",sSub))
	    	print(paste("aCoeff=",aCoeff))
	    	print(paste("len =", len))
	    	print(paste("width =", width))
	    	print(paste("height =", height))
	    }

	}



	if(timeStep == 10){
	    print(paste("ETA:", 
			round((Sys.time()-start)*length((timeStep+1):timeLength)/10, 2), 
			"seconds or", 
			round((Sys.time()-start)*length((timeStep+1):timeLength)/10/60, 2), 
			"minutes. Will finish at", 
			(Sys.time()-start)*length((timeStep+1):timeLength)/10+Sys.time()
		))
	}
    }
    
    return(results)
}
