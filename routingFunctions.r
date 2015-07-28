###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:
#
#Sourced into streamNetwork.r file to do actual routing
#


#Generates matrix of runoff by catchment and time step
#Builds raster brick and uses "extract" function to accumulate runoff
#writes to runoff.txt text file
aggregateRunoff <- function(ncFile, catchmentPolygons, runoffVar, fname=NULL){
    brick <- brick(ncFile, varname=runoffVar)
    print("finished building brick")
    runoff <- data.frame(t(extract(brick, catchmentPolygons, na.rm=T, fun=sum)))
    print("finished extracting data")
    #convert from mm/m2/day to m3/sec
    runoff <- runoff/1000*1000000/(24*60*60)
    colnames(runoff) <- catchmentPolygons$HydroID
    #rownames(runoff) <- c(1:dim(brick)[3])
    if(!is.null(fname)){
    	write.table(runoff, paste(fname, ".txt", sep=""))
    }
    return(runoff)
}

aggregateRunoff2 <- function(ncFile, catchmentPolygons, runoffVars, fname=NULL){
    for(ncVar in runoffVars){
    	ncBrick <- brick(ncFile, varname=ncVar)
    	print(paste("finished building", ncVar, "brick"))
    	assign(ncVar, t(extract(ncBrick, catchmentPolygons, na.rm=TRUE, fun=sum))/1000*1000000/(24*60*60))
    	print(paste("finished extracting", ncVar, "data"))
    	#rownames(runoff) <- c(1:dim(brick)[3])
    }

    runoff <- mget(runoffVars) 
    runoff <- lapply(runoff, function(x) {
	   colnames(x) <- catchmentPolygons$HydroID
	   return(x)
    })

    if(!is.null(fname)){
    	write.table(runoff, paste(fname, ".txt", sep=""))
    }
    return(runoff)
}


correctEdgeSlopes <- function(edges){
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




assignContribArea <- function(edges){
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


assignBfDepth <- function(edges, a, b){

    edges$bfDepth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfDepth"] <- a*(edges[i,]$ContribArea)^b
    }
    return(edges)
}


assignBfWidth <- function(edges, a, b){

    edges$bfWidth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfWidth"] <- a*(edges[i,]$ContribArea)^b
    }
    return(edges)
}

assignAcoeff <- function(edges, coeff){

    edges$aCoeff <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"aCoeff"] <- (edges[i,]$Shape_Ar_1)*coeff
    }
    return(edges)
}


getCatchInBounds <- function(catchments, hucCodes){
    catchmentCodes <- catchments$HUC10
    catchmentCodes[is.na(catchmentCodes)] <- 0
    catchments$HUC10 <- catchmentCodes
    return(catchments[catchments$HUC10 %in% hucCodes,])
}




routeWater <- function(edges, Rsurf, Rsub, debugMode=F){
    
    #Order edges by Shreve order so calculation is in right order
    edges <- edges[order(edges$RiverOrder),]

    timeLength <- nrow(Rsurf) 
    seedMatrix <- matrix(0, nrow=timeLength, ncol=ncol(Rsurf),
        dimnames=list(c(1:timeLength), edges$DrainID))

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
    
    for(day in 1:timeLength){
	if(day == 1){
	    start <- Sys.time()
	}

        for(i in 1:nrow(edges)){

            hydroID <- as.character(edges[i,]$DrainID)
            
            if(edges[i,]$RiverOrder == 1){
                qIn <- 0
            } else {
                qIn <- sum(results$qOut[day,
                as.character(
                edges[edges$NextDownID == edges[i,]$DrainID,]$DrainID)])
                results$qIn[day, hydroID] <- qIn #sum(results$qIn[day,c(as.character(parentEdges$DrainID))])
            }




	    rS <- Rsurf[day, hydroID]
	    rSub <- Rsub[day, hydroID]
	    len <- edges$LengthKM[i]
	    width <- edges$bfWidth[i]

	    if(day == 1){
		sRiv <- 0
		height <- 1
		sSub <- 0
	    } else {
		sRiv <- results$sRiv[day-1, hydroID]
		height <- (sRiv * 86400)/(len * 1000 * width)
		sSub <- results$sSub[day-1, hydroID]
	    }


	    v <- (((height*width)/(2*height+width))^(2/3) * edges$SLOPE2[i]^.5)/manningN 
	    v <- ifelse(v >= 3.0, 3.0, v)	
	    v <- ifelse(v <= .25, .25, v)	

	    v <- v*86.4 #to convert to km per day
	    
            qSub <- (sSub/edges$aCoeff[i])^(1/.5)
	    
	    #Assumes that l/v <= 1, if not, need different routing scheme 
	    qOut <- sRiv + (1-len/v)*qIn + (1 - len/(2*v))*(rS + qSub)


	    #results$v[day, hydroID] <- v/86.4
            #results$h[day, hydroID] <- height
            results$qOut[day, hydroID] <- qOut
            #results$qSub[day, hydroID] <- qSub
            results$sRiv[day, hydroID] <- sRiv + rS + qIn + qSub - qOut
            results$sSub[day, hydroID] <- sSub + rSub - qSub


	    if(debugMode){
		print(" ")
	    	print(paste("Day:",day, "Edge:", hydroID, "Order:", edges[i,]$RiverOrder))
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



	if(day == 10){
	    print(paste("ETA:", 
			round((Sys.time()-start)*length((day+1):timeLength)/10, 2), 
			"seconds or", 
			round((Sys.time()-start)*length((day+1):timeLength)/10/60, 2), 
			"minutes. Will finish at", 
			(Sys.time()-start)*length((day+1):timeLength)/10+Sys.time()
		))
	}
    }
    
    return(results)
}
