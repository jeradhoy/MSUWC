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
generateRunoff <- function(ncFile, catchmentPolygons, runoffVar){
    brick <- brick(ncFile, varname=runoffVar)
    print("Finished building brick")
    runoff <- data.frame(t(extract(brick, catchmentPolygons, na.rm=TRUE, fun=sum)))
    print("finished extracting data")
    #convert from mm/m2/day to m3/sec
    runoff <- runoff/1000*1000000/(30*24*60*60)
    colnames(runoff) <- catchmentPolygons$HydroID
    #rownames(runoff) <- c(1:dim(brick)[3])
    write.table(runoff, "runoff.txt")
    return(runoff)
}

#Routes water simplistically by just 
routeWater <- function(edges, Rs){#, varTable=FALSE, plotNetwork=FALSE, writeData=FALSE){
    
    #Order edges by Shreve order so calculation is in right order
    edges <- edges[order(edges$RiverOrder),]
    
    results <- list(
    qIn = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs),
        dimnames=list(c(1:nrow(Rs)), edges$DrainID)),
    qOut = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs),
        dimnames=list(c(1:nrow(Rs)), edges$DrainID))
    )
    
    for(month in 1:nrow(Rs)){
	if(month == 1){
	    start <- Sys.time()
	}

        for(i in 1:nrow(edges)){

            hydroID <- edges[i,]$DrainID
            
            if(edges[i,]$RiverOrder == 1){
                qIn <- 0
            } else {
                qIn <- sum(results$qOut[month,
                as.character(
                edges[edges$NextDownID == edges[i,]$DrainID,]$DrainID)])
                results$qIn[month, as.character(hydroID)] <- qIn #sum(results$qIn[month,c(as.character(parentEdges$DrainID))])
            }
            
            results$qOut[month, as.character(hydroID)] <- qIn + Rs[month,as.character(hydroID)]
            
        }
	if(month == 10){
	    print(paste("ETA:", (Sys.time()-start)*length((month+1):nrow(Rs))/10, "seconds.", "Will finish at", (Sys.time()-start)*length((month+1):nrow(Rs))/10+Sys.time()))
	}
    }
    
    return(results)
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

getCatchInBounds <- function(catchments, hucCodes){
    catchmentCodes <- catchments$HUC10
    catchmentCodes[is.na(catchmentCodes)] <- 0
    catchments$HUC10 <- catchmentCodes
    return(catchments[catchments$HUC10 %in% hucCodes,])
}




routeWaterDimensions <- function(edges, Rs){#, varTable=FALSE, plotNetwork=FALSE, writeData=FALSE){
    
    #Order edges by Shreve order so calculation is in right order
    edges <- edges[order(edges$RiverOrder),]
    
    results <- list(
    qIn = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs),
        dimnames=list(c(1:nrow(Rs)), edges$DrainID)),
    qOut = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs),
        dimnames=list(c(1:nrow(Rs)), edges$DrainID)),
    sRiv = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs),
        dimnames=list(c(1:nrow(Rs)), edges$DrainID))
    )
    
    for(day in 1:nrow(Rs)){
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

	    rS <- Rs[day, hydroID]
	    len <- edges$LengthKM[i]
	    #width <- edges$bfWidth[i]
	    #slope <- edges$SLOPE2[i]
	    #manningN <- edges$manningN[i]
	    #manningN <- .05

	    if(day == 1){
		sRiv <- 0
		#height <- 1
	    } else {
		sRiv <- results$sRiv[day-1, hydroID]
		#height <- (results$qOut[day-1, hydroID] * 86400)/(len * 1000 * width)
		#height <- 1
	    }
	    

	    #v <- (((height*width)/(2*height+width))^(2/3) * (slope)^.5)/manningN 
	    #print(v)	    
            v <- 1.25
	    v <- v*86.4*30 #to convert to km per day
	    
	    #Assumes that l/v <= 1, if not, need different routing scheme 
	    qOut <- sRiv + (1-len/v)*qIn + (1 - len/(2*v))*rS
	    #qOut <- sRiv + (1-len/v)*(qIn+rS)
            results$qOut[day, hydroID] <- qOut
            results$sRiv[day, hydroID] <- sRiv + rS + qIn - qOut
            
         }
	if(day == 10){
	    print(paste("ETA:", 
			round((Sys.time()-start)*length((day+1):nrow(Rs))/10, 2), 
			"seconds or", 
			round((Sys.time()-start)*length((day+1):nrow(Rs))/10/60, 2), 
			"minutes. Will finish at", 
			(Sys.time()-start)*length((day+1):nrow(Rs))/10+Sys.time()
		))
	}
    }
    
    return(results)
}
