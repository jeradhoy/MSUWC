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
    #convert from mm/m2/month to m3/sec
    runoff <- runoff/1000*1000000/(24*60*60)
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
        dimnames=list(c(1:nrow(Rs)), edges$HydroID_1)),
    qOut = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs),
        dimnames=list(c(1:nrow(Rs)), edges$HydroID_1))
    )
    
    for(month in 1:nrow(Rs)){
	if(month == 1){
	    start <- Sys.time()
	}

        for(i in 1:nrow(edges)){

            hydroID <- edges[i,]$HydroID_1
            
            if(edges[i,]$RiverOrder == 1){
                qIn <- 0
            } else {
                qIn <- sum(results$qOut[month,
                as.character(
                edges[edges$NextDownID == edges[i,]$HydroID_1,]$HydroID_1)])
                results$qIn[month, as.character(hydroID)] <- qIn #sum(results$qIn[month,c(as.character(parentEdges$HydroID_1))])
            }
            
            results$qOut[month, as.character(hydroID)] <- qIn + Rs[month,as.character(hydroID)]
            
        }
	if(month == 10){
	    print(paste("ETA:", (Sys.time()-start)*length((month+1):nrow(Rs))/10, "seconds.", "Will finish at", (Sys.time()-start)*length((month+1):nrow(Rs))/10+Sys.time()))
	}
    }
    
    return(results)
}

assignContribArea <- function(edges){
    edges <- edges[order(edges$RiverOrder),]
    edges$ContribArea <- NA
    
    for(i in 1:nrow(edges)){
        
        if(edges[i,]$RiverOrder == 1){
            #print(paste("Area of stream order", edges[i,]$RiverOrder, "is", edges[i,]$Shape_Ar_1))
            edges[i,"ContribArea"] <- edges[i,]$Shape_Ar_1
        } else {
            #print(paste("Area of stream order", edges[i,]$RiverOrder, "is", sum(edges[edges$NextDownID == edges[i,]$HydroID_1,]$Shape_Ar_1) + edges[i,]$Shape_Ar_1))
            #print(sum(edges[edges$NextDownID == edges[i,]$HydroID_1,]$Shape_Ar_1) + edges[i,]$Shape_Ar_1)
            edges[i,"ContribArea"] <- sum(edges[edges$NextDownID == edges[i,]$HydroID_1,]$ContribArea) + edges[i,]$Shape_Ar_1 #sum(results$qIn[month,c(as.character(parentEdges$HydroID_1))])
        }
        
    }
    return(edges)
}


assignBfDepth <- function(edges, a, b){

    edges$bfDepth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfDepth"] <- a(edges[i,]$ContribArea)^b
    }
    return(edges)
}


assignBfWidth <- function(edges, a, b){

    edges$bfWidth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfWidth"] <- a(edges[i,]$ContribArea)^b
    }
    return(edges)
}

getCatchInBounds <- function(catchments, hucCodes){
    catchmentCodes <- catchments$HUC10
    catchmentCodes[is.na(catchmentCodes)] <- 0
    catchments$HUC10 <- catchmentCodes
    return(catchments[catchments$HUC10 %in% hucCodes,])
}
