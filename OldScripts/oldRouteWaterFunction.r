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
