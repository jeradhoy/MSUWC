##############################
#
# Created by Jerad Hoy
# Date 5/26/2015
#
# Creates network from edge table and routes water.
#
#
#
##############################

#####################
#       TODO
# 1. Write data to text file
# 2. Integrate monthly runoff and velocity
# 4. Include width and depth from raymond to better approximate sRiv
# 2. Seperate into two r files maybe
# 3. Generate graphs of edges 
#
#
#
#####################
library(sp)
library(maptools)
library(raster)
library(igraph)
library(rgdal)

#######
#' Generate a matrix of random runoff
#'
#' @param nCatch - Number of catchments to make runoff for 
#' @param nMonth - Number of months to make runoff for - 30 years by default
#' @return A matrix of random runoff by catchment and months
#######

createRandomRunoff <- function(nCatch, nMonth=12*30){
    runoff <- matrix(sample(10:30,nMonth*nCatch,replace=T), ncol=nCatch)
    colnames(runoff) <- c(paste('w',1:nCatch,sep=''))
    rownames(runoff) <- rep(c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), ceiling(nMonth/12))[1:nMonth]
    return(runoff)
}

createNetwork <- function(edgeFrame){
    
    edgeFrame <- edgeFrame[order(edgeFrame$order),]
    
    network <- graph.data.frame(edgeFrame, directed=TRUE, vertices=unique(c(edgeFrame$fromNode, edgeFrame$toNode)))
    
    E(network)$qIn <- 0
    E(network)$qOut <- 0
    E(network)$sRiv <- 0
    
    ## Need to calculate this from average qout per month ####
    E(network)$baseFlow <- list(c(1,2,3,4,5,6,7,8,9,10,11,12))
    E(network)$baseVelocity <- list(c(1,2,3,4,5,6,7,8,9,10,11,12))
    
    
    return(network)
}

#######
#' Routes water through a network for one timestep (month)
#'
#' @param network - Stream network generated from the createNetwork func
#' @param Rs - array of runoff for one timestep, with each value corresponding to a catchment
#' @param monthCount - the month (mar=3), used to find baseflow for that month
#' @return A network with qIn, sRiv, and qOut updated, can also plot network, dataframe or output to a text file
#######
routeWater <- function(network, Rs, monthCount=6, varTable=FALSE, plotNetwork=FALSE, writeData=FALSE){
        
    for (edge in 1:length(E(network))){
        ###Should set initial values from each edge to speed up
        sRiv <- E(network)[edge]$sRiv
        qOut <- E(network)[edge]$qOut
        
        #waterLossCoeff <- .9 ###add in assume only 90% of water reaches the next edge
        #qIn <- E(network)[edge]$qIn
        #length <- E(network)[edge]$length
        
        E(network)[edge]$qIn <- sum(E(network)[to(get.edge(network, edge)[1])]$qOut)
                        
        E(network)[edge]$sRiv <- sRiv + E(network)[edge]$qIn - E(network)[edge]$qOut + Rs[edge]
        
        E(network)[edge]$qOut <- max(E(network)[edge]$sRiv, E(network)[[edge]]$baseFlow[monthCount])# * waterLossCoeff#* 1*60*60*24*30/ E(network)[edge]$length*1000

    }
    
    data <- data.frame(catchID=E(network)$catchID, order=E(network)$order, qIn=E(network)$qIn, qOut=E(network)$qOut, sRiv=E(network)$sRiv)
    if(varTable){
        print(data)
    }
    if(plotNetwork){
        plot(network, vertex.color="red", edge.color="deepskyblue", edge.label.color="black", vertex.label.color="black", vertex.label=NA)#, edge.label=E(network)$catchID)#ifelse("catchID" %in% list.edge.attributes(network), E(network)$catchID, NA))
    }
    if(writeData){
        write.table(data, "data.txt")
    }
                                     
    return(network)
}



runRiverRouting <- function(network, surfaceRunoffDF, varTable=TRUE, plotNetwork=TRUE){
       
    i <- 1
    
    while(i <= nrow(surfaceRunoffDF)){
        network <- routeWater(network, surfaceRunoffDF[i,], varTable=TRUE)
        i <- i + 1
    }
    
    #labels < paste("qIn:", E(network)$qIn, " -- qOut:", E(network)$qOut, " -- sRiv:", E(g)$sRiv)
    
    #print(data.frame(qIn=E(g)$qIn, qOut=E(g)$qOut, sRiv=E(g)$sRiv))
    if(varTable){
        print(data.frame(catchID=E(network)$catchID, order=E(network)$order, qIn=E(network)$qIn, qOut=E(network)$qOut, sRiv=E(network)$sRiv))
    }
    if(plotNetwork){
        plot(network, vertex.color="red", edge.color="deepskyblue", edge.label.color="black", vertex.label.color="black", vertex.label=NA, edge.label=E(network)$catchID)#ifelse("catchID" %in% list.edge.attributes(network), E(network)$catchID, NA))
    }
}

