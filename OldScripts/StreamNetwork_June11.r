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


######
#
# Sample edges and routing
#
######

edges1 <- data.frame(fromNode = seq(70000, 70002), 
                    toNode = c(70002, 70002, 70003),#, 70005, 70005, 70006, 70006, 70007),
                    row.names = seq(50000, 50002),
                    order = c(1,1,2),
                    length = c(1,1,1),
                    catchID = c(1,2,3))

#gLayout <- matrix(c(2,0,1,1, 1.8, 2.3, 1, 0),4)




edges2 <- data.frame(fromNode = seq(70001, 70007),
                    toNode = c(70005, 70005, 70006, 70006, 70007,70007, 70008),#, 70005, 70005, 70006, 70006, 70007),
                    row.names = seq(50001, 50007),
                    order = c(1,1,1,1,2,2,3),
                    length = c(1,1,1,1,1,1,1),
                    catchID = c(1,2,3,4,5,6,7))

#edges2runoff <- createRandomRunoff(7, 1)
#runoff



g <- createNetwork(edges2)
routeWater(g, createRandomRunoff(length(E(g)), 1), varTable=TRUE, plotNetwork=TRUE)

runRiverRouting(g, createRandomRunoff(length(E(g)), 10), plotNetwork=TRUE)


g <- routeWater(g, createRandomRunoff(7, 1))
g <- routeWater(g, createRandomRunoff(7, 1))
g <- routeWater(g, createRandomRunoff(7, 1))
g <- routeWater(g, runoff, plotNetwork=TRUE, varTable=TRUE)

#plot(g, vertex.color="red", edge.color="blue",)g <- routeWater(createNetwork(edges1), createRandomRunoff(3)[1,])
g <- routeWater(g, createRandomRunoff(3)[1,])
g <- routeWater(g, createRandomRunoff(3)[1,])
g <- routeWater(g, createRandomRunoff(3)[1,], varTable=TRUE, plotNetwork=TRUE)

#print("qIn:", E(g)$qIn)
#print(data.frame(qIn=E(g)$qIn, qOut=E(g)$qOut, sRiv=E(g)$sRiv))

#labels <- paste("qIn:", E(g)$qIn, " -- qOut:", E(g)$qOut, " -- sRiv:", E(g)$sRiv)
#plot(g, vertex.color="red", edge.color="blue", layout=gLayout)df <- edges2
runRiverRouting(createNetwork(df), createRandomRunoff(nrow(df), 20))



bhEdgesShp <- readOGR("/Users/hoy/Desktop/bhEdges", "bhEdges")

bhEdges <- data.frame(fromNode = bhEdgesShp$HydroEdg_4, 
                    toNode = bhEdgesShp$HydroEdg_3,
                    order = bhEdgesShp$Catchmen_1,
                    length = bhEdgesShp$HydroEdge_,
                    catchID = bhEdgesShp$HydroEdg_1,
                    x = numeric(nrow(bhEdgesShp)),
                    y = numeric(nrow(bhEdgesShp))
                    )
for(i in 1:nrow(bhEdges)){
    bhEdges[i,"x"] <- abs(coordinates(bhEdgesShp)[[i]][[1]][1,1]) - 110.5
    bhEdges[i,"y"] <- coordinates(bhEdgesShp)[[i]][[1]][1,2] - 44.3
}

unlist(coordinates(bhEdgesShp[3,]))[c(1,3)]
coordinates(bhEdgesShp[3,])[[1]][[1]][1,]


df <- bhEdges
net <- createNetwork(df)
V(net)$size = 1
plot(net, vertex.label=NA, edge.label=NA, edge.arrow.mode=0, layout=cbind(E(net)$x, E(net)$y))

matrix(E(net))




hydroEdgeShp <- readOGR("/Users/hoy/Desktop/hydroEdge", "hydroEdges")


df <- hydroEdges
net <- createNetwork(df)

runoff <- createRandomRunoff(nrow(df), 1)
runoff
#list.edge.attributes(net)
#print(data.frame(catchID=E(net)$catchID, qIn=E(net)$qIn, qOut=E(net)$qOut, sRiv=E(net)$sRiv))


#print(data.frame(name=V(net)$name))
V(net)$size = 5
routeWater(net, runoff, varTable=TRUE, plotNetwork=TRUE)
#runRiverRouting(createNetwork(df), createRandomRunoff(nrow(df), 1))

hydroEdges <- data.frame(fromNode = hydroEdgeShp$FROM_NODE, 
                    toNode = hydroEdgeShp$TO_NODE,
                    order = hydroEdgeShp$RiverOrder,
                    length = hydroEdgeShp$Shape_Leng,
                    catchID = hydroEdgeShp$DrainID)
hydroEdges[1:10,]

df <- hydroEdges[1:10,]
net <- createNetwork(df)

runoff <- createRandomRunoff(nrow(df), 1)
runoff
#list.edge.attributes(net)
#print(data.frame(catchID=E(net)$catchID, qIn=E(net)$qIn, qOut=E(net)$qOut, sRiv=E(net)$sRiv))


#print(data.frame(name=V(net)$name))
V(net)$size = 5
routeWater(net, runoff, varTable=TRUE, plotNetwork=TRUE)
#runRiverRouting(createNetwork(df), createRandomRunoff(nrow(df), 1))


