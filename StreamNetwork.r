##############################
# Script for routing water
#
# Created by Jerad Hoy
# Date 5/26/2015
#
#
##############################

##Load neccessary libraries
library(sp)
library(maptools)
library(raster)
library(igraph)
library(rgdal)
library(ncdf)
library(raster)
library(foreach)
library(doParallel)
options(scipen=999)
library(mailR)
library(plotrix)

#Source in scripts for routing functions and notifier
source("notifyMe.r")
source("routingFunctions.r")

#Read in catchments, generate catchments in bounds by HUC10 codes
catchments <- readOGR("/Users/hoy/Desktop/MSUWC/Data/Catchments", "Catchments", stringsAsFactors=FALSE)
catchmentsInBounds <- getCatchInBounds(catchments, c(1007000105, 1007000106))
plot(catchmentsInBounds)

#Generate runoff from merged lpj-outputs
runoff <- generateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/merged.nc", catchmentsInBounds, "dsro")
notifyMe("Runoff generated")

#Read in edges, subset with names of runoff (could use catchmentsInBounds too)
hydroEdges <- readOGR("/Users/hoy/Desktop/MSUWC/Data/hydroEdge", "hydroEdge")
edgesInBounds <- hydroEdges[hydroEdges$DrainID %in% as.numeric(names(runoff)),]

plot(catchmentsInBounds, lwd=.5)
lines(edgesInBounds, col="blue", lwd=2)
plot(edgesInBounds, col="blue", lwd=2)

#Assign contributing area to edges
edgesInBounds <- assignContribArea(edgesInBounds)

#Route water with edges and runoff
flow <- routeWater(edgesInBounds, runoff)
notifyMe("Finished Routing")

####Need to multiply by 30 to fix conversion problem in generateRunoff(), can skip in future (fixed function)
flow$qIn <- flow$qIn*30
flow$qOut <- flow$qOut*30
#### DELETE ^ part when running again!

## Trying unsuccessfully to assign dated rownames to flow tables
dates <- seq(as.Date("1980/1/1"), as.Date("2012/12/31"), by="day")
dates <- dates[c(-grep("02-29", dates))]
length(dates)
rownames(flow$qIn) <- dates 
rownames(flow$qOut) <- dates

#Initial plotting to validate everthing worked
plot(as.Date(dates), flow$qIn[,"60298"], type="l")
plot(as.Date(dates), flow$qOut[,"60960"], type="l")







#Read in lamarTowerQ data, and process
lamarQ <- read.table("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06188000&period=P10000000W&parameterCd=00060", header=TRUE, stringsAsFactors = FALSE)
lamarQ <- lamarQ[-c(1),]
lamarQ <- lamarQ[-c(grep("02-29", lamarQ$datetime)),]
colnames(lamarQ)  <- c("agency_cd", "site_no", "datetime", "Q", "Qcd")
lamarQ$Q[lamarQ$Q == "Ice"] <- 0
lamarQ$Q <- as.numeric(lamarQ$Q)/35.3146666666666666666666666666666666667 #Converting ft3 to m3

#Plotting lamarTower gauge data with flow table
plot(as.Date(dates), flow$qOut[,"60960"], type="l",col="red", xlim=as.Date(c("1980-1-1","2012-1-1")), ylim=c(0,440), ylab="M3/s")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)

#Same as above, but with dates from 2005-2010 to get closer look at data
plot(as.Date(dates), flow$qOut[,"60960"], type="l",col="red", xlim=as.Date(c("2005-1-1","2010-1-1")), ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)


#Reading in Soda Butted gauge data and processing
sodaButteQ <- read.table("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06187915&period=P10000000W&parameterCd=00060", header=TRUE, stringsAsFactors = FALSE)
sodaButteQ <- sodaButteQ[-c(1),]
sodaButteQ <- sodaButteQ[-c(grep("02-29", sodaButteQ$datetime)),]
colnames(sodaButteQ)  <- c("agency_cd", "site_no", "datetime", "Q", "Qcd")
sodaButteQ$Q[sodaButteQ$Q == "Ice"] <- 0
sodaButteQ$Q <- as.numeric(sodaButteQ$Q)/35.3146666666666666666666666666666666667

#Plotting soda butte data with data from qIn of edge 60298
plot(as.Date(dates), flow$qIn[,"60298"], type="l",col="red", xlim=as.Date(c("1998-10-1","2012-1-1")), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime), sodaButteQ$Q)
title("Soda Butte Creek Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)

#Create taylor diagram to assess the models
taylor.diagram(
	       lamarQ$Q[which(lamarQ$datetime == "1990-01-01"):which(lamarQ$datetime == "2012-12-31")],
	       flow$qOut[which(dates == "1990-01-01"):which(dates == "2012-12-31"),"60960"])

taylor.diagram(
	       sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")],
	       flow$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"], add=TRUE, col="blue")

#Getting contributing area for two gauge edges
edgesInBounds[edgesInBounds$HydroID_1 == "60298",]$ContribArea
edgesInBounds[edgesInBounds$HydroID_1 == "60960",]$ContribArea


exit
############
#
#
#OLD CODE - NEED to go through and save what is needed
#
#
#############



runoff <- generateRunoff("/Users/hoy/Desktop/MSUWC/Data/UL_stand_monthly_p100_ID80408.nc", readOGR("/Users/hoy/Desktop/MSUWC/Data/Catchments", "Catchments", stringsAsFactors=FALSE), "msro", 1007000105)

hydroEdges <- readOGR("/Users/hoy/Desktop/MSUWC/Data/hydroEdge", "hydroEdge")

edgesInBounds <- hydroEdges[hydroEdges$DrainID %in% as.numeric(names(runoff)),]
length(edgesInBounds)
edgesInBounds <- assignContribArea(edgesInBounds)

catchments <- readOGR("/Users/hoy/Desktop/MSUWC/Data/Catchments", "Catchments", stringsAsFactors=FALSE)
#catchInBounds <- catchments[catchments$HydroID %in% as.numeric(names(runoff)),]



catchments <- readOGR("/Users/hoy/Desktop/MSUWC/Data/Catchments", "Catchments", stringsAsFactors=FALSE)

catchmentCodes <- catchments$HUC10
catchmentCodes[is.na(catchmentCodes)] <- 0
catchments$HUC10 <- catchmentCodes

catchInBounds <- catchments[catchments$HUC10 == 1007000105,]
runoff <- generateRunoff("/Users/hoy/Desktop/MSUWC/Data/UL_stand_monthly_p100_ID80408.nc", readOGR("/Users/hoy/Desktop/MSUWC/Data/Catchments", "Catchments", stringsAsFactors=FALSE), "msro", 1007000105)for(i in 1:396){
    plot(runoff[,i], type="l", ylim=c(0,8), lwd=.3, ylab="Runoff for each Catchment (m3/month)", xlab="Time (months)")
}plot(runoff[,1], type="l", ylim=c(0,8), lwd=.3, ylab="Runoff for each Catchment (m3/month)", xlab="Time (months)")
for(i in 2:396){
    lines(runoff[,i], lwd=.3)
}edges <- readOGR("/Users/hoy/Desktop/MSUWC/Data/hydroEdge", "hydroEdge")edgesInBounds <- edges[edges$DrainID %in% as.numeric(names(runoff)),]HydroID_1 <- 61027  
edge1 <- edgesInBounds[edgesInBounds$HydroID_1 == HydroID_1,]

edgesInBounds$qOut <- rep(list(matrix(, nrow = nrow(runoff), ncol = 2, dimnames=list(c(1:nrow(runoff)), c("qIn", "qOut")))), nrow(edgesInBounds))
edgesInBounds[1,]$qOutplot(edgesInBounds)for(i in 1:nrow(edgesInBounds)){
    edgesInBounds[i, "x1"] <- coordinates(edgesInBounds[i,])[[1]][[1]][1,1]
    edgesInBounds[i, "y1"] <- coordinates(edgesInBounds[i,])[[1]][[1]][1,2]
    edgesInBounds[i, "x2"] <- tail(coordinates(edgesInBounds[i,])[[1]][[1]], n=1)[1]
    edgesInBounds[i, "y2"] <- tail(coordinates(edgesInBounds[i,])[[1]][[1]], n=1)[2]
}edgeFrame <- data.frame(fromNode = edgesInBounds$FROM_NODE, 
                    toNode = edgesInBounds$TO_NODE,
                    order = edgesInBounds$RiverOrder,
                    length = edgesInBounds$Shape_Leng,
                    catchID = edgesInBounds$DrainID,
                    x1 = edgesInBounds$x1,
                    y1 = edgesInBounds$y1,
                    x2 = edgesInBounds$x2,
                    y2 = edgesInBounds$y2
                    )

#nrow(edgesInBounds)
net <- createNetwork(edgeFrame)
#plot(catchments[catchments$HydroID %in% as.numeric(names(runoff)),])
#E(net)
for(e in 1:length(E(net))){
    V(net)[from(e)]$x <- E(net)[e]$x1
    V(net)[from(e)]$y <- E(net)[e]$y1
    V(net)[to(e)]$x <- E(net)[e]$x2
    V(net)[to(e)]$y <- E(net)[e]$y2
}

#V(net)$x[is.na(V(net)$x)] <- -109.8 #sample(-seq(109.8, 110.2, .01),13,replace=T)
#V(net)$y[is.na(V(net)$y)] <- 44.97 #sample(seq(44.57, 44.97, .01),13,replace=T)

V(net)$size = 2
plot(net, vertex.label=NA, edge.label=E(net)$order, edge.arrow.mode=0)
#plot(catchmentsInBounds)routeWater(net, runoff[66,], TRUE, TRUE)runRiverRouting(net, runoff)edges1 <- data.frame(fromNode = seq(70000, 70002), 
                    toNode = c(70002, 70002, 70003),#, 70005, 70005, 70006, 70006, 70007),
                    row.names = seq(50000, 50002),
                    order = c(1,1,2),
                    length = c(1,1,1),
                    catchID = c(1,2,3))

#gLayout <- matrix(c(2,0,1,1, 1.8, 2.3, 1, 0),4)edges2 <- data.frame(fromNode = seq(70001, 70007),
                    toNode = c(70005, 70005, 70006, 70006, 70007,70007, 70008),#, 70005, 70005, 70006, 70006, 70007),
                    row.names = seq(50001, 50007),
                    order = c(1,1,1,1,2,2,3),
                    length = c(1,1,1,1,1,1,1),
                    catchID = c(1,2,3,4,5,6,7))g <- createNetwork(edges2)


routeWater(g, createRandomRunoff(length(E(g)), 1), varTable=TRUE, plotNetwork=TRUE)

runRiverRouting(g, createRandomRunoff(length(E(g)), 10), plotNetwork=TRUE)df <- edges2
runRiverRouting(createNetwork(df), createRandomRunoff(nrow(df), 20))
bhEdgesShp <- readOGR("/Users/hoy/Desktop/bhEdges", "bhEdges")
#print(bhEdgesShp[1:5,])

a <- coordinates(bhEdgesShp[1,])
#print(a)
print(a)[[1]]

bhEdgesShp$x.coords

bhEdges <- data.frame(fromNode = bhEdgesShp$HydroEdg_4, 
                    toNode = bhEdgesShp$HydroEdg_3,
                    order = bhEdgesShp$Catchmen_1,
                    length = bhEdgesShp$HydroEdge_,
                    catchID = bhEdgesShp$HydroEdg_1)

df <- bhEdgeshh
net <- createNetwork(df)
V(net)$size = 1
plot(net, vertex.label=NA, edge.label=NA, edge.arrow.mode=0)
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
                    length = hydroEdgeShp$Shape_Leng *  ,
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

library(ncdf)
library(raster)

#testNcdf <- create.ncdf(filename="testNcdf.nc", vars="Rs")
z = matrix(sample(10:30,10000,replace=T), ncol=100)      # matrix of elevations
x = 100* (1:nrow(z))   # meter spacing (S to N)
y = 100* (1:ncol(z))   # meter spacing (E to W)

# define the netcdf coordinate variables -- note these have values!

dim1 = dim.def.ncdf( "EW","kilometers", as.double(x))
dim2 = dim.def.ncdf( "SN","kilometers", as.double(y))

# define the EMPTY (elevation) netcdf variable

varz = var.def.ncdf("Elevation","meters", list(dim1,dim2), -1, 
          longname="The Classic R New Zealand Volcano")

# associate the netcdf variable with a netcdf file   
# put the variable into the file, and
# close

nc.ex = create.ncdf( "example.nc", varz )
put.var.ncdf(nc.ex, varz, z)

http://waterservices.usgs.gov/nwis/iv/?sites=08313000&period=P7D&format=rdb

wget --header="Accept-Encoding: gzip" http://waterservices.usgs.gov/nwis/iv/?sites=08313000&period=P7D&format=rdb

http://waterservices.usgs.gov/nwis/iv/?sites=08313000&period=P7D&format=rdb

http://waterservices.usgs.gov/nwis/iv/?sites=08313000,########,#######,&startDT=1990-1-1&format=rdb

http://waterservices.usgs.gov/nwis/iv/?sites=06188000&startDT=1990-1-1&format=rdb

curl -o -H 'Accept-Encoding: gzip,deflate' targetFile "httpAdress"

curl -o -H 'Accept-Encoding: gzip,deflate' targetFile "http://waterservices.usgs.gov/nwis/iv/?sites=08313000&period=P7D&format=rdb"

curl -o targetfile 

#### This actually worked!

curl -o lamarTower2 "http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06188000&period=P10000000W&parameterCd=00060"

curl -o lamarTower2 "http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06188000&period=P10000000W"

lamarQ <- read.table("/Users/hoy/Desktop/MSUWC/Data/usgsGuage/lamarTower2", header=TRUE, stringsAsFactors = FALSE)

lamarQ <- lamarQ[-c(1),]

length(lamarQ$X01_00060_00003)
lamarQ$X01_00060_00003[lamarQ$X01_00060_00003 == "Ice"] <- 0
lamarQ$X01_00060_00003 <- as.numeric(lamarQ$X01_00060_00003)/35.31466666666666667

head(lamarQ$datetime)

plot(tail(lamarQ$X01_00060_00003[], n=1000), type="l")

ff <- open.ncdf("/Users/hoy/Desktop/MSUWC/Data/UL_stand_monthly_p100_ID80408.nc")
mro.mm.m2.month <- get.var.ncdf(ff, "mro")
mro.m.m2.month <- mro.mm.m2.month/(1000)         #Convert to meters per month per m2
mro.m3.month <- mro.m.m2.month*1000000                      #Convert to meters per month per gridcell (1km2 grid cell area)
mro.m3.second <- mro.m3.month/(30*24*60*60)     #Convert to meters cubed per second per gridcell

mro.m3.second.basin <- 0

for(m in 1:396){
    mro.m3.second.basin[m] <- sum(mro.m3.second [,,m], na.rm=T)
}
plot(tail(mro.m3.second.basin, n=300), type="l", ylab="Q (m3/s)")
#plot(tail(lamarQ$X01_00060_00003[], n=10000), type="l")

tail(mro.m3.second.basin, n=5)


routeWaterParallel2 <- function(edges, Rs){#, varTable=FALSE, plotNetwork=FALSE, writeData=FALSE){
    
    #cl <- makeCluster(8)
    #registerDoParallel(cl)

    #Order edges by Shreve order so calculation is in right order
    edges <- edges[order(edges$RiverOrder),]
    
    results <- list(
        qIn = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs), 
                dimnames=list(c(1:nrow(Rs)), edges$HydroID_1)),
        qOut = matrix(0, nrow=nrow(Rs), ncol=ncol(Rs), 
                dimnames=list(c(1:nrow(Rs)), edges$HydroID_1))
    )
    
    
    #Order edges by Shreve order so calculation is in right order 
    foreach(order = icount(max(edges$RiverOrder))) %do% {
    
    #print(paste("Processing edges or order", order))
    
    edgesO <- edges[edges$RiverOrder == order,]
    
    foreach(month = icount(nrow(Rs))) %do% {
        
        
        #print(paste("Processing Month", month, "of order", order))
        
        foreach(i = icount(nrow(edgesO))) %dopar% {
            ###Should set initial values from each edge to speed up
            #sRiv <- E(network)[edge]$sRiv
            
            
            #print(paste("Processing Month", month, "of order", order, "for edge", i))

            
            hydroID <- edgesO[i,]$HydroID_1
            
            if(edgesO[i,]$RiverOrder == 1){
                qIn <- 0
            } else {

                qIn <- sum(results$qOut[month, 
                    c(as.character(
                        edges[edges$NextDownID == edgesO[i,]$HydroID_1,]$HydroID_1))])
                results$qIn[month, as.character(hydroID)] <- qIn #sum(results$qIn[month,c(as.character(parentEdges$HydroID_1))])
            }
            
            results$qOut[month, as.character(hydroID)] <- qIn + Rs[month,as.character(hydroID)]

        }
    }
   }
    return(results)
    
}


