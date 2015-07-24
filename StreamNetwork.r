#############################
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

lamarNC <- open.ncdf("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/merged.nc")
snowpack <- get.var.ncdf(lamarNC, "spack")
dailySpack <- apply(snowpack, 3, sum, na.rm=T)
dailySnowpack <- dailySpack/1000*1000000/(24*60*60)
plot(as.Date(dates), dailySnowpack, type="l")

lamarPrecip <- open.ncdf("/Users/hoy/Desktop/MSUWC/Data/DriverData/Daymet/prcp_gye_1980_2012_daily_latlon_monthly.nc") 
dailyPrecip <- apply(get.var.ncdf(lamarPrecip, "prcp"), 3, sum, na.rm=T)
monthDates <- seq(as.Date("1980/1/1"), as.Date("2012/12/31"), by="month")
plot(monthDates, dailyPrecip, type="l")


/1000*1000000/(24*60*60)


#Read in catchments, generate catchments in bounds by HUC10 codes
catchments <- readOGR("/Users/hoy/Desktop/MSUWC/Data/Catchments", "Catchments", stringsAsFactors=FALSE)
catchmentsInBounds <- getCatchInBounds(catchments, c(1007000105, 1007000106))
plot(catchmentsInBounds)

#Generate runoff from merged lpj-outputs
runoff <- generateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/merged.nc", catchmentsInBounds, "dsro")
notifyMe("Runoff generated")

runoffTotal <- generateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/merged.nc", catchmentsInBounds, "dro")
notifyMe("Total Runoff generated")

flowTotal <- routeWater(edgesInBounds, runoffTotal)
notifyMe("Finished Routing")

source("routingFunctions.r") 
#Read in edges, subset with names of runoff (could use catchmentsInBounds too)
hydroEdges <- readOGR("/Users/hoy/Desktop/MSUWC/Data/hydroEdge2", "hydroEdge2")
hydroNames <- names(hydroEdges)
hydroNames[2] <- "Shape_Length"
hydroNames[4] <- "DrainID"
names(hydroEdges) <- hydroNames
names(hydroEdges)
edgesInBounds <- hydroEdges[hydroEdges$DrainID %in% as.numeric(names(runoff)),]

#edgesInBounds <- correctEdgeSlopes(edgesInBounds)



plot(catchmentsInBounds, lwd=.5)
lines(edgesInBounds, col="blue", lwd=2)
plot(edgesInBounds, col="blue", lwd=2)

#Assign contributing area to edges
edgesInBounds <- assignContribArea(edgesInBounds)
#Assigning bank-full depth, using a = .1 and b = .4 from Li paper
edgesInBounds <- assignBfDepth(edgesInBounds, .1, .4)
#Assigning bank-full width, using a = .1 and b = .6 from Li paper
edgesInBounds <- assignBfWidth(edgesInBounds, .1, .6)
#Assign reach length in KM
edgesInBounds$LengthKM <- edgesInBounds$Shape_Length*120

slopes <- edgesInBounds$SLOPE/120000
slopes[slopes <= 0] <- median(edgesInBounds$SLOPE2)
edgesInBounds$SLOPE2 <- slopes




#Route water with edges and runoff
source("routingFunctions.r")
flowDim <- routeWaterDimensions(edgesInBounds, runoff[which(dates == "2012-05-01"):which(dates == "2012-06-30"), ])
#notifyMe("Finished Routing surface runoff with dim")
flowDim$qOut <- flowDim$qOut*30
flowDim$qIn <- flowDim$qIn*30
flowDim$sRiv <- flowDim$sRiv*30

plot(flowDim$qOut[,"60960"],type="l") 
lines(flow2$qOut[,"60960"], type="l", col="red") 


flow2 <- routeWater(edgesInBounds, runoff[which(dates == "2012-05-01"):which(dates == "2012-06-30"), ])
flow2$qOut <- flow2$qOut*30
flow2$qIn <- flow2$qIn*30
flow2$sRiv <- flow2$sRiv*30

lines(flow2$qOut[,"60960"], type="l") 



plot(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flowDim$qOut[,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])

head(which(dates == "1999-01-01"):which(dates == "2012-12-31"))
source("routingFunctions.r")
flowDim2 <- routeWaterDimensions(edgesInBounds, runoff[1:365,])
flow2 <- routeWater(edgesInBounds, runoff[1:365,])

flowDim1.5 <- routeWaterDimensions(edgesInBounds, runoff)
flowDim1.5$qOut <- flowDim1.5$qOut*30
notifyMe("Finished routing surface water with velocity of 1.5")
notifyMe(paste(gof(flowDim1.5$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]), collapse=", "))

flowTotalDim <- routeWaterDimensions(edgesInBounds, runoffTotal[which(dates == "1999-01-01"):which(dates == "2012-12-31"), ])
notifyMe("Finished Routing flowTotal for 1999-2012")




edgesInBounds[edgesInBounds$HydroID_1 == "60298",]$LengthKM
edgesInBounds[edgesInBounds$HydroID_1 == "60960",]$LengthKM

plot(flow2$qIn[,"60298"]*30, type="l",ylim=c(0,20))
lines(flow$qIn[1:365,"60298"], type="l")
lines(flow2$sRiv[,"60298"]*30, type="l")

plot(flow2$qOut[,"60960"]*30, type="l")
lines(flow$qOut[1:365,"60960"], type="l")
lines(flow2$sRiv[,"60960"]*30, type="l")




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
par(new=T)
plot(as.Date(dates), dailySnowpack, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
axis(4)
par(new=T)
plot(monthDates, dailyPrecip, type="l", col="blue", lty=3, lwd=2, axes=F, xlab=NA, ylab=NA)


short.date = strftime(lamarQ$datetime, "%Y/%m")
aggr.stat = aggregate(lamarQ$Q ~ short.date, FUN = sum)
aggr.stat


#Read in lamarTowerQ data, and process
lamarQ <- read.table("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06188000&period=P10000000W&parameterCd=00060", header=TRUE, stringsAsFactors = FALSE)
lamarQ <- lamarQ[-c(1),]
lamarQ <- lamarQ[-c(grep("02-29", lamarQ$datetime)),]
lamarQ[,3] <- as.Date(lamarQ[,3])
colnames(lamarQ)  <- c("agency_cd", "site_no", "datetime", "Q", "Qcd")
lamarQ$Q[lamarQ$Q == "Ice"] <- 0
lamarQ$Q <- as.numeric(lamarQ$Q)/35.3146666666666666666666666666666666667 #Converting ft3 to m3

#Plotting lamarTower gauge data with flow table
plot(as.Date(dates), flow$qOut[,"60960"], type="l",col="red", xlim=as.Date(c("1980-1-1","2012-1-1")), ylim=c(0,440), ylab="M3/s")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)

#Same as above, but with dates from 2005-2010 to get closer look at data
plot(as.Date(dates), flow$qOut[,"60960"], type="l",col="red", xlim=as.Date(c("2000-1-1","2010-1-1")), ylim=c(0,350), ylab="M3/s")
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


#
#
# Graphing and analysis of total surface runoff data
#
#
#






#Initial plotting to validate everthing worked
plot(as.Date(dates), flowTotal$qIn[,"60298"], type="l")
plot(as.Date(dates), flowTotal$qOut[,"60960"], type="l")





#Read in lamarTowerQ data, and process
lamarQ <- read.table("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06188000&period=P10000000W&parameterCd=00060", header=TRUE, stringsAsFactors = FALSE)
lamarQ <- lamarQ[-c(1),]
lamarQ <- lamarQ[-c(grep("02-29", lamarQ$datetime)),]
colnames(lamarQ)  <- c("agency_cd", "site_no", "datetime", "Q", "Qcd")
lamarQ$Q[lamarQ$Q == "Ice"] <- 0
lamarQ$Q <- as.numeric(lamarQ$Q)/35.3146666666666666666666666666666666667 #Converting ft3 to m3

#Plotting lamarTower gauge data with flowTotal table
plot(as.Date(dates), flowTotal$qOut[,"60960"], type="l",col="red", xlim=as.Date(c("1980-1-1","2012-1-1")), ylim=c(0,440), ylab="M3/s")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)

#Same as above, but with dates from 2005-2010 to get closer look at data
plot(as.Date(dates), flowTotal$qOut[,"60960"], type="l",col="red", xlim=as.Date(c("2005-1-1","2010-1-1")), ylim=c(0,350), ylab="M3/s")
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
plot(as.Date(dates), flowTotal$qIn[,"60298"], type="l",col="red", xlim=as.Date(c("1998-10-1","2012-1-1")), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime), sodaButteQ$Q)
title("Soda Butte Creek Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)

#Create taylor diagram to assess the models
taylor.diagram(
	       lamarQ$Q[which(lamarQ$datetime == "1990-01-01"):which(lamarQ$datetime == "2012-12-31")],
	       flowTotal$qOut[which(dates == "1990-01-01"):which(dates == "2012-12-31"),"60960"])

taylor.diagram(
	       sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")],
	       flowTotal$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"], add=TRUE, col="blue")

dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")]


cbind(
    gof(flowTotalDim$qOut[,"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flowTotal$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flow$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flowDim$qOut[,"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
)





#Goodnes of fit and plot for flowTotaldim and lamar tower data
gof(flowTotalDim$qOut[,"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])

plot(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flowTotalDim$qOut[,"60960"], type="l",col="red", ylim=c(0,440), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)

  

#Goodness of fit and plot for flowTotal and lamar tower data
gof(flowTotal$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])

plot(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flowTotal$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)


#Goodness of fit and plot for flowTotal and lamar tower data
gof(flow$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])

plot(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flow$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)




#Goodness of fit and plot for flowDim and lamar tower data
gof(flowDim$qOut[,"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])

plot(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flowDim$qOut[,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)




# Compaing total vs only surface runnoff with simple additive routing
plot(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flowTotal$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
lines(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flow$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Total vs. Surface Runoff")
legend(a'topleft', c("Lamar Tower Gauge", "LPJ-Guess Total Runoff", "LPJ-Guess Surface Runoff"), col=c("black","red","blue"), lty=1)


plot(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowTotal$qOut[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")])
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qOut[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Total vs. Surface Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Total Runoff", "LPJ-Guess Surface Runoff"), col=c("black","red","blue"), lty=1)

RsVtotal <- cbind(
    gof(flow$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flowTotal$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
)
colnames(RsVtotal) <- c("Surface", "Total")
RsVtotal


# Compaing total vs only surface runnoff with simple additive routing for Soda Butte
plot(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flowTotal$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"], type="l",col="red", ylim=c(0,30), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")]), sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")], lwd=c(1,1,1))
lines(dates[which(dates == "1999-01-01"):which(dates == "2012-12-31")], flow$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Total vs. Surface Runoff for Soda Butte")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess Total Runoff", "LPJ-Guess Surface Runoff"), col=c("black","red","blue"), lty=1)


plot(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowTotal$qIn[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60298"], type="l",col="red", ylim=c(0,30), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime[which(sodaButteQ$datetime == "2009-01-01"):which(sodaButteQ$datetime == "2012-12-31")]), sodaButteQ$Q[which(sodaButteQ$datetime == "2009-01-01"):which(sodaButteQ$datetime == "2012-12-31")], lwd=2)
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qIn[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60298"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Total vs. Surface Runoff for Soda Butte")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess Total Runoff", "LPJ-Guess Surface Runoff"), col=c("black","red","blue"), lty=1)

RsVtotal <- cbind(
    gof(flow$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"], sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")]),
    gof(flowTotal$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"], sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")])
)
colnames(RsVtotal) <- c("Surface", "Total")
RsVtotal





# Comparing flow and flowDim
plot(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowDim$qOut[3651:5110,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")])
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qOut[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Surface Runoff vs. Simple Storage")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Surface With Storage", "LPJ-Guess Surface Without Storage"), col=c("black","red"), lty=1)




# Comparing flow and flowDim
plot(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowDim1.5$qOut[3651:5110,"60960"], type="l",col="yellow", ylim=c(0,350), ylab="M3/s")
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowDim$qOut[3651:5110,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")])
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qOut[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Surface Runoff vs. Simple Storage")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Surface With Storage", "LPJ-Guess Surface Without Storage"), col=c("black","red"), lty=1)




# Comparing flow and flowDim for Soda butte
plot(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowDim$qIn[3651:5110,"60298"], type="l",col="red", ylim=c(0,30), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime[which(sodaButteQ$datetime == "2009-01-01"):which(sodaButteQ$datetime == "2012-12-31")]), sodaButteQ$Q[which(sodaButteQ$datetime == "2009-01-01"):which(sodaButteQ$datetime == "2012-12-31")])
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qIn[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60298"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Surface Runoff vs. Simple Storage for Soda Butte")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess Surface With Storage", "LPJ-Guess Surface Without Storage"), col=c("black","red"), lty=1)

























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


