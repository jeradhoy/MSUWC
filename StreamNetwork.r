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
library(rgdal)
library(ncdf)
library(raster)
options(scipen=999)
library(mailR)
library(plotrix)
library(hydroGOF)

#Source in scripts for routing functions and notifier
source("notifyMe.r")
source("routingFunctions.r")

############
#
# Generate Runoff Tables
#
############

#Generate runoff from merged lpj-outputs
monthlyLamarDaymetSurfaceRunoff <- AggregateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/lamar_daymet_monthlyStand.nc", catchmentsInBounds, "msro", startDate="1980-01-01",by="month")

monthlyLamarDaymetSubRunoff <- AggregateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/lamar_daymet_monthlyStand.nc", catchmentsInBounds, "mssro", startDate="1980-01-01",by="month")


dailyLamarTopoWxRunoff <- generateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/Lamar_TopoWx_stand_daily.nc", catchmentsInBounds, "dsro", "dailyLamarTopoWxRunoff")
notifyMe("Finished Routing Daily TopoWx Runoff")


#Read in surface and subsurface runoff
source("routingFunctions.r")
surfRunoff <- AggregateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/lamar_daymet_dailyStand.nc", catchmentsInBounds, "dsro")

subRunoff <- AggregateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/lamar_daymet_dailyStand.nc", catchmentsInBounds, "dssro")

topoWxSurfRunoff <- AggregateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/Lamar_TopoWx_stand_daily.nc", catchmentsInBounds, "dsro")

topoWxSubRunoff <- AggregateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/Lamar_TopoWx_stand_daily.nc", catchmentsInBounds, "dssro")
notifyMe("Done aggregating topoWx runoff")

dailySumSub <- apply(get.var.ncdf(lamarNC, "dssro"), 3, sum, na.rm=T)/1000*1000000/(24*60*60)
dailySumSurf <- apply(get.var.ncdf(lamarNC, "dsro"), 3, sum, na.rm=T)/1000*1000000/(24*60*60)
dailySumTotal <- apply(get.var.ncdf(lamarNC, "dro"), 3, sum, na.rm=T)/1000*1000000/(24*60*60)

#runoffTotal <- generateRunoff("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/merged.nc", catchmentsInBounds, "dro")


#############
#
# Read in Catchments and edges, subset to get edges in bounds
#
#############

#Read in catchments, generate catchments in bounds by HUC10 codes
catchments <- readOGR("/Users/hoy/Desktop/MSUWC/Data/Catchments", "Catchments", stringsAsFactors=FALSE)
catchmentsInBounds <- getCatchInBounds(catchments, c(1007000105, 1007000106))

GetCatchInBounds(catchments, c(1007000105))
GetCatchInBounds(catchments, c(1007000106))
GetCatchInBounds(catchments, c(1007000105, 1007000106))

#Read in edges, subset with names of runoff (could use catchmentsInBounds too)
hydroEdges <- readOGR("/Users/hoy/Desktop/MSUWC/Data/hydroEdge2", "hydroEdge2")

#Correct truncated hydroedge names
hydroNames <- names(hydroEdges)
hydroNames[2] <- "Shape_Length"
hydroNames[4] <- "DrainID"
names(hydroEdges) <- hydroNames
names(hydroEdges)


#Get edges in bounds of the catchment were working with
edgesInBounds <- hydroEdges[hydroEdges$DrainID %in% as.numeric(catchmentsInBounds$HydroID),]


#Assign reach length in KM
edgesInBounds$LengthKM <- edgesInBounds$Shape_Length*120
#Assign contributing area to edges
edgesInBounds <- AssignContribArea(edgesInBounds)
#Assigning bank-full depth, using a = .1 and b = .4 from Li paper
edgesInBounds <- AssignBfDepth(edgesInBounds, .1, .4)
#Assigning bank-full width, using a = .1 and b = .6 from Li paper
edgesInBounds <- AssignBfWidth(edgesInBounds, .3, .6)
edgesInBounds$bfWidth
#Correct slopes that are negative or 0
slopes <- edgesInBounds$SLOPE/120000
slopes[slopes <= 0] <- median(edgesInBounds$SLOPE2)
edgesInBounds$SLOPE2 <- slopes

median(edgesInBounds$SLOPE)
hist(edgesInBounds$SLOPE/120000, breaks=20)

edgesInBounds <- AssignAcoeff(edgesInBounds, 3)
names(edgesInBounds)


plot(catchmentsInBounds, lwd=.5)
lines(edgesInBounds, col="blue", lwd=2)
plot(edgesInBounds, col="blue", lwd=2)


#############
#
# Route Water through edges
#
#############



#flowTotal <- RouteWater(edgesInBounds, runoffTotal)


#Route water with edges and runoff
source("routingFunctions.r")
startDate <- "2010-01-01"
endDate <- "2012-12-31"


flow <- RouteWater(edgesInBounds, surfRunoff[which(dates == startDate):which(dates == endDate), ], subRunoff[which(dates == startDate):which(dates == endDate), ], debugMode=F, widthCoeffs=c(.3, .6))
flow2 <- RouteWater(edgesInBounds, surfRunoff[which(dates == startDate):which(dates == endDate), ], subRunoff[which(dates == startDate):which(dates == endDate), ], debugMode=F, widthCoeffs=c(.6, .5))

plot(dates[which(dates == startDate):which(dates == endDate)],flow$v[,"60960"], type="l", col="blue", ylab="Flow (m/s)")
lines(dates[which(dates == startDate):which(dates == endDate)],flow2$v[,"60960"], type="l", col="blue", ylab="Flow (m/s)")
plot(dates[which(dates == startDate):which(dates == endDate)],flow$v[,"59952"], type="l", col="blue", ylab="Flow (m/s)")
plot(dates[which(dates == startDate):which(dates == endDate)],flow$qOut[,"59952"], type="l", col="blue", ylab="Flow (m/s)")
abline(0, 0) 
abline(.29641, 0) 


flowMonthlyDaymet <- RouteWater(edgesInBounds, monthlyLamarDaymetSurfaceRunoff, monthlyLamarDaymetSubRunoff, by="month")
flowMonthlyDaymet2 <- RouteWater(edgesInBounds, monthlyLamarDaymetSurfaceRunoff, monthlyLamarDaymetSubRunoff, by="month")


source("routingFunctions.r")
flowTopoWx <- RouteWater(edgesInBounds, topoWxSurfRunoff, topoWxSubRunoff, debugMode=F)
flowDaymet <- RouteWater(edgesInBounds, surfRunoff, subRunoff, debugMode=F)
notifyMe("Done Routing stuff")

plot(dates, flow$qOut[,"60960"], type="l", col="red", ylab="Flow (m/s)")
plot(dates, flowTopoWx$qOut[,"60960"], type="l", col="red", ylab="Flow (m/s)")
plot(dates, flowDaymet$qOut[,"60960"], type="l", col="red", ylab="Flow (m/s)")
plot(dates, flowDaymet$v[,"60960"], type="l", col="red", ylab="Flow (m/s)")
lines(dates[which(dates == startDate):which(dates == endDate)],flowDimDaymet$qOut[,"60960"], type="l", col="blue", ylab="Flow (m/s)")
#lines(lamarQ$datetime[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)], lamarQ$Q[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)])
lines(lamarQ$datetime, lamarQ$Q)
abline(3,0)
legend("topright", lty=1, legend=c("Routed Daymet Flow", "Lamar Tower Gauge"), col=c("red", "black"))

cbind(
gof(flow$qOut[which(dates == startDate):which(dates == endDate),"60960"], lamarQ$Q[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)]) ,
gof(flowTopoWx$qOut[which(dates == startDate):which(dates == endDate),"60960"], lamarQ$Q[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)]) 
)

plot(flow$sSub[,"60960"], type="l", col="red")

length(which(dates == startDate):which(dates == endDate))




plot(dates[which(dates == startDate):which(dates == endDate)],flowTopoWx$qOut[,"60960"], type="l", col="red", ylab="Flow (m/s)")
lines(dates[which(dates == startDate):which(dates == endDate)],flowDimDaymet$qOut[,"60960"], type="l", col="blue", ylab="Flow (m/s)")
lines(dates[which(dates == startDate):which(dates == endDate)],flow$qOut[,"60960"], type="l", col="blue", ylab="Flow (m/s)")
#lines(lamarQ$datetime[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)], lamarQ$Q[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)])
lines(lamarQ$datetime, lamarQ$Q)
abline(3,0)
legend("topleft", lty=1, legend=c("Routed TopoWx Flow", "Routed Daymet Flow", "Lamar Tower Gauge"), col=c("red", "blue", "black"))
gof(flow$qOut[,"60960"], lamarQ$Q[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)]) 



#############
#
# Read in other data such as Gauges, snowfall, precip
#
#############

# Read in snowpack data
lamarNC <- open.ncdf("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/merged.nc")
snowpack <- get.var.ncdf(lamarNC, "spack")
dailySpack <- apply(snowpack, 3, sum, na.rm=T)
dailySnowpack <- dailySpack/1000*1000000/(24*60*60)
plot(as.Date(dates), dailySnowpack, type="l")


# Read in snowpack data
topoWxSnowpack <- apply(get.var.ncdf(open.ncdf("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/Lamar_TopoWx_stand_daily.nc"), "spack"), 3, sum, na.rm=T)/1000*1000000/(24*60*60)
daymetSnowpack <- apply(get.var.ncdf(open.ncdf("/Users/hoy/Desktop/MSUWC/Data/Output_Lamar_Runoff/lamar_daymet_dailyStand.nc"), "spack"), 3, sum, na.rm=T)/1000*1000000/(24*60*60)
plot(as.Date(dates), topoWxSnowpack, type="l", col="red")
lines(as.Date(dates), daymetSnowpack, type="l", col="blue")
legend("topleft", lty=1, col=c("blue", "red"), legend=c("Daymet Snowpack", "TopoWx Snowpack"))




# Read in precipitation data
lamarPrecip <- open.ncdf("/Users/hoy/Desktop/MSUWC/Data/DriverData/Daymet/prcp_gye_1980_2012_daily_latlon_monthly.nc") 
dailyPrecip <- apply(get.var.ncdf(lamarPrecip, "prcp"), 3, sum, na.rm=T)
monthDates <- seq(as.Date("1980/1/1"), as.Date("2012/12/31"), by="month")
plot(monthDates, dailyPrecip, type="l")

#Reading in Soda Butted gauge data and processing
sodaButteQ <- read.table("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06187915&period=P10000000W&parameterCd=00060", header=TRUE, stringsAsFactors = FALSE)
sodaButteQ <- sodaButteQ[-c(1),]
sodaButteQ <- sodaButteQ[-c(grep("02-29", sodaButteQ$datetime)),]
colnames(sodaButteQ)  <- c("agency_cd", "site_no", "datetime", "Q", "Qcd")
sodaButteQ$Q[sodaButteQ$Q == "Ice"] <- 0
sodaButteQ$Q <- as.numeric(sodaButteQ$Q)/35.3146666666666666666666666666666666667

#Read in lamarTowerQ data, and process
lamarQ <- read.table("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=06188000&period=P10000000W&parameterCd=00060", header=TRUE, stringsAsFactors = FALSE)
lamarQ <- lamarQ[-c(1),]
lamarQ <- lamarQ[-c(grep("02-29", lamarQ$datetime)),]
lamarQ[,3] <- as.Date(lamarQ[,3])
colnames(lamarQ)  <- c("agency_cd", "site_no", "datetime", "Q", "Qcd")
lamarQ$Q[lamarQ$Q == "Ice"] <- 0
lamarQ$Q <- as.numeric(lamarQ$Q)/35.3146666666666666666666666666666666667 #Converting ft3 to m3



#############
#
# Data Exploration and doing comparisons
#
#############


edgesInBounds[edgesInBounds$HydroID_1 == "60298",]$LengthKM
edgesInBounds[edgesInBounds$HydroID_1 == "60960",]$LengthKM

#Getting contributing area for two gauge edges
edgesInBounds[edgesInBounds$HydroID_1 == "60298",]$ContribArea
edgesInBounds[edgesInBounds$HydroID_1 == "60960",]$ContribArea



## Trying unsuccessfully to assign dated rownames to flow tables
dates <- seq(as.Date("1980/1/1"), as.Date("2012/12/31"), by="day")
dates <- dates[c(-grep("02-29", dates))]
length(dates)
rownames(flow$qIn) <- dates 
rownames(flow$qOut) <- dates


#Initial plotting to validate everthing worked
plot(as.Date(dates), flowTotal$qIn[,"60298"], type="l")
plot(as.Date(dates), flowTotal$qOut[,"60960"], type="l")

#Initial plotting to validate everthing worked
lines(as.Date(dates), flowDimTopoWx$qIn[,"60298"], type="l", col="red")
plot(as.Date(dates), flowDim$qOut[,"60298"], type="l")



plot(rowSums(runoff), type="l")
lines(rowSums(dailyLamarTopoWxRunoff), col="red")
legend("topleft", col=c("black", "red"), legend=c("Total Lamar Surface Runoff w/ Daymet", "Total Lamar Surface Runoff w/ TopoWx"))

rbind(summary(rowSums(runoff)),
summary(rowSums(dailyLamarTopoWxRunoff)))

plot(as.Date(dates), flowDimTopoWx$qOut[,"60960"], type="l", col="red")
lines(as.Date(dates),flowDimDaymet$qOut[,"60960"], type="l")
lines(as.Date(dates),flow$qOut[,"60960"], type="l", col="orange")

lines(as.Date(dates),flowTopoWx$qOut[,"60960"], type="l", col="orange")

short.date = strftime(lamarQ$datetime, "%Y/%m")
aggr.stat = aggregate(lamarQ$Q ~ short.date, FUN = sum)
aggr.stat

startDate <- "2003-1-1"
endDate <- "2006-1-1"

lamarQ$Q[which(lamarQ$datetime == "1980-01-01"):which(lamarQ$datetime == "2012-12-31")]
plot(lamarQ$Q, type="l")

xlimVals <- c(as.Date("2003-1-1"), as.Date("2006-1-1"))
ylimVals <- c(1,150)

plot(as.Date(dates),flowDimDaymet$qOut[,"60960"], type="l", xlim=xlimVals, ylim=ylimVals, col="red")
lines(as.Date(dates),flowDaymet$qOut[,"60960"], type="l", col="blue")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
par(new=T)

plot(as.Date(dates),flowDimDaymet$v[,"60960"], type="l", xlim=xlimVals, col="red")
plot(as.Date(dates),flowDimDaymet$h[,"60960"], type="l", xlim=xlimVals, col="red")
hist(flowDimDaymet$h[,"60961"], labels=T)

plot(flowDimDaymet$v[1:365,"60960"])
plot(flowDimDaymet$v[1:365,"60298"])
which(flowDimDaymet$v[1:365,"60960"] == 3)
dev.off()
j
#############
#
# Creating actual plots for presentation and viewing
#
#############

#Initial plotting to validate everthing worked
plot(as.Date(dates), flow$qIn[,"60298"], type="l")
plot(as.Date(dates), flow$qOut[,"60960"], type="l")


###### To put daily snowcover and monthly precip into graphs
par(new=T)
plot(as.Date(dates), dailySnowpack, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
axis(4)
par(new=T)
plot(monthDates, dailyPrecip, type="l", col="blue", lty=3, lwd=2, axes=F, xlab=NA, ylab=NA)

#### Lamar Tower Hydrographs

xlimVals <- as.Date(c("1990-1-1","2012-1-1"))

#Just Regular hydrograph with lamar tower and guage
plot(as.Date(dates),flowDimDaymet$qOut[,"60960"], type="l",col="red", xlim=xlimVals, ylim=c(0,350), ylab="M3/s", xlab="Years")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Routed Runoff"), col=c("black","red"), lty=1)


xlimVals <- as.Date(c("2005-1-1","2010-1-1"))

#Same as above, zoomed in
plot(as.Date(dates),flowDimDaymet$qOut[,"60960"], type="l",col="red", xlim=xlimVals, ylim=c(0,350), ylab="M3/s", xlab="Years")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Routed Runoff"), col=c("black","red"), lty=1)


xlimVals <- as.Date(c("2005-1-1","2010-1-1"))

plot(as.Date(dates),flowDimDaymet$qOut[,"60960"], type="l",col="red", xlim=xlimVals, ylim=c(0,350), ylab="M3/s", xlab="Years")
lines(as.Date(lamarQ$datetime), lamarQ$Q)
title("Lamar Tower Gauge vs LPJ-GUESS Runoff")

par(new=T)
plot(as.Date(dates), dailySnowpack, xlim=xlimVals, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
#axis(4)
par(new=T)
plot(monthDates, dailyPrecip, xlim=xlimVals, type="l", col="blue", lty=3, lwd=2, axes=F, xlab=NA, ylab=NA)
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Routed Runoff", "LPJ-Guess Snowpack", "LPJ-Guess Monthly Precip"), col=c("black","red", "cyan", "blue"), lty=c(1,1,3,3))

###Lamar Tower velocity and height

xlimVals <- as.Date(c("2005-1-1","2012-1-1"))

plot(as.Date(dates),flowDimDaymet$v[,"60960"], type="l", xlim=xlimVals, col="black", ylab="Velocity (m/s)", xlab="Years")
title("Lamar Tower Stream Velocity, and Height")
par(new=T)
plot(as.Date(dates),flowDimDaymet$h[,"60960"], xlab=NA, ylab=NA, type="l", xlim=xlimVals, col="red", axes=F)
axis(4)
legend("topright", legend=c("Velocity", "Height"), col=c("black", "red"), lty=1)


hist(flowDimDaymet$h[,"60961"], labels=T)

###Plot of summed surface, subsurface, and total runoff


xlimVals <- as.Date(c("2010-4-1","2010-10-1"))

plot(as.Date(dates),dailySumSurf, type="l",col="black", xlim=xlimVals, ylab="M3/s", xlab="Years")
lines(as.Date(dates),dailySumSub, col="red")
lines(as.Date(dates),flowDimDaymet$qOut[,"60960"], col="red")
#lines(as.Date(dates),dailySumTotal)
#lines(as.Date(lamarQ$datetime), lamarQ$Q)
par(new=T)
plot(as.Date(dates), dailySnowpack, xlim=xlimVals, col="cyan", type="l", lty=3, lwd=2,axes=F, xlab=NA, ylab=NA)
#axis(4)
par(new=T)
plot(monthDates, dailyPrecip, xlim=xlimVals, type="l", col="blue", lty=3, lwd=2, axes=F, xlab=NA, ylab=NA)
title("LPJ-Guess Outputs")
legend('topleft', c("Total Lamar Surface Runoff", "Total Lamar Subsurface Runoff", "Snowpack", "Monthly Precip"), col=c("black","red", "cyan", "blue"), lty=c(1,1,3,3))

xlimVals <- as.Date(c("2010-4-1","2010-10-1"))

#Comparing totalled runnoff to routed runoff to see difference with routing
plot(as.Date(dates),dailySumSurf, type="l",col="black", xlim=xlimVals, ylab="M3/s", xlab="Years")
lines(as.Date(dates),flowDimDaymet$qOut[,"60960"], col="red")
title("Totaled Surface Runoff vs Lamar Tower Reach Outflow")
legend('topleft', c("Total Lamar Surface Runoff", "Routed Lamar Tower Reach Runoff"), col=c("black","red"), lty=c(1,1))

#### Soda Butte Hydrographs

xlimVals <- as.Date(c("1999-1-1","2012-1-1"))

#Just Regular hydrograph with lamar tower and guage
plot(as.Date(dates),flowDimDaymet$qOut[,"60298"], type="l",col="red", xlim=xlimVals, ylim=c(0,30), ylab="M3/s", xlab="Years")
lines(as.Date(sodaButteQ$datetime), sodaButteQ$Q)
title("Soda Butte Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess Routed Runoff"), col=c("black","red"), lty=1)


xlimVals <- as.Date(c("2005-1-1","2010-1-1"))

#Same as above, zoomed in
plot(as.Date(dates),flowDimDaymet$qOut[,"60298"], type="l",col="red", xlim=xlimVals, ylim=c(0,30), ylab="M3/s", xlab="Years")
lines(as.Date(sodaButteQ$datetime), sodaButteQ$Q)
title("Soda Butte Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess Routed Runoff"), col=c("black","red"), lty=1)



###Soda Butte velocity and height

xlimVals <- as.Date(c("2005-1-1","2012-1-1"))

plot(as.Date(dates),flowDimDaymet$v[,"60298"], type="l", xlim=xlimVals, col="black", ylab="Velocity (m/s)", xlab="Years")
title("Soda Butte Stream Velocity, and Height")
par(new=T)
plot(as.Date(dates),flowDimDaymet$h[,"60298"], xlab=NA, ylab=NA, type="l", xlim=xlimVals, col="red", axes=F)
axis(4)
legend("topright", legend=c("Velocity", "Height"), col=c("black", "red"), lty=1)





#### Taylor Diagrams

taylor.diagram(
	       lamarQ$Q[which(lamarQ$datetime == "1990-01-01"):which(lamarQ$datetime == endDate)],
	       flowDimDaymet$qOut[which(dates == "1990-01-01"):which(dates == endDate),"60960"], col="darkblue")

taylor.diagram(
	       sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == endDate)],
	       flowDimDaymet$qIn[which(dates == "1999-01-01"):which(dates == endDate),"60298"], add=TRUE, col="blue")

taylor.diagram(
	       lamarQ$Q[which(lamarQ$datetime == "1990-01-01"):which(lamarQ$datetime == endDate)],
	       flowDim$qOut[which(dates == "1990-01-01"):which(dates == endDate),"60960"], add=TRUE, col="red", pch=17)

taylor.diagram(
	       sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == endDate)],
	       flowDim$qIn[which(dates == "1999-01-01"):which(dates == endDate),"60298"], add=TRUE, col="darkred", pch=17)

legend(60,100, col=c(NA, "darkblue", "blue", NA, "darkred", "red"),pch=c(NA, 19,19,NA,17,17), 
       legend = c(expression(bold("Without Variable Parameter Routing")), "Lamar Tower", "Soda Butte", expression(bold("With Variable Parameter Routing")), "Lamar Tower", "Soda Butte"))




#### Goodness of fit analysis


cbind(
    gof(flowTotalDim$qOut[,"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flowTotal$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flowDaymet$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flowDimDaymet$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
)


    gof(flow$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])



daymetNSE <- cbind(
    gof(flowDaymet$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")]),
    gof(flowDimDaymet$qOut[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60960"], lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == "2012-12-31")])
)

colnames(daymetNSE) <- c("w/o routing", "w/ routing")
daymetNSE


daymetNSE <- cbind(
    gof(flowDaymet$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"],sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")]),
    gof(flowDimDaymet$qIn[which(dates == "1999-01-01"):which(dates == "2012-12-31"),"60298"],sodaButteQ$Q[which(sodaButteQ$datetime == "1999-01-01"):which(sodaButteQ$datetime == "2012-12-31")])
)

colnames(daymetNSE) <- c("w/o routing", "w/ routing")
daymetNSE














# Comparing flow and flowDim
plot(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowDim1.5$qOut[3651:5110,"60960"], type="l",col="yellow", ylim=c(0,350), ylab="M3/s")
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowDim$qOut[3651:5110,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")])
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qOut[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Surface Runoff vs. Simple Storage")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Surface With Storage", "LPJ-Guess Surface Without Storage"), col=c("black","red"), lty=1)




# Comparing flow and flowDim for Soda butte
plot(dates[which(dates == "2010-01-01"):which(dates == "2012-12-31")], flowDim$qIn[3651:5110,"60298"], type="l",col="red", ylim=c(0,30), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime[which(sodaButteQ$datetime == "2009-01-01"):which(sodaButteQ$datetime == "2012-12-31")]), sodaButteQ$Q[which(sodaButteQ$datetime == "2009-01-01"):which(sodaButteQ$datetime == "2012-12-31")])
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qIn[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60298"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Surface Runoff vs. Simple Storage for Soda Butte")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess Surface With Storage", "LPJ-Guess Surface Without Storage"), col=c("black","red"), lty=1)







plot(as.Date(lamarQ$datetime[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)]), flowDim$qOut[,"60960"],type="l") 
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)]), flow2$qOut[,"60960"], type="l", col="red") 

lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)]), lamarQ$Q[which(lamarQ$datetime == startDate):which(lamarQ$datetime == endDate)], col="blue")
par(new=T)
plot(flowDim$h[, "60960"], type="l")
plot(flowDim$v[, "60960"], type="l")

par(new=T)
plot(as.Date(dates), dailySnowpack, type="l")


plot(dates[which(dates == "1999-01-01"):which(dates == endDate)], flowDim$qOut[,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == endDate)]), lamarQ$Q[which(lamarQ$datetime == "1999-01-01"):which(lamarQ$datetime == endDate)])


plot(flow2$qIn[,"60298"]*30, type="l",ylim=c(0,20))
lines(flow$qIn[1:365,"60298"], type="l")
lines(flow2$sRiv[,"60298"]*30, type="l")

plot(flow2$qOut[,"60960"]*30, type="l")
lines(flow$qOut[1:365,"60960"], type="l")
lines(flow2$sRiv[,"60960"]*30, type="l")










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



#Plotting soda butte data with data from qIn of edge 60298
plot(as.Date(dates), flow$qIn[,"60298"], type="l",col="red", xlim=as.Date(c("1998-10-1","2012-1-1")), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime), sodaButteQ$Q)
title("Soda Butte Creek Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)




#
#
# Graphing and analysis of total surface runoff data
#
#
#











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


#Plotting soda butte data with data from qIn of edge 60298
plot(as.Date(dates), flowTotal$qIn[,"60298"], type="l",col="red", xlim=as.Date(c("1998-10-1","2012-1-1")), ylab="M3/s")
lines(as.Date(sodaButteQ$datetime), sodaButteQ$Q)
title("Soda Butte Creek Gauge vs LPJ-GUESS Runoff")
legend('topleft', c("Soda Butte Gauge", "LPJ-Guess"), col=c("black","red"), lty=1)










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

plot(flowDim$qOut[,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")



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






# Comparing flow and flowDim
plot(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flowDim$qOut[3651:5110,"60960"], type="l",col="red", ylim=c(0,350), ylab="M3/s")
lines(as.Date(lamarQ$datetime[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")]), lamarQ$Q[which(lamarQ$datetime == "2009-01-01"):which(lamarQ$datetime == "2012-12-31")])
lines(dates[which(dates == "2009-01-01"):which(dates == "2012-12-31")], flow$qOut[which(dates == "2009-01-01"):which(dates == "2012-12-31"),"60960"], type="l",col="blue", ylim=c(0,350), ylab="M3/s")
title("Surface Runoff vs. Simple Storage")
legend('topleft', c("Lamar Tower Gauge", "LPJ-Guess Surface With Storage", "LPJ-Guess Surface Without Storage"), col=c("black","red"), lty=1)
