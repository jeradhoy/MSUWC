########################
# Outline-Skeleton for doing all routing
#
# Created by Jerad Hoy
# Date 8/4/2015
#
#
#########################

## Load necessary packages
library(sp)
library(maptools)
library(raster)
library(rgdal)
library(ncdf)
library(raster)
library(plotrix)
library(hydroGOF)
library(RCurl)
options(scipen=999)
.pardefault <- par()


## Set directory locations
sdir <- getwd()

catchmentFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Catchments/"
catchmentFileName <- "GYE_Catchments" 
edgeFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_DrainageLine2"
edgeFileName <- "GYE_DrainageLine2"

ncdir <- "/Users/hoy/Desktop/MSUWC/Data/GYE_Daymet_Outputs"
surfaceNcName <- "GYE_Daymet_stand_monthly_msro.nc"
subNcName <- "GYE_Daymet_stand_monthly_mssro.nc"

nwisGaugeDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/nwisGauges"
nwisGaugeFname <- "NWISMapperExport"

plotDir = paste(getwd(), "/Plots", sep="")

## Set script options

# Set the names of the edge fields to be used
edgeIdField <- "DrainID"
edgeOrderField <- "RiverOrder"
edgeLengthField <- "Shape_Leng"
edgeAreaField <- "Shape_Ar_1"
edgeHucField <- "HUC10"
edgeNextDownField <- "NextDown_2"
slopeFieldDeg <- "SLOPE"


# Set the names of the catchment fields to be used
catchAreaField <- "Shape_Area"
catchIdField <- "HydroID"
catchNextDownField <- "NextDownID"
catchOrderField <- "RiverOrder"
catchHucField <- "HUC10"


# Set huc10 codes for routing, need to include headwater huc's, input as number not text
#hucCodes <- c(1008001301, 1008001302, 1008001303)
timeStep <- "month" # "month" or "day"
surfaceVarName <- "msro"
subsurfVarName <- "mssro"
simStartDate <- "1980-01-01"
simEndDate <- "2012-12-31"

streamWidthCoeffs <- c(.3, .6)
manningN <- .05
slopeMin <- .01
aCoeffCoeff <- 3
aggregateAllCatchments <- T

# For selecting huc/catchments
selectByHuc <- T
hucCodes <- c(1008000702)
edgeIds <- c()

aggregateGaugeDataByMonth <- F
saveHydrographs <- F


# Load scripts
source(paste(sdir,"/", "routingFunctions.r", sep=""))
#source(paste(sdir, "notifyMe.r", sep=""))


# Read in edges and catchments
if(!exists("catchments")){
    catchments <- readOGR(catchmentFileDir, catchmentFileName, stringsAsFactors=F)
    edges <- readOGR(edgeFileDir, edgeFileName, stringsAsFactors=F)
}

if(selectByHuc){
    # Subset edges and catchments
    edgesInBounds <- GetShapesInBounds(edges, hucCodes)
} else {
    #Can also subset by edge or catchment ID's if hucSelection isn't working as desired
    edgesInBounds <- GetShapesById(edges, edgeIds)
}

catchmentsInBounds <- catchments[catchments@data[, catchIdField] %in% as.numeric(edgesInBounds@data[, edgeIdField]),]

# Check to make sure it plotted edges you want
plot(edgesInBounds)
plot(catchmentsInBounds)
lines(edgesInBounds)

# Generate Runoff
if(aggregateAllCatchments){
    catchmentsToUse <- catchments
} else {
    catchmentsToUse <- catchmentsInBounds
}

if(!exists("surfaceRunoff")){

    surfaceRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep) 

    subsurfRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  subNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=subsurfVarName, startDate=simStartDate, by=timeStep) 

}

# Route Water
source(paste(sdir,"/", "routingFunctions.r", sep=""))
flow <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff, Rsub=subsurfRunoff, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)



# Read in gauge data
if(!exists("nwisGauges")){
    nwisGauges <- readOGR(nwisGaugeDir, nwisGaugeFname, stringsAsFactors=F)
}

source(paste(sdir,"/", "routingFunctions.r", sep=""))
gaugeData <- GetGaugeData(edgesInBounds, nwisGauges, aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=T)

#makeHydrographs(flow, gaugeData, saveGraphs=T)
makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs)



CalcGOFs(flow, gaugeData)

makeTaylorDiagrams(flow, gaugeData)
i=1
taylor.diagram(flow$qOut[, names(gaugeData)[i]], gaugeData[[i]][,2], pch=i)
taylor.diagram(flow$qOut[, names(gaugeData)[i+1]], gaugeData[[i+1]][,2], pch=i+1, add=T)

taylor.diagram(flow$qOut[, names(gaugeData)[3]], gaugeData[[3]][,2], pch=3)



flow$qOut[,1]

mean(seq(1, 10, length.out=31))

daysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)

daysInMonth[as.numeric(format(as.yearmon(rownames(flow$qOut)[row]), "%m"))]


out <- c()
for(col in 1){
    for(row in 1:(nrow(flow$qOut)-1)){
	out <- c(out, seq(flow$qOut[row, col], flow$qOut[row+1, col], length.out=daysInMonth[as.numeric(format(as.yearmon(rownames(flow$qOut)[row]), "%m"))]))
    }
	out <- c(out, seq(flow$qOut[row, col], 0, length.out=daysInMonth[as.numeric(format(as.yearmon(rownames(flow$qOut)[row]), "%m"))]+1))
    
}

plot(out, type="l")
plot(flow$qOut[,1], type="l")


i=6
seq(as.Date(as.yearmon(rownames(flow$qOut)[i])),as.Date(as.yearmon(rownames(flow$qOut)[i+1])), length.out=31)

seq(flow$qOut[i, 1],flow$qOut[i+1, 1], length.out=31)
