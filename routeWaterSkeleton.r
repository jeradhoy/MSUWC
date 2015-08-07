##########################
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
options(scipen=999)

## Set directory locations
ncdir <- ""
sdir <- ""

catchmentFileDir <- ""
catchmentFileName <- ""
edgeFileDir <- ""
edgeFileName <- ""

ncName <- ""

nwisGaugeDir <- ""
nwisGaugeFname <- ""

## Set script options

# Set huc10 codes for routing, need to include headwater huc's, input as number not text
hucCodes <- c()
timeStep <- "" # "monthly" or "daily"
surfaceVarName <- ""
subsurfVarName <- ""
simStartDate <- ""

streamWidthCoeffs <- c()
manningN <- .05
slopeMin <- .01
aCoeffCoeff <- 3


# Set the names of the fields to be used
orderVar <- ""
lengthVar <- ""
nextDownVar <- ""
areaVar <- ""
hydroIdVar <- ""
slopeVar <- ""



# Load scripts
source(paste(sdir, "routingFunctions.r", sep=""))
#source(paste(sdir, "notifyMe.r", sep=""))

# Read in edges and catchments
catchments <- readOGR(catchmentFileDir, catchmentFileName, stringsAsFactors=F)
edges <- readOGR(edgeFileDir, edgeFileName, stringsAsFactors=F)

# Subset edges and catchments
catchmentsInBounds <- GetCatchInBounds(catchments, hucCodes)
edgesInBounds <- edges[edges$DrainID %in% as.numeric(catchmentsInBounds$HydroID),]


# Generate Runoff
surfaceRunoff <- AggregateRunoff(ncFile=paste(ncdir, ncName, sep=""), catchmentPolygons=catchmentsInBounds, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep) 

subsurfRunoff <- AggregateRunoff(ncFile=paste(ncdir, ncName, sep=""), catchmentPolygons=catchmentsInBounds, runoffVar=subsurfVarName, startDate=simStartDate, by=timeStep) 


# Route Water
flow <- RouteWater(edges=edgesInBounds, Rsurf=surfaceRunoff, Rsub=subsurfRunoff, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)


# Read in gauge data
gaugeData <- GetGaugeData(edgesInBounds, paste(nwisGaugeDir, nwisGaugeFname, sep=""))

# Create graphs and do analysis
dates <- as.Date(colnames(flow$qOut[,1])
plot(dates, flow$qOut[names(gaugeData)[1],], )
lines(as.Date(gaugeDate[names(gaugeData)[1], "datetime"]), gaugeDate[names(gaugeData)[1],"Q"], )
