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
timeStep <- "month" # "month" or "daily"
surfaceVarName <- "msro"
subsurfVarName <- "mssro"
simStartDate <- "1980-01-01"

streamWidthCoeffs <- c(.3, .6)
manningN <- .05
slopeMin <- .01
aCoeffCoeff <- 3
aggregateAllCatchments <- T




# Load scripts
source(paste(sdir,"/", "routingFunctions.r", sep=""))
#source(paste(sdir, "notifyMe.r", sep=""))


# Read in edges and catchments
if(!exists("catchments")){
    catchments <- readOGR(catchmentFileDir, catchmentFileName, stringsAsFactors=F)
    edges <- readOGR(edgeFileDir, edgeFileName, stringsAsFactors=F)
}

# Subset edges and catchments
source(paste(sdir,"/", "routingFunctions.r", sep=""))
hucCodes <- c(1007000204)
edgesInBounds <- GetShapesInBounds(edges, hucCodes)

#Can also subset by edge or catchment ID's if hucSelection isn't working as desired
edgeIds <- c(21647)
edgesInBounds <- GetShapesById(edges, edgeIds)

catchmentsInBounds <- catchments[catchments@data[, catchIdField] %in% as.numeric(edgesInBounds@data[, edgeIdField]),]

plot(edgesInBounds)

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
gaugeData <- GetGaugeData(edgesInBounds, nwisGauges)

makeHydrographs(flow, gaugeData)
makeHydrographs(flow, gaugeData, saveGraphs=T)
makeHydrographs(flow, gaugeData, interact=T)

