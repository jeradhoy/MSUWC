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
options(scipen=999)

## Set directory locations
sdir <- getwd()

catchmentFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Catchments/"
catchmentFileName <- "GYE_Catchments"
edgeFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_DrainageLine"
edgeFileName <- "GYE_DrainageLine"

ncdir <- "/Users/hoy/Desktop/MSUWC/Data/GYE_Daymet_Outputs"
surfaceNcName <- "GYE_Daymet_stand_monthly_msro.nc"
subNcName <- "GYE_Daymet_stand_monthly_mssro.nc"

nwisGaugeDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/nwisGauges"
nwisGaugeFname <- "NWISMapperExport"

## Set script options

# Set huc10 codes for routing, need to include headwater huc's, input as number not text
#hucCodes <- c(1008001301, 1008001302, 1008001303)
hucCodes <- c(1002000809)
timeStep <- "month" # "month" or "daily"
surfaceVarName <- "msro"
subsurfVarName <- "mssro"
simStartDate <- "1980-01-01"

streamWidthCoeffs <- c(.3, .6)
manningN <- .05
slopeMin <- .01
aCoeffCoeff <- 3


# Set the names of the edge fields to be used
edgeIdField <- "Drainage_2"
edgeOrderField <- "RiverOrder"
edgeLengthField <- "DrainageLi"
edgeNextDownField <- "NextDownID"
slopeFieldDeg <- "SLOPE"


# Set the names of the catchment fields to be used
catchAreaField <- "Shape_Area"
catchIdField <- "HydroID"
catchNextDownField <- "NextDownID"
catchOrderField <- "RiverOrder"
catchHucField <- "HUC10"


# Load scripts
source(paste(sdir,"/", "routingFunctions.r", sep=""))
#source(paste(sdir, "notifyMe.r", sep=""))


# Read in edges and catchments
if(!exists("catchments")){
    catchments <- readogr(catchmentfiledir, catchmentfilename, stringsasfactors=f)
    edges <- readogr(edgefiledir, edgefilename, stringsasfactors=f)
}

# Subset edges and catchments
catchmentsInBounds <- GetCatchInBounds(catchments, hucCodes)
edgesInBounds <- edges[edges@data[, edgeIdField] %in% as.numeric(catchmentsInBounds@data[, catchIdField]),]


# Generate Runoff
surfaceRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsInBounds, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep) 

allSurfaceRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchments, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep) 

subsurfRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  subNcName, sep=""), catchmentPolygons=catchmentsInBounds, runoffVar=subsurfVarName, startDate=simStartDate, by=timeStep) 

llSubsurfRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  subNcName, sep=""), catchmentPolygons=catchments, runoffVar=subsurfVarName, startDate=simStartDate, by=timeStep) 


# Route Water
flow <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff, Rsub=subsurfRunoff, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)




# Read in gauge data
if(!exists("nwisGauges")){
    nwisGauges <- readOGR(nwisGaugeDir, nwisGaugeFname, stringsAsFactors=F)
}

source(paste(sdir,"/", "routingFunctions.r", sep=""))
gaugeData <- GetGaugeData(edgesInBounds, nwisGauges)


# Create graphs and do analysis
par(mfrow=c(length(gaugeData), 1))
for(i in 1:length(gaugeData)){
    dates <- as.Date(as.yearmon(rownames(flow$qOut)))
    plot(dates, flow$qOut[, names(gaugeData)[i]], type="l", col="red")
    lines(as.Date(gaugeData[[i]][, "datetime"]), gaugeData[[i]][,4])
    abline(0, 0)
    title(gaugesInBounds@data[i,"SITENAME"])
}

