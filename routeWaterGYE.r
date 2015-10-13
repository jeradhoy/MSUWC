#####################
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
library(devtools)


###########
# Set directory and file locations
###########
load_all("msuwcRouting")

# Catchment polygons shapefile created with ArcHydro
# Refer to arcHydroNotes.docx for instructions on how to create
catchmentFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Catchments/"
catchmentFileName <- "GYE_Catchments" 

# DrainageLine shapefile created with ArcHydro 
# Refer to arcHydroNotes.docx for instructions on how to create
edgeFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_DrainageLine2"
edgeFileName <- "GYE_DrainageLine2"

# LPJ-Guess outputs of surface and subsurface runoff
ncdir <- "/Users/hoy/Desktop/MSUWC/Data/DriverData/GYE_Daymet_Outputs"
surfaceNcName <- "GYE_Daymet_stand_monthly_msro.nc"
subNcName <- "GYE_Daymet_stand_monthly_mssro.nc"
snowpackNcName <- ""
precipNcName  <- ""

# NWIS gauge locations point shapefile
# Refer to arcHydroNotes.docx for instructions on how to create
nwisGaugeDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/nwisGauges"
nwisGaugeFname <- "NWISMapperExport"

# Directory where plots are to be stored
# By default, it is /Plots in the cwd
plotDir = paste(getwd(), "/Plots", sep="")
dataOutputDir = paste(getwd(), "/Outputs", sep="")

###########
# Set edge and catchment table field names
###########

# Set the names of the edge fields to be used
edgeIdField <- "DrainID"
edgeOrderField <- "RiverOrder"
edgeLengthField <- "Shape_Leng"
edgeAreaField <- "Shape_Ar_1"
edgeHucField <- "HUC10"
edgeNextDownField <- "NextDown_2"
edgeSlopeFieldDeg <- "SLOPE"


# Set the names of the catchment fields to be used
catchAreaField <- "Shape_Area"
catchIdField <- "HydroID"
catchNextDownField <- "NextDownID"
catchOrderField <- "RiverOrder"
catchHucField <- "" #unnessary if subsetting with edges

## Note: edgeIdField and catchIdField are linked and must be the same for corresponding edges and nodes


###########
# Set simulations information
##########

timeStep <- "month" # "month" or "day"
surfaceVarName <- "msro"
subsurfVarName <- "mssro"
snowpackVarName <- ""
precipVarName <- ""


simStartDate <- "1980-01-01"
simEndDate <- "2012-12-31"

##########
# Set routing parameters
##########
streamWidthCoeffs <- c(.3, .6)
manningN <- .01
slopeMin <- .01
aCoeffCoeff <- 2

#########
# Set other function parameters
#########

aggregatePrecip <- T
aggregateSnowpack <- T

# If you want to select by HUC10 code, set to T
# If you want to selct by edgeID, set to F
selectByHuc <- F
hucCodes <- c(1007000105, 1007000106)

# If selecting by edgeID, fill out, otherwise, continue
edgeIds <- c(17446)

# If you want to aggregate all runoff data at once, set to T
aggregateAllCatchments <- T

# If you want to aggregate gauge data by month, set to T
aggregateGaugeDataByMonth <- T

# If you want to save all plots as png's, set to T 
saveHydrographs <- F

########################################################
#
# Rest of script should not need configuration
# Run lines sequentially
#
# May want to CHECK data at each step to make sure it is working properly
#
#######################################################

# Load scripts
load_all("msuwcRouting")


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

#########################
# CHECK to make sure edges are subsetted properly 
#########################
plot(edgesInBounds)
plot(catchmentsInBounds, add=T)

########################
# CHECK to make sure catchments cover NetCDF
# Empty gridcells will be given value of 0, and discharge will appear to be less than actual
#######################
plot(brick(paste(ncdir, "/",  surfaceNcName, sep=""), surfaceVarName), 1, ext=extent(catchmentsInBounds)+.1, col="red")
plot(catchmentsInBounds, add=T)

#catchmentsInBounds  <- catchments

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

if(aggregatePrecip){
    if(!exists("precip")){
	precip <- AggregateRunoff(ncFile=paste(ncdir, "/", precipNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=precipVarName, startDate=simStartDate, by=timeStep) 
    }
}
if(aggregateSnowpack){
    if(!exists("snowpack")){
	snowpack <- AggregateRunoff(ncFile=paste(ncdir, "/",  snowpackNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=snowpackVarName, startDate=simStartDate, by=timeStep) 
    }
}

# Route Water
load_all("msuwcRouting")
aCoeffCoeff <- 50
manningN <- .1
gwSpinUpCycles <- 3
flow <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff, Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)



# Read in gauge data
if(!exists("nwisGauges")){
    nwisGauges <- readOGR(nwisGaugeDir, nwisGaugeFname, stringsAsFactors=F)
}

gyeGauges <- nwisGauges[nwisGauges@data[,1] %in% gaugeList,]

load_all("msuwcRouting")


gaugeData <- GetGaugeData(edgesInBounds, nwisGauges[1:10,], aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F)


gaugeDataDaily <- GetGaugeData(edges, gyeGauges, aggregateByMonth=F, checkGauges=F)
save(gaugeDataDaily, file="gyeGuageDataDaily.RData")

gaugeDataMonthly <- GetGaugeData(edges, gyeGauges, aggregateByMonth=T, checkGauges=F)
save(gaugeDataMonthly, file="gyeGuageDataMonthly.RData")



gaugeData <- GetGaugeData(edges, nwisGauges[1:1000,], aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F)
notifyMe("First 1000 gauges Processed")
gaugeData <- c(gaugeData, GetGaugeData(edges, nwisGauges[1001:2000,], aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F))
notifyMe("First 2000 gauges processed")
gaugeData <- c(gaugeData, GetGaugeData(edges, nwisGauges[2001:nrow(nwisGauges),], aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F))
notifyMe("All gauges processed")

#makeHydrographs(flow, gaugeData, saveGraphs=T)
load_all("msuwcRouting")
par(mfrow=c(1,1))
makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs)
names(flow)
plotHydroVar(flow, gaugeData, hydroVar="v", saveGraphs=saveHydrographs)

CalcGOFs(flow, gaugeData)

makeTaylorDiagrams(flow, gaugeData)






load_all("msuwcRouting")
par(mfrow=c(4,3))

makeHydrographs(flow, gaugeData[5], saveGraphs=saveHydrographs, plotSeason=1, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[5], saveGraphs=saveHydrographs, plotSeason=2, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[5], saveGraphs=saveHydrographs, plotSeason=3, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[5], saveGraphs=saveHydrographs, plotSeason=4, plotAnnual=F, plotStats=F)

makeHydrographs(flow, gaugeData[8], saveGraphs=saveHydrographs, plotSeason=1, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[8], saveGraphs=saveHydrographs, plotSeason=2, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[8], saveGraphs=saveHydrographs, plotSeason=3, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[8], saveGraphs=saveHydrographs, plotSeason=4, plotAnnual=F, plotStats=F)

makeHydrographs(flow, gaugeData[9], saveGraphs=saveHydrographs, plotSeason=1, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[9], saveGraphs=saveHydrographs, plotSeason=2, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[9], saveGraphs=saveHydrographs, plotSeason=3, plotAnnual=F, plotStats=F)
makeHydrographs(flow, gaugeData[9], saveGraphs=saveHydrographs, plotSeason=4, plotAnnual=F, plotStats=F)


makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs, plotSeason=NULL, plotAnnual=T, plotStats=F)







load_all("msuwcRouting")
    AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsInBounds, useWeights=T, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep)
    system.time(replicate(10, AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsInBounds, useWeights=T, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep)))
    system.time(replicate(10, AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsInBounds, useWeights=F, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep)))

user  system elapsed
 13.631   0.261  13.886

user  system elapsed
  9.332   0.143   9.481

#Note: takes about 1.46 times the amount of time to use weights


hrr.shp.2 <- spTransform(hrr.shp, CRS("+init=epsg:26978"))

cat2 <- spTransform(cat, CRS("+init=epsg:32612"))


catchmentsInBounds[1,]$HydroID

edgesInBounds[1,] 
ed <- edgesInBounds[edgesInBounds@data[, edgeIdField] == 20058,]
cat <- catchmentsInBounds[catchmentsInBounds@data[, catchIdField] == 20058, ]
cat2 <- spTransform(cat, CRS("+init=epsg:32612"))
ed2 <- spTransform(ed, CRS("+init=epsg:32612"))
plot(cat2)
lines(ed2)
pts <- spsample(cat2, n = 100000, type="regular")
points(pts)
fDistance(pts, ed2)
