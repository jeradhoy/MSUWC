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
library(devtools)
load_all("msuwcRouting")

options(scipen=999)
#.pardefault <- par()
#par(mar=c(5,4,4,2)+.1)


setupDefaults <- read.delim("routingDefaults.tsv", stringsAsFactors=F, header=F, comment.char="#")
setupDefaults
setupList <- as.list(setupDefaults[,2])
names(setupList) <- setupDefaults[,1]
rm(setupDefaults)
setupList
for(i in 1:length(setupList)){
    if(!is.na(as.numeric(setupList[[i]]))){
		setupList[[i]] <- as.numeric(setupList[[i]])
    }
    if(!is.na(as.logical(setupList[[i]]))){
		setupList[[i]] <- as.logical(setupList[[i]])
    }
    if(substr(setupList[[i]], 1, 2) == "c("){
		setupList[[i]] <- eval(parse(text=setupList[[i]]))
    }
}

setupList

###########
# Set directory and file locations
###########
load_all("msuwcRouting")

# Catchment polygons shapefile created with ArcHydro
# Refer to arcHydroNotes.docx for instructions on how to create
catchmentFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Cathments_Clipped/"
catchmentFileName <- "GYE_Cathments_Clipped" 

# DrainageLine shapefile created with ArcHydro 
# Refer to arcHydroNotes.docx for instructions on how to create
edgeFileDir <- "/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_DrainageLine2"
edgeFileName <- "GYE_DrainageLine2"

# LPJ-Guess outputs of surface and subsurface runoff
ncdir <- "/Users/hoy/Desktop/MSUWC/Data/DriverData/Output_GYE_RoughCut_2050/"
surfaceNcName <- "GYE_RoughCut_2050_stand_monthly_msro.nc"
subNcName <- "GYE_RoughCut_2050_stand_monthly_mssro.nc"
snowpackNcName <- "GYE_RoughCut_2050_stand_monthly_spack.nc"
precipNcName  <- "GYE_RoughCut_2050_stand_monthly_prcp.nc"

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
simEndDate <- "2050-12-31"

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

aggregatePrecip <- F

# If you want to select by HUC10 code, set to T
# If you want to selct by edgeID, set to F
subsetEdgesCatchs <- T
selectByHuc <- F
hucCodes <- as.numeric(as.character(read.dbf("/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Huc10_Basins.dbf")$HUC10))
hucCodes <- c(1007000105, 1007000106)

# If selecting by edgeID, fill out, otherwise, continue
edgeIds <- c(16716, 23917)
edgeIds <- c(21647)

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

#edgesInBounds <- edges[edges$HUC10 %in% as.numeric(as.character(read.dbf("/Users/hoy/Desktop/MSUWC/Data/Shapefiles/GYE_Huc10_Basins.dbf")$HUC10)),]
#edgesInBounds <- edges


if(subsetEdgesCatchs){
    if(selectByHuc){
	# Subset edges and catchments
	edgesInBounds <- GetShapesInBounds(edges, hucCodes)
    } else {
	#Can also subset by edge or catchment ID's if hucSelection isn't working as desired
	edgesInBounds <- GetShapesById(edges, edgeIds)
    }
} else {
    edgesInBounds <- edges
}

#### Temporary quick fix for GYE IOE run
catchmentsInBounds <- catchments
edgesInBounds <- edges[edges@data[, edgeIdField] %in% as.numeric(catchmentsInBounds@data[, catchIdField]),]
###################

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

    surfaceRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep) 

    notifyMe("GYE Runoff finished aggregating")
    subsurfRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  subNcName, sep=""), catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=subsurfVarName, startDate=simStartDate, by=timeStep) 
    notifyMe("GYE Subsurface Runoff finished aggregating")

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
aCoeffCoeff <- 40
manningN <- .1
gwSpinUpCycles <- 0
spinUpYears <- 10


flow <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, spinUpYears=spinUpYears, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)



# Read in gauge data
if(!exists("nwisGauges")){
    nwisGauges <- readOGR(nwisGaugeDir, nwisGaugeFname, stringsAsFactors=F)
}


load_all("msuwcRouting")

gaugeData <- GetGaugeData(edgesInBounds, nwisGauges, aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F)

notifyMe("All gauges processed")


makeHydrographs(flow, gaugeData, saveGraphs=T)
load_all("msuwcRouting")

gaugeData <- gaugeDataMonthly

gaugeData <- gaugeData[!is.na(names(gaugeData))]

load_all("msuwcRouting")

makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs, plotSeason=F, plotAnnual=F, plotStats=F, dataMin=100)

makeHydrographs(flow, gaugeData, plotSeason=F, plotAnnual=F, saveGraphs=saveHydrographs)

makeHydrographs(flow, gaugeData, plotSeason=F, plotAnnual=T, saveGraphs=saveHydrographs, plotStats=F)

makeHydrographs(flow, gaugeData, plotSeason=T, plotAnnual=F, saveGraphs=saveHydrographs, plotStats=F)

names(flow)
plotHydroVar(flow, gaugeData, hydroVar="v", saveGraphs=saveHydrographs)

flowToTest
gofs <- CalcGOFs(flow, gaugeData)
gofs

######
cex=1.3



makeTaylorDiagrams(flow, gaugeData)


######## Plotting statistics

gofs <- CalcGOFs(flow, gaugeData)
cbind(1:20, rownames(gofs))
statToPlot <- 11
rownames(gofs)[statToPlot]
gyeNse <- gofs[statToPlot,-c(2, 7, 19, 20)]
gyeNse <- gyeNse[order(names(gyeNse))]

meanQs <- colMeans(flow$qOut[1:396, colnames(flow$qOut) %in% names(gyeNse)])
meanQs <- meanQs[order(names(meanQs))]

names(meanQs) == names(gyeNse)

par(mfrow=c(1,1))
plot(meanQs, gyeNse, xlab="Mean Annual Discharge (m3/s)", ylab=rownames(gofs)[statToPlot], main=paste(rownames(gofs)[statToPlot], "vs. Mean Annual Discahrge"))
#abline(lm(gyeNse ~ meanQs))
plot(meanQs, pmax(gyeNse, 0), xlab="Mean Annual Discharge (m3/s)", ylab=rownames(gofs)[statToPlot], main=paste(rownames(gofs)[statToPlot], "vs. Mean Annual Discahrge"))
#abline(lm(pmax(gyeNse, 0) ~ meanQs))

par(mar=c(5,4.5,4,2)+.1)
plot(meanQs[gyeNse > 0]+1, gyeNse[gyeNse > 0], ylim=c(0,1), log="x", cex.lab=1.3, cex.main=1.5, xlab=expression(paste("Mean Annual Discharge (m"^"3", "/s)", sep="")), ylab=rownames(gofs)[statToPlot], main=paste(rownames(gofs)[statToPlot], "vs. Mean Annual Discharge"))
points(meanQs[gyeNse < 0]+1, rep(0, length(gyeNse[gyeNse < 0])), col="black", pch=19)
#abline(lm(pmax(gyeNse, 0) ~ log(meanQs)))

stop()
ku
par(mfrow=c(1,1)) 




abline(lm(pmax(gyeNse, 0) ~ meanQs))


load_all("msuwcRouting")


makeHydrographs(flow, gaugeData, saveGraphs=saveHydrographs, plotSeason=F, plotAnnual=F, plotStats=F, dataMin=100)














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







snowpackBrick <- brick(paste(ncdir, "/", snowpackNcName, sep=""), "spack")
snowpackBrick



snowpack <- get.var.ncdf(open.ncdf(paste(ncdir, "/", snowpackNcName, sep="")), "spack")
meanSnow <- apply(snowpack, MARGIN=3, mean, na.rm=T)
rm(snowpack)
dim(snowpack)
dim(meanSnow)
length(meanSnow)

precip <- get.var.ncdf(open.ncdf(paste(ncdir, "/", precipNcName, sep="")), "prcp")
meanPrecip <- apply(precip, MARGIN=3, mean, na.rm=T)
rm(precip)

msro <- get.var.ncdf(open.ncdf(paste(ncdir, "/", surfaceNcName, sep="")), "msro")
meanMsro <- apply(msro, MARGIN=3, mean, na.rm=T)
rm(msro)

mssro <- get.var.ncdf(open.ncdf(paste(ncdir, "/", subNcName, sep="")), "mssro")
meanMssro <- apply(mssro, MARGIN=3, mean, na.rm=T)
rm(mssro)
notifyMe("Done meanming mssro")

plot(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanSnow, type="l", col="cyan", ylab="Mean Monthly Water (mm/m2)", main="GYE LPJ-Guess Mean Monthly Outputs", xlab=NA)
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanPrecip, type="l", col="blue")
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanMsro, type="l", col="red")
lines(seq(as.Date(simStartDate), as.Date(simEndDate), by="month"), meanMssro, type="l", col="black")
legend("topright", lty=1, col=c("cyan", "blue", "red", "black"), legend=c("Snowpack", "Precipitation", "Surface Runoff", "Subsurface Runoff"))

























load_all("msuwcRouting")

runoff <- AggregateRunoffSnow(ncFileName=surfaceNcName, ncDir=ncdir, snowFile=snowpackNcName, catchmentPolygons=catchmentsToUse, useWeights=T, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep) 





write.table(surfaceRunoff, file="surfaceRunoff.tsv", sep="\t")
runoff2 <- read.delim("surfaceRunoff.tsv")
colnames(runoff2) <- substring(colnames(runoff2), 2)
surfaceRunoff[1:10, 1:10]
runoff2[1:10, 1:10]
surfaceRunoff[1:10, 1:10] == runoff2[1:10, 1:10]




sort( sapply(ls(),function(x){object.size(get(x))}))



edges2 <- edgesInBounds@data

N  <- 1000
mean(replicate(1000, system.time(flow$qOut[1, edges2[edges2[, edgeNextDownField] == edges2[1, edgeIdField], edgeIdField]])[3], trimmed=0.05))
