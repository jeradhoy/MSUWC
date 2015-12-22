####################
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


setupDefaults <- read.delim("routingDefaults.tsv", sep="=", stringsAsFactors=F, header=F, comment.char="#")

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
	lamarEdges <- GetShapesById(edges, edgeIds)
    }
} else {
    edgesInBounds <- edges
}

#### Temporary quick fix for GYE IOE run
	catchmentsInBounds <- catchments
	edgesInBounds <- edges[edges@data[, edgeIdField] %in% as.numeric(catchmentsInBounds@data[, catchIdField]),]
###################

catchmentsInBounds <- catchments[catchments@data[, catchIdField] %in% as.numeric(edgesInBounds@data[, edgeIdField]),]
lamarCatch <- catchments[catchments@data[, catchIdField] %in% as.numeric(lamarEdges@data[, edgeIdField]),]

#########################
# CHECK to make sure edges are subsetted properly 
#########################
plot(edgesInBounds)
plot(catchmentsInBounds, add=T)

########################
# CHECK to make sure catchments cover NetCDF
# Empty gridcells will be given value of 0, and discharge will appear to be less than actual
#######################
plot(raster::brick(paste(ncdir, "/",  surfaceNcName, sep=""), surfaceVarName), 1, ext=raster::extent(catchmentsInBounds)+.1, col="red")
plot(catchmentsInBounds, add=T)
plot(streamPoints, add=T)


tmean <- brick("/Users/hoy/Desktop/MSUWC/Data/DriverData/GYE_Daymet_stand_monthly_tmean.nc", "tmean") 

cellStats(subset(tmean, 1:10), "mean")
runOnHyalite("tmeanStats", objs=c("tmean"), packages=c("raster"), oneLine=T)
#catchmentsInBounds  <- catchments

# Generate Runoff
if(setupList$aggregateAllCatchments){
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
gwSpinUpCycles <- 10
spinUpYears <- 10


flow <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, spinUpYears=spinUpYears, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)

runHyalite("flowGyeNoSpin", oneLine=F)

load_all("msuwcRouting")
flowGyeNoSpin <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff[360:396,],  Rsub=subsurfRunoff[360:396,], spinUpCycles=0, spinUpYears=10, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)

hyalite.send



# Read in gauge data
if(!exists("nwisGauges")){
    nwisGauges <- readOGR(nwisGaugeDir, nwisGaugeFname, stringsAsFactors=F)
}

load_all("msuwcRouting")

gaugeData <- GetGaugeData(edgesInBounds, nwisGauges, aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=F)

runOnHyalite("tempDataMonthly", obj=c("edgesInBounds", "nwisGauges", "setupList"), oneLine=T)

tempDataMonthly <- GetGaugeData(edgesInBounds, nwisGauges, simEndDate="2015-12-31", aggregateByMonth=T, checkGauges=F, varCode="00010")


notifyMe("All gauges processed")


makeHydrographs(flowGYE, gaugeData)
makeHydrographs(tempSim, gaugeData=NULL )
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




ll <- sort( sapply(ls(),function(x){object.size(get(x))}))

catchmentsToUse <- catchmentsInBounds
catchmentsToUse

load_all("msuwcRouting")

runHyalite("tMeanGye", overwrite=T)

tMeanGye <- AggregateRunoff(ncFile="/mnt/lustrefs/store/jerad.hoy/MSUWC/Data/DriverData/GYE_Daymet_stand_monthly_tmean.nc", catchmentPolygons=catchmentsInBounds, useWeights=T, sumData=F, runoffVar="tmean", startDate=simStartDate, by=timeStep, convertToDischarge=F) 


flowTest <- RouteWater(edges=edgesInBounds, catchments=catchmentsInBounds, Rsurf=surfaceRunoff,  Rsub=subsurfRunoff, spinUpCycles=gwSpinUpCycles, spinUpYears=spinUpYears, debugMode=F, by=timeStep, widthCoeffs=streamWidthCoeffs, manningN=manningN, slopeMin=slopeMin, aCoeffCoeff=aCoeffCoeff)




load_all("msuwcRouting")

runHyalite("tempSimGyeK10_c", objs=c("edgesInBounds", "catchmentsInBounds", "snow", "tMeanGye", "flowGye", "setupList", "StreamTemp_c"), oneLine=T, packages="msuwcRouting", overwrite=T)


tempSimGYEK10 <- StreamTemp(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1)

tempSimGYEK10_c <- StreamTemp_c(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1)

load_all("msuwcRouting")
tempSimLamar <- StreamTemp(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=300, K=10, etaInt=1, prof="prof2.out")




load_all("msuwcRouting")
sourceCpp("/Users/hoy/Desktop/MSUWC/Scripts/msuwcRouting/R/streamTempLoop.cpp")

tempSimLamarCpp <- StreamTempCpp(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=300, K=10, etaInt=1, prof="prof2.out")
tempSimLamar <- StreamTemp(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=300, K=10, etaInt=1, prof="prof2.out")

tempSimLamar

tempSimGyeCpp <- StreamTempCpp(edges=edgesInBounds, catchments=catchmentsInBounds, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, K=10, etaInt=1, prof="prof2.out")

tempSimGYEK10$Twater[,] == tempSimGye[,]

a <- !unlist(lapply(1:ncol(tempSimGye), function(i) {any(round(!tempSimGye[,i], 2) == round(tempSimGYEK10$Twater[,i], 2))}))
a

any(!colnames(tempSimGYEK10$Twater) == colnames(tempSimGye))
tempSimGye[,7616] == tempSimGYEK10$Twater[,7616]

cbind(
tempSimGye[,14365],
tempSimGYEK10$Twater[,14365]
)

tempSimLamarCpp[[2]]
tempSimLamar$TwaterUpstream


a <- !unlist(lapply(1:ncol(tempSimLamarCpp[[1]]), function(i) {any(!tempSimLamarCpp[[1]][,i] == tempSimLamar$Twater[,i])}))

a <- !unlist(lapply(1:ncol(tempSimLamarCpp[[3]]), function(i) {any(round(!tempSimLamarCpp[[3]][,i], 2) == round(tempSimLamar$TwaterLocal[,i], 2))}))
a

i=157

while(i <= ncol(tempSimLamarCpp[[1]])){
	print(cbind(
		tempSimLamarCpp[[1]][,i],
		tempSimLamar$Twater[,i]
	))
	print(i)
	i= i+1
	readline()
}




Rprof(NULL)
summaryRprof("prof.out")
summaryRprof("prof.out", lines="show" )
summaryRprof("prof.out", memory="stats" )
summaryRprof("prof.out", lines="both", memory="both" )
proftable("prof.out")

summaryRprof("prof2.out")
summaryRprof("prof2.out", lines="show" )
summaryRprof("prof2.out", memory="stats" )
summaryRprof("prof2.out", lines="both", memory="both" )
proftable("prof2.out")



a <- edg[RiverOrder > 1, DrainID]
b <- lapply(a, function(x) {as.character(edg[NextDown_2 == x, DrainID])})
names(b) <- a
b
setkey(edg, NULL)
setkey(edg, DrainID)
b[["45015"]]
edg[NextDown_2 == "45015", DrainID]
i=1e5
system.time(rep(b[["45015"]], i))
system.time(rep(edg[NextDown_2 == "45015", DrainID], i))

ids <- edg[,DrainID]

system.time(rep(edg[4000, DrainID], i))
system.time(rep(ids[4000],i))

a <- flowGye$v[3,]
system.time(rep(a <- flowGye$v[3,], i))





names(a) <- NULL

system.time(rep(flowGye$v[3,100], i))
system.time(rep(a[100], i))


tempSimLamar <- StreamTemp_c(edges=lamarEdges, catchments=lamarCatch, RsurfSnow=snow$msroSnow, RsurfNoSnow=snow$msroNoSnow, Tair=tMeanGye, simFlow=flowGye, defaults=setupList, by="month", outputExtraVars=T, debugMode=F, runStop=10, K=10, etaInt=1)

StreamTemp_c <- compiler::cmpfun(StreamTemp)

tempSim2010K10NoSpin <- tempSimGyeNoSpinK10
write.csv(t(tempSim2010K10NoSpin$Twater), "tempSimGye2010")

rm(tempSimGyeNoSpinK10)

#flowGye and flowGye2 loaded last nightkk

notifyMe("Temp finished running")

a <- 1e20
system.time(rep(for(i in 1:10) i*2, a))

system.time(rep(c(1:10)*2, times=a))


edgeToPlot <- "20008"
dgeToPlot <- "21647"
plot(tempSim[["Twater"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterLocal"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterLocalWarmed"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterQin"]][, edgeToPlot], type="l")
lines(tempSim[["TwaterSriv"]][, edgeToPlot], type="l")

plot(tempSimK.1[["Twater"]][, edgeToPlot], type="l")
plot(tempSimK.1[["TwaterLocal"]][, edgeToPlot], type="l")
plot(tempSimK.1[["TwaterLocalWarmed"]][, edgeToPlot], type="l")
lines(tempSimK.1[["TwaterQin"]][, edgeToPlot], type="l")
lines(tempSimK.1[["TwaterSriv"]][, edgeToPlot], type="l")

plot(tempSimK.9[["Twater"]][, edgeToPlot], type="l")
plot(tempSimK.9[["TwaterLocal"]][, edgeToPlot], type="l")
plot(tempSimK.9[["TwaterLocalWarmed"]][, edgeToPlot], type="l")
lines(tempSimK.9[["TwaterQin"]][, edgeToPlot], type="l")
lines(tempSimK.9[["TwaterSriv"]][, edgeToPlot], type="l")

par(mfrow=c(1,2))
plot(tempSimK10[["Twater"]][, edgeToPlot], type="l", col="black", ylim=c(0,16), lwd=1.5)
lines(rowMeans(tMeanCatch), type="l", lty=5, col="blue")

plot(tempSimK40[["Twater"]][, edgeToPlot], type="l", col="black", ylim=c(0,16), lwd=1.5)

plot(tempSimK0[["Twater"]][, edgeToPlot], type="l", col="black",ylim=c(0,16), lwd=1.5)
lines(rowMeans(tMeanCatch), type="l", lty=1, col="blue")

plotHydroVar(tempSim, gauges=NULL, hydroVar="Twater", edgeIdList=c("21647"), saveGraphs=saveHydrographs)
plotHydroVar(tempSimK10, gauges=NULL, hydroVar="Twater", edgeIdList=c("21647"), saveGraphs=saveHydrographs)

par(mfrow=c(1,2))
plot(tempData[[4]][12500:13140,2], type="l", ylim=c(0,17))
plot(tempDataMonthly[[4]][412:432,2], type="l", ylim=c(0,17))

plot(tempSimK10[["Twater"]][348:396, edgeToPlot], type="l", col="black", ylim=c(0,17), lwd=1.5)
lines(rowMeans(tMeanCatch)[348:396], type="l", lty=5, col="blue")

plot(tempSimK0[["Twater"]][348:396, edgeToPlot], type="l", col="black", ylim=c(0,17), lwd=1.5)
lines(rowMeans(tMeanCatch)[348:396], type="l", lty=5, col="blue")


plotHydroVar(tempSim, gauges=NULL, hydroVar="TwaterLocal", edgeIdList=c("21647"), saveGraphs=saveHydrographs)

plotHydroVar(tempSim, gauges=NULL, hydroVar="TwaterLocalWarmed", edgeIdList=c("21647"), saveGraphs=saveHydrographs)

##NOTES ON NEEDED INPUTS STILL
	##TT - calculate from velocity and stream length
	

load_all("msuwcRouting")
catchmentsToUse <- catchmentsInBounds

lamarSnowMsro <- AggregateSnow(defaults=setupList, catchmentPolygons=catchmentsToUse, ncDir="/mnt/lustrefs/store/jerad.hoy/MSUWC/Data/DriverData/Output_GYE_RoughCut_2050/")
hyalite.send

edgesTest <- edgesInBounds@data[order(edgesInBounds@data[, setupList$edgeOrderField]),]
edgesTest <- edges@data[order(edges@data[, setupList$edgeOrderField]),]
nrow(edgesTest)

edgeBlock <- list()
counter <- 1
while(nrow(edgesTest) > 0){
	if(nrow(edgesTest) < 1000){
		edgeBlock[[counter]] <- c(edgesTest$DrainID)
		edgesTest <- edgesTest[-which(edgesTest[, "DrainID"] %in% c(edgesTest$DrainID)),]
		next
	}
	#tail(edgesTest$RiverOrder, n=100)
	(lowestEdge <- tail(edgesTest, n=1)$DrainID)
	#print(lowestEdge)
	try(parents <- findAllParents(edgesTest, as.character(lowestEdge)))
	print(length(parents))
	edgeBlock[[counter]] <- c(lowestEdge, parents)
	edgesTest <- edgesTest[-which(edgesTest[, "DrainID"] %in% c(lowestEdge, parents)),]
	counter <- counter+1
}



##### processing al's data
tempData <- read.csv2("/Users/hoy/Desktop/MSUWC/Data/streamData.csv", header=T, sep=",", stringsAsFactors=F)
tempData <- data.table::data.table(tempData)
tempData[,X := NULL]
setnames(tempData, "Daily_Avg_T", "temp")
tempData[, Date := as.Date(Date, format="%m/%d/%Y")]
tempData[temp == "\xa0", temp := NA]
tempData[,temp := as.numeric(temp)]

tempData

setkey(tempData, ID)
streams <- unique(tempData)
streams[,c("Date", "temp") := NULL]

streams[, LATDEC := as.numeric(LATDEC)]
streams[, Long := as.numeric(Long)]

streamPoints <- sp::SpatialPointsDataFrame(coords=streams[, .(Long, LATDEC)], data=streams, coords.nrs=c(5,4), proj4string=edges@proj4string)

(strmPtsSnapped <- snapPointsToLines(streamPoints, edgesInBounds, maxDist=0.01, idField=setupList$edgeIdField))


##Need to create matrix with ID as colnames, date as rownmames and Daily_Avg temp as data

tempList <- list()

for(i in unique(tempData[,ID])){
	print(i)
	a <- tempData[ID == i, .(Date,temp)]
	tempList[[i]] <- a
}

tempListCleaned <- lapply(tempList, function(x) {try(cleanDat(x, setupList$simStartDate, setupList$simEndDate))})
tempListCleaned <- lapply(tempListCleaned, function(x) {try(as.data.frame(x))})
tempListMonthly <- lapply(tempListCleaned, function(x) {try(aggregate(x[,2], by=list((substr(x[,1], 1, 7))), mean))})
tempListMonthly <- lapply(tempListMonthly, function(x) {try(data.frame(zoo::as.yearmon(x[,1]), x[,2]))})
tempListMonthly


plot(tempListMonthly[[1]][,2], type="l")


	







system.time(rep(flowGye2$qOut[100, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]], 100000000))

system.time(rep(tempSim2010K10NoSpin$Twater[5, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]], 100000000))

tempSim2010K10NoSpin$Twater[8, as.character(edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == "21647", setupList$edgeIdField])]


tempTest <- lapply(tempSim2010K10NoSpin, function(x) {data.table(x)})
tempTest <- tempTest$Twater

system.time(rep(tempTest$Twater[5, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]], 1e8))

tempTest$Twater[, edgesInBounds[edgesInBounds[, setupList$edgeNextDownField] == edgesInBounds[i, setupList$edgeIdField], setupList$edgeIdField]]

t(tempTest)

test <- data.table(ID=colnames(tempTest), t(tempTest))
setkey(test, ID)

edgesTest <- data.table(edgesInBounds@data)
setkey(edgesTest, NULL)
edgesTest[, DrainID := as.character(DrainID)]
edgesTest[, NextDown_2 := as.character(NextDown_2)]

edgesTest


edgesTest[NextDown_2 == 21647, DrainID]

test[edgesTest[NextDown_2 == 21647, DrainID], V8]

tempSim2010K10NoSpin$Twater[8, as.character(edgesInBounds[edgesInBounds[, "NextDown_2"] == "21647", "DrainID"])]


getSalmonoidGrowth <- function(TwaterFrame){

	YCT <- apply(TwaterFrame, function(x) {-4.1727 + 0.946*x - 0.0348*x^2}, MARGIN=c(1,2))
	RBT <- apply(TwaterFrame, function(x) {-0.7691 + 0.4514*x - 0.0173*x^2}, MARGIN=c(1,2))
	BKT <- apply(TwaterFrame, function(x) {-1.2653 + 0.5213*x - 0.0196*x^2}, MARGIN=c(1,2))
	return(list(YCT=YCT, RBT=RBT, BKT=BKT))
}

sgm <- getSalmonoidGrowth(tempSimGyeCpp[[1]])

## Growth is daily percent of initial body mass
GrowthYCT  <- -4.1727 + 0.946*Twater - 0.0348*Twater^2

GrowthRBT <- -0.7691 + 0.4514*Twater - 0.0173*Twater^2

GrowthBKT <- -1.2653 + 0.5213*Twater - 0.0196*Twater^2



rle(sort(edgesInBounds$RiverOrder))
