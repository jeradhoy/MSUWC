###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:
#
#Sourced into streamNetwork.r file to do actual routing
#


# Generates matrix of runoff by catchment and time step
# Builds raster brick and uses "extract" function to accumulate runoff
# writes to text file if specified
AggregateRunoff <- function(ncFile, catchmentPolygons, runoffVar, startDate=NULL, leapDays=F, by="day", fname=NULL){

    # Create Raster brick from NC file
    brick <- brick(ncFile, varname=runoffVar)
    #print("Finished building brick")

    # Use extract function to sum runnoff for each catchment
    runoff <- data.frame(t(extract(brick, catchmentPolygons, weights=F, na.rm=T, fun=sum)))
    #print("finished extracting data")

    if(by == "day"){
      # convert from mm/m2/day to m3/sec
      runoff <- runoff/1000*1000000/(24*60*60)

      if(!is.null(startDate)){
	# Create sequence of dates to use as rownames
	dates <- seq(as.Date(startDate), by="day", length.out=nrow(runoff))
	if(leapDays == F){
	  # Take out leap days
	  dates <- dates[c(-grep("02-29", dates))]
	}
	rownames(runoff) <- dates
      }

    }

    if(by == "month"){

      if(!is.null(startDate)){
	dates <- as.yearmon(seq(as.Date(startDate), by="month", length.out=nrow(runoff)))
	rownames(runoff) <- dates
      }

	# Starts with december so that 12%%12 returns 1
	daysInMonth <- c(31, 31,28,31,30,31,30,31,31,30,31,30)

	for(month in 1:nrow(runoff)){
	  # Convert form mm/m2/timestep to m3/sec
	  # Need to vary number of days in each month for conversion
      	  runoff[month,] <- runoff[month,]/1000*1000000/(24*60*60*daysInMonth[as.integer(format(as.yearmon(rownames(runoff[month,])), "%m"))%%12 + 1])
	}
    }

    colnames(runoff) <- catchmentPolygons$HydroID


    if(!is.null(fname)){
    	write.table(runoff, paste(fname, ".txt", sep=""))
    }
    return(runoff)
}




GetGaugeData <- function(edges, gauges, maxDist=.005, idField=edgeIdField, startDate=simStartDate){

    gaugesInBounds <<- snapPointsToLines(gauges, edges, maxDist=maxDist, idField=idField)
    print(paste("Found", nrow(gaugesInBounds), "gauges to process."))

    gaugeData <- list()
    #For each gauge, append to a list the matrix of gauge data

    for(i in 1:nrow(gaugesInBounds)){

	

	if(url.exists(paste("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=", gaugesInBounds@data[i, 1], "&period=P10000000W&parameterCd=00060", sep=""))){
	   dat <- read.table(paste("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=", gaugesInBounds@data[i, 1], "&period=P10000000W&parameterCd=00060", sep=""),  header=TRUE, stringsAsFactors = FALSE)

	} else {
	    print(paste(gaugesInBounds@data[i,1], "gauge data URL does not exist."))
	    next
	}

	print(paste("Processing gauge", gaugesInBounds@data[i,1]))
	dat <- dat[-c(1),]

	if(as.Date(tail(dat[,3], n=1L)) < as.Date(simStartDate)){
	    print(paste("Data for gauge", gaugesInBounds@data[i,1], "ends before simStartDate at", simStartDate))
	    next
	}

	if(as.Date(dat[1,3]) > seq(as.Date(simStartDate), by=paste(timeStep, "s", sep=""), len=nrow(surfaceRunoff)+1)[nrow(surfaceRunoff+1)]){
	    print(paste("Data for gauge", gaugesInBounds@data[i,1], "starts before end of endDate ", seq(as.Date(simStartDate), by=paste(timeStep, "s", sep=""), len=nrow(surfaceRunoff)+1)[nrow(surfaceRunoff+1)]))
	    next
	}
	
	print(paste("Data is new enough for gauge", gaugesInBounds@data[i,1]))
	
	dat <- dat[-c(grep("02-29", dat[,3])),]

	if(any(dat[,4] == "Ice")){
	   dat[dat[,4] == "Ice", 4] <- 0
	}

	dat[,4] <- as.numeric(dat[,4])/35.3146666666666666666666666666666666667
	dat <- list(dat)
	names(dat) <- as.character(gaugesInBounds@data[i, 8])
	gaugeData <- c(gaugeData, dat)


    }

   return(gaugeData)
}




# Convert to m/m and correct negative and zero slopes in edges and set to a slopeMin value
CorrectEdgeSlopes <- function(edges, slopeMin=.01){
   slopes <- edges@data[, slopeFieldDeg]/120000
   slopes[slopes <= 0] <- slopeMin 
   edges$Slope2 <- slopes
   return(edges)
}



# Goes down edges and sums upstream contributing area, used in later calculations
AssignContribArea <- function(edges, catchments){
    edges <- edges[order(edges@data[, edgeOrderField]),]

    edges$ContribArea <- NA
    
    for(i in 1:nrow(edges)){
        
        if(edges@data[i,edgeOrderField] == 1){
            edges[i,"ContribArea"]  <- catchments@data[which(catchments@data[,catchIdField] == edges@data[i, edgeIdField]), catchAreaField]

        } else {
	    

            edges[i,"ContribArea"] <- sum(edges@data[edges@data[, edgeNextDownField] == edges@data[i,edgeIdField],"ContribArea"]) + catchments@data[which(catchments@data[,catchIdField] == edges@data[i, edgeIdField]), catchAreaField] 
        }
        
    }

    edges$ContribArea <- edges$ContribArea*14400

    return(edges)
}

# Assigning bankfull depth, could be used for flood situation, but not currently used
#AssignBfDepth <- function(edges, a, b){

#    edges$bfDepth <- NA
    
#    for(i in 1:nrow(edges)){
#        edges[i,"bfDepth"] <- a*(edges[i,]$ContribArea)^b
#    }
#    return(edges)
#}

# Assigns bankfull width, but used as width for channel dimensions
AssignBfWidth <- function(edges, a, b){

    edges$bfWidth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfWidth"] <- a*(edges[i,]$ContribArea)^b
    }
    return(edges)
}

# Assigns an 'a' coefficient for use in non-linear groundwater storage-discharge
AssignAcoeff <- function(edges, catchments, coeff){

    edges$aCoeff <- NA
    
    for(i in 1:nrow(edges)){
	edges[i,"aCoeff"] <- coeff * catchments@data[which(catchments@data[,catchIdField] == edges@data[i, edgeIdField]), catchAreaField] * 14400
    }

    return(edges)

}

getParents <- function(shapeFrame, id, idField=edgeIdField, nextDownField=edgeNextDownField){
            shapeFrame@data[shapeFrame@data[, nextDownField] == id, idField]
}

getOrder <- function(shapeFrame, id, idField=edgeIdField, orderField=edgeOrderField){
        shapeFrame@data[shapeFrame@data[, idField] == id, orderField]
}


findAllParents <- function(shapeFrame, ID, createArray=T, idField=edgeIdField, nextDownField=edgeNextDownField, orderField=edgeOrderField){
        if(createArray == T){
           IDarray <<- c()
        }
        for(i in 1:length(ID)){

	    if(getOrder(shapeFrame, ID[i], idField, orderField) >= 2){

		parents <- getParents(shapeFrame, ID[i], idField, nextDownField)
		IDarray <<- c(IDarray, parents)

		for(j in 1:length(parents)){

		    if(getOrder(shapeFrame, parents[j], idField, orderField) > 1){
		       findAllParents(shapeFrame, parents[j], createArray=F, idField, nextDownField)
		    }
		}
	     }
          }
	    return(IDarray)
}


# Subesets catchments by HUC10 codes, ignores NA HUC10 codes
GetShapesInBounds <- function(shapeFrame, hucCodes, by="HUC"){

    if(shapeFrame@class[1] == "SpatialLinesDataFrame"){
	hucField <- edgeHucField
	orderField <- edgeOrderField
	nextDownField <- edgeNextDownField
	idField <- edgeIdField
    } else {
	hucField <- catchHucField
	orderField <- catchOrderField
	nextDownField <- catchNextDownField
	idField <- catchIdField
    }
    print(idField)


    shapesInHuc <- shapeFrame[shapeFrame@data[, hucField] %in% hucCodes,]
    
    maxOrderId <- shapesInHuc@data[which(shapesInHuc@data[, orderField] == max(shapesInHuc@data[, orderField])), idField]

    print(maxOrderId)

    maxOrderId <- shapesInHuc@data[shapesInHuc@data[, nextDownField] == shapesInHuc@data[shapesInHuc@data[, idField] == maxOrderId , nextDownField], idField]

    print(maxOrderId)



    shapeIDsInBounds <- c(maxOrderId, findAllParents(shapeFrame, maxOrderId, createArray=T, idField, nextDownField, orderField))



    if(nrow(shapesInHuc) != length(shapeIDsInBounds)){
	warning(paste("Some headwater shapes not in selected HUC. Adding", length(shapeIDsInBounds)-nrow(shapesInHuc), "shapes."))
    }

	shapesInBounds <- shapeFrame[shapeFrame@data[, idField] %in% shapeIDsInBounds,]

    return(shapesInBounds)
}



GetShapesById <- function(shapeFrame, IDs){

    if(shapeFrame@class[1] == "SpatialLinesDataFrame"){
	hucField <- edgeHucField
	orderField <- edgeOrderField
	nextDownField <- edgeNextDownField
	idField <- edgeIdField
    } else {
	hucField <- catchHucField
	orderField <- catchOrderField
	nextDownField <- catchNextDownField
	idField <- catchIdField
    }

    shapeIDsInBounds <- c(IDs, findAllParents(shapeFrame, IDs, createArray=T, idField, nextDownField, orderField))

    shapesInBounds <- shapeFrame[shapeFrame@data[, idField] %in% shapeIDsInBounds,]


    return(shapesInBounds)
}

makeHydrographs <- function(flowData, gauges, edges=NULL, precip=NULL, spack=NULL, saveGraphs=F, plotTogether=F, interact=F){


    if(length(gauges) >= 1){

	if(plotTogether){
	    par(mfrow=c(length(gauges), 1))
	} else {
	    par(.pardefault)
	}
	for(i in 1:length(gauges)){
	    if(saveGraphs){
		png(paste(plotDir, "/",gsub(" ", "_", gsub("[[:punct:]]", "", gaugesInBounds@data[1, "SITENAME"])), ".png", sep=""))
	    }

	    dates <- as.Date(as.yearmon(rownames(flow$qOut)))
	    plot(dates, flowData$qOut[, names(gauges)[i]], type="l", col="red", xlab="",  ylab="Flow (m/s)")
	    lines(as.Date(gauges[[i]][, "datetime"]), gauges[[i]][,4])
	    abline(0, 0)
	    title(gaugesInBounds@data[gaugesInBounds@data[,8] == as.numeric(names(gauges[1])),"SITENAME"])
	    legend("topleft", col=c("red", "black"), legend=c("Routed LPJ-Guess Runoff", "Gauge Data"), lty=1)

	    if(saveGraphs){
		dev.off()
	    }
	    if(interact){
		if(i < length(gauges)){
		    answer <- readline("Press Enter To Continue")
		    if(answer == ""){
			next
		    } else {
			break
		    }
		}
	    }
		
	}
    }
}



# Routes surface and subsurface water through river network
RouteWater <- function(edges, catchments, Rsurf, Rsub, debugMode=F, by="day", widthCoeffs=c(.3, .6), manningN=.07, slopeMin=.01, aCoeffCoeff=3){
    
    # Order edges by Shreve order so calculation is in right order
    edges <- edges[order(edges@data[, edgeOrderField]),]

    print(dim(edges))
    print(edgeIdField)
    print(length(edges@data))

    Rsurf <- Rsurf[,as.numeric(colnames(Rsurf)) %in% edges@data[, edgeIdField]]
    Rsub <- Rsub[,as.numeric(colnames(Rsub)) %in% edges@data[, edgeIdField]]

    edges <- AssignContribArea(edges, catchments)
    edges <- AssignBfWidth(edges, widthCoeffs[1], widthCoeffs[2])
    edges <- CorrectEdgeSlopes(edges, slopeMin)
    edges <- AssignAcoeff(edges, catchments, aCoeffCoeff)
    edges$LengthKM <- edges@data[, edgeLengthField] * 120


    # Set the timeLength to avoid re-executing nrow function
    timeLength <- nrow(Rsurf) 
    # Create seed matrix to use for storing data in results

    print(dim(Rsurf))
    print(dim(catchmentsInBounds))
    print(length(colnames(Rsurf)))

    seedMatrix <- matrix(0, nrow=timeLength, ncol=ncol(Rsurf),
        dimnames=list(rownames(Rsurf), edges@data[, edgeIdField]))



    results <- list(
        qIn = seedMatrix,
        qOut = seedMatrix,
        sRiv = seedMatrix,
        sSub = seedMatrix, 
        qSub = seedMatrix, 
        v = seedMatrix,
        h = seedMatrix 
    )

    
    if(by == "day"){
	# Set the velocity conversion factor for a daily timestep
	vConvFactor <- 60*60*24/1000
    }
    
    # Set days in month to use for monthly-timestep velocity conversion factor
    daysInMonth <- c(31, 31,28,31,30,31,30,31,31,30,31,30)


    # Loop through each day/timestep
    for(timeStep in 1:timeLength){

	# Start time to use for fucntion time-estimates
	if(timeStep == 1){
	    start <- Sys.time()
	}

        if(by == "month"){
	    # Set velocity conversion factor based on month of timestep
            #vConvFactor <- 60*60*24*daysInMonth[as.integer(format(as.yearmon(rownames(Rsurf[timeStep,])), "%m"))%%12 + 1]/1000
            vConvFactor <- 60*60*24*daysInMonth[timeStep%%12 + 1]/1000
        }

	# Loop through edges in river network
        for(i in 1:nrow(edges)){

	    # Set hydroID of edges so don have to keep subsetting
            hydroID <- as.character(edges@data[i,edgeIdField])
            
            if(edges@data[i, edgeOrderField] == 1){
		# Set qIn to 0 for edges of Order 1
                qIn <- 0
            } else {
		# Sum qOut of parent edges for edges of Order > 1
                qIn <- sum(results$qOut[timeStep,
                	   as.character(edges@data[edges@data[, edgeNextDownField] == edges@data[i, edgeIdField], edgeIdField])])
            }


	    # Set rS(surface runoff), rSub(sub runoff)
	    rS <- Rsurf[timeStep, hydroID]
	    rSub <- Rsub[timeStep, hydroID]

	    # Set fixed channel dimensions
	    len <- edges$LengthKM[i]
	    width <- edges$bfWidth[i]

	    if(timeStep == 1){
		# On the first timestep, sRiv and sSub (storage) is 0, height needs to be initialized
		sRiv <- 0
		height <- 1
		sSub <- 0
	    } else {
		# Set sRiv, sSub from previous day
		sRiv <- results$sRiv[timeStep-1, hydroID]
		sSub <- results$sSub[timeStep-1, hydroID]

		# Calculate height form sRiv and channel dimensions
		height <- (sRiv * vConvFactor*1000)/(len * 1000 * width)
	    }

	    # Manning's Eqation used to calculate velocity
	    v <- (((height*width)/(2*height+width))^(2/3) * edges@data[i, "Slope2"]^.5)/manningN 
	    # Set velocity caps, upper cap may be unncessary, need to calibrate values to get realisic velocity
	    #v <- ifelse(v >= 3.0, 3.0, v)	
	    v <- ifelse(v <= .01, .01, v)	

	    # Convert velocity form m/s to km/timestep (km/day)
	    v <- v*vConvFactor

	    # Caluclate groundwater discharge
	    # Ignores current timestep subsurface runoff, assuming that groundwater movement is too slow
            qSub <- (sSub/edges$aCoeff[i])^(1/.5)
	    # Could base it off stream dimensions
	    qLoss <- 0
	    
	    if(qIn > 1e5){
		stop("Ridiculous Values!!!")
	    }
	
	    # Assumes that l/v <= 1, if not, need different routing scheme 
	    # Delta t is always one, so l/v can work
	    #qOut <- sRiv + (1-len/v)*qIn + (1 - len/(2*v))*(rS + qSub)
	    qOut <- if(len/v < 1){
		sRiv + (1 - len/v)*qIn + (1 - len/(2*v))*(rS + qSub)
	    } else {
		(v/len)*sRiv + 0*qIn + (v/(2*len))*(rS + qSub) 
	    }


	    # Store values in results list
            results$sRiv[timeStep, hydroID] <- sRiv + rS + qIn + qSub - qOut - qLoss
            results$sSub[timeStep, hydroID] <- sSub + rSub - qSub
            results$qOut[timeStep, hydroID] <- qOut
            results$qIn[timeStep, hydroID] <- qIn
            results$qSub[timeStep, hydroID] <- qSub
	    results$v[timeStep, hydroID] <- v/vConvFactor
            results$h[timeStep, hydroID] <- height
            
	    if(debugMode){
		# DebugMode in order to debug problems
		print(" ")
	    	print(paste("Day:",timeStep, "Edge:", hydroID, "Order:", edges@data[i, edgeOrderField]))
	    	print(paste("qIn =", qIn))
	    	print(paste("qOut=",qOut))
	    	print(paste("qSub=",qSub))
	    	print(paste("sRiv =",sRiv))
	    	print(paste("rS =", rS))
	    	print(paste("rSub =",rSub))
	    	print(paste("sSub =",sSub))
	    	print(paste("len =", len))
	    	print(paste("width =", width))
	    	print(paste("height =", height))
	    }


	}



	if(timeStep == 10){
	    print(paste("ETA:", 
			round((Sys.time()-start)*length((timeStep+1):timeLength)/10, 2), 
			"seconds or", 
			round((Sys.time()-start)*length((timeStep+1):timeLength)/10/60, 2), 
			"minutes. Will finish at", 
			(Sys.time()-start)*length((timeStep+1):timeLength)/10+Sys.time()
		))
	}
    }
    
    return(results)
}
