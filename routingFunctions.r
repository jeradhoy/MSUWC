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
    runoff <- data.frame(t(extract(brick, catchmentPolygons, na.rm=T, fun=sum)))
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

# Correct negative and zero slopes in edges and set to a slopeMin value
CorrectEdgeSlopes <- function(edges, slopeMin=.01){
   slopes <- edges$SLOPE/120000
   slopes[slopes <= 0] <- slopeMin 
   edges$SLOPE2 <- slopes
   return(edges)
}



# Goes down edges and sums upstream contributing area, used in later calculations
AssignContribArea <- function(edges){
    edges <- edges[order(edges$RiverOrder),]
    edges$ContribArea <- NA
    
    for(i in 1:nrow(edges)){
        
        if(edges[i,]$RiverOrder == 1){
            edges[i,"ContribArea"] <- edges[i,]$Shape_Ar_1
        } else {
            edges[i,"ContribArea"] <- sum(edges[edges$NextDownID == edges[i,]$DrainID,]$ContribArea) + edges[i,]$Shape_Ar_1 
        }
        
    }
    return(edges)
}

# Assigning bankfull depth, could be used for flood situation, but not currently used
AssignBfDepth <- function(edges, a, b){

    edges$bfDepth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfDepth"] <- a*(edges[i,]$ContribArea)^b
    }
    return(edges)
}

# Assigns bankfull width, but used as width for channel dimensions
AssignBfWidth <- function(edges, a, b){

    edges$bfWidth <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"bfWidth"] <- a*(edges[i,]$ContribArea)^b
    }
    return(edges)
}

# Assigns an 'a' coefficient for use in non-linear groundwater storage-discharge
AssignAcoeff <- function(edges, coeff){

    edges$aCoeff <- NA
    
    for(i in 1:nrow(edges)){
        edges[i,"aCoeff"] <- (edges[i,]$Shape_Ar_1)*coeff
    }
    return(edges)
}

# Subesets catchments by HUC10 codes, ignores NA HUC10 codes
GetCatchInBounds <- function(catchments, hucCodes){
    catchmentCodes <- catchments$HUC10
    catchmentCodes[is.na(catchmentCodes)] <- 0
    catchments$HUC10 <- catchmentCodes
    return(catchments[catchments$HUC10 %in% hucCodes,])
}



# Routes surface and subsurface water through river network
RouteWater <- function(edges, Rsurf, Rsub, debugMode=F, by="day"){
    
    # Order edges by Shreve order so calculation is in right order
    edges <- edges[order(edges$RiverOrder),]

    # Set the timeLength to avoid re-executing nrow function
    timeLength <- nrow(Rsurf) 
    # Create seed matrix to use for storing data in results
    seedMatrix <- matrix(0, nrow=timeLength, ncol=ncol(Rsurf),
        dimnames=list(rownames(Rsurf), edges$DrainID))

    # Set mannings N value to a fixed .05, could be calibrated in the future
    manningN <- .05


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
	vConvFactor <- 60*60*24 
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
            vConvFactor <- 60*60*24*daysInMonth[as.integer(format(as.yearmon(rownames(Rsurf[timeStep,])), "%m"))%%12 + 1]/100
        }

	# Loop through edges in river network
        for(i in 1:nrow(edges)){

	    # Set hydroID of edges so don have to keep subsetting
            hydroID <- as.character(edges[i,]$DrainID)
            
            if(edges[i,]$RiverOrder == 1){
		# Set qIn to 0 for edges of Order 1
                qIn <- 0
            } else {
		# Sum qOut of parent edges for edges of Order > 1
                qIn <- sum(results$qOut[timeStep,
                	   as.character(edges[edges$NextDownID == edges[i,]$DrainID,]$DrainID)])
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
	    v <- (((height*width)/(2*height+width))^(2/3) * edges$SLOPE2[i]^.5)/manningN 
	    # Set velocity caps, upper cap may be unncessary, need to calibrate values to get realisic velocity
	    #v <- ifelse(v >= 3.0, 3.0, v)	
	    #v <- ifelse(v <= .25, .25, v)	

	    # Convert velocity form m/s to km/timestep (km/day)
	    v <- v*vConvFactor

	    # IMPORTANT, len/v < 1 for mass to be conserved, enforcing velocity minimum
	    v <- ifelse(len/v < 1, len, v)	
	    
	    # Caluclate groundwater discharge
	    # Ignores current timestep subsurface runoff, assuming that groundwater movement is too slow
            qSub <- (sSub/edges$aCoeff[i])^(1/.5)
	    
	
	    # Assumes that l/v <= 1, if not, need different routing scheme 
	    qOut <- sRiv + (1-len/v)*qIn + (1 - len/(2*v))*(rS + qSub)

	    # Store values in results list
            results$sRiv[timeStep, hydroID] <- sRiv + rS + qIn + qSub - qOut
            results$sSub[timeStep, hydroID] <- sSub + rSub - qSub
            results$qOut[timeStep, hydroID] <- qOut
            results$qIn[timeStep, hydroID] <- qIn
            results$qSub[timeStep, hydroID] <- qSub
	    results$v[timeStep, hydroID] <- v/vConvFactor
            results$h[timeStep, hydroID] <- height


	    if(debugMode){
		# DebugMode in order to debug problems
		print(" ")
	    	print(paste("Day:",timeStep, "Edge:", hydroID, "Order:", edges[i,]$RiverOrder))
	    	print(paste("qIn =", qIn))
	    	print(paste("qOut=",qOut))
	    	print(paste("qSub=",qSub))
	    	print(paste("sRiv =",sRiv))
	    	print(paste("rS =", rS))
	    	print(paste("rSub =",rSub))
	    	print(paste("sSub =",sSub))
	    	print(paste("aCoeff=",aCoeff))
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
