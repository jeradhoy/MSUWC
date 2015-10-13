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

#' Aggregate Runoff
#'
#' Reads in runnoff NetCDFs and generates matrix of runoff aggregated by catchment and time stepusing the extract() function
#'
#' @param ncFile NetCDF runoff file to be processed
#' @param catchmentPolygons catchments to be used for aggregation
#' @param runoffVar netCDF variable that the runoff data is stored in 
#' @param startDate simulation start date, needed for aggregating by month and rownames. Default is NULL and ok if only using daily data
#' @param leapDay set to False if leapdays are not included in simulation. Used for setting rownames of daily runoff. Default is F
#' @param by time step of runoff. Can be "day", or "month". Default is day. Used for unit conversion and rownames
#' @param fname filename to write data to. Default is NULL. If not NULL, data will be written to text file in format fname.txt
#'
#' @return Dataframe of runoff aggregated by catchments and time
#'
#' @examples
#' surfaceRunoff <- AggregateRunoff(ncFile=paste(ncdir, "/",  surfaceNcName, sep=""), catchmentPolygons=catchmentsToUse, runoffVar=surfaceVarName, startDate=simStartDate, by=timeStep)
#' 
#' @name aggregateRunoff
#' @export

AggregateRunoff <- function(ncFile, catchmentPolygons, runoffVar, useWeights=F, startDate=NULL, leapDays=F, by="day", fname=NULL){

    # Create Raster brick from NC file
    brick <- brick(ncFile, varname=runoffVar)
    print("Finished building brick")

    # Use extract function to sum runnoff for each catchment
    runoff <- data.frame(t(extract(brick, catchmentPolygons, weights=useWeights, na.rm=T, fun=mean)))

    print("finished extracting data")

    if(useWeights){
	runoff <- sweep(runoff, MARGIN=2, catchmentPolygons@data[, catchAreaField]*14400, '*')
    }

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
	daysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)

	for(month in 1:nrow(runoff)){
	  # Convert form mm/m2/timestep to m3/sec
	  # Need to vary number of days in each month for conversion
      	  runoff[month,] <- runoff[month,]/1000*1000000/(24*60*60*daysInMonth[as.numeric(format(as.yearmon(rownames(runoff[month,])), "%m"))])
	}
    }

    colnames(runoff) <- catchmentPolygons$HydroID


    if(!is.null(fname)){
    	write.table(runoff, paste(fname, ".txt", sep=""))
    }
    return(runoff)
}





AggregateRunoffSnow <- function(ncFileName, ncDir, snowFile, catchmentPolygons, runoffVar, useWeights=F, startDate=NULL, leapDays=F, by="day", fname=NULL){

    #Create 2 new nc files, one from snow one not
    if(!file.exists(paste(ncdir, "msroSnow.nc", sep=""))){
	system(paste("cdo ifthenc,1 ", ncDir, ncFileName, " ", ncDir, "snowMask.nc", sep=""))
	system(paste("cdo mul ", ncDir, "snowMask.nc ", ncDir, ncFileName, " ", ncDir, "msroSnow.nc", sep=""))
	system(paste("rm ", ncDir, "snowMask.nc", sep=""))
    }

    if(!file.exists(paste(ncdir, "msroNoSnow.nc", sep=""))){
	system(paste("cdo ifnotthenc,1 ", ncDir, ncFileName, " ", ncDir, "noSnowMask.nc", sep=""))
	system(paste("cdo mul ", ncDir, "noSnowMask.nc ", ncDir, ncFileName, " ", ncDir, "msroNoSnow.nc", sep=""))
	system(paste("rm ", ncDir, "noSnowMask.nc", sep=""))
    }

    filesToProcess <- c("msroSnow.nc", "msroNoSnow.nc")
   
    results <- list()

    for(ncFile in 1:length(filesToProcess)){

	# Create Raster brick from NC file
	brick <- brick(paste(ncDir, filesToProcess[ncFile], sep=""), varname=runoffVar)
	print("Finished building bricks")

	# Use extract function to sum runnoff for each catchment
	runoff <- data.frame(t(extract(brick, catchmentPolygons, weights=useWeights, na.rm=T, fun=mean)))

	print("finished extracting data")

	if(useWeights){
	    runoff <- sweep(runoff, MARGIN=2, catchmentPolygons@data[, catchAreaField]*14400, '*')
	}

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
	    daysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)

	    for(month in 1:nrow(runoff)){
	      # Convert form mm/m2/timestep to m3/sec
	      # Need to vary number of days in each month for conversion
	      runoff[month,] <- runoff[month,]/1000*1000000/(24*60*60*daysInMonth[as.numeric(format(as.yearmon(rownames(runoff[month,])), "%m"))])
	    }
	}

	colnames(runoff) <- catchmentPolygons$HydroID


	if(!is.null(fname)){
	    write.table(runoff, paste(fname, ".txt", sep=""))
	}

	runoff <- list(runoff)
	names(runoff) <- unlist(strplit(filesToProcess[ncFile], "[.]"))[1]
	results <- c(results, runoff)
    }
    return(results)
}
