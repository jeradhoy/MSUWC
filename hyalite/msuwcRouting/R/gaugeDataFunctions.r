###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:


#' Fill Data Gaps
#'
#' Fills gaps in daily stream gaugedata with NA so that statistics can later be calculated with data. Function only used by getGaugeData
#'
#' @param dat dataframe of guage data to be cleaned and filled
#' @param simStartDate date of the beginning of the simulation
#' @param simEndDate date of the end of the simulation
#'
#' @return Dataframe of stream gauge data with missing values filled with NAs
#'
#' @examples
#' cleanDat(dat, simStartDate, simEndDate)
#'
#' @name cleanDat
#' @export

cleanDat <- function(dat, simStartDate, simEndDate){

    #Erases data befor start date
    if(as.Date(dat[1,1]) < as.Date(simStartDate)){
	print("Erasing beginning")
        dat <- dat[-c(which(dat[,1] < as.Date(simStartDate))),]
    }

    # Erases data after end date
    if(as.Date(tail(dat[,1], n=1L)) > as.Date(simEndDate)){
	print("Erasing end data")
        dat <- dat[-c(which(dat[,1] > as.Date(simEndDate))),]
    }

    # Fills in beginning if with NAs
    if(as.Date(dat[1,1]) > as.Date(simStartDate)){
	print("Filling in beginning")
        d <- as.character(seq(as.Date(simStartDate), as.Date(dat[1,1]), by="day"))
	d <- d[1:(length(d)-1)]
	d <- data.frame(d, NA)
	names(d) <- names(dat)
	dat <- rbind(d, dat)
    }

    # Fills in end with NAs
    if(as.Date(tail(dat[,1], n=1L)) < as.Date(simEndDate)){
	print("Filling in end data")
        # Fills in missing end
        d <- as.character(seq(as.Date(tail(dat[,1], n=1L)), as.Date(simEndDate), by="day"))[-1]
	d <- data.frame(d, NA)
	names(d) <- names(dat)
	dat <- rbind(dat, d)
	print("Filled in end data")
    }

    i <- 1
    a <- nrow(dat)
    while(i < a){
	# Fills in holes in data with NAs 
	if(as.Date(dat[i+1, 1]) != as.Date(seq(as.Date(dat[i,1]), by="day", length=2)[2])){

	    print(paste("Data missing after", dat[i,1]))
            d <- as.character(seq(as.Date(dat[i,1]), as.Date(dat[i+1, 1]), by="day"))
	    d <- d[-c(1, length(d))]
	    d <- data.frame(d, NA)
	    names(d) <- names(dat)
	    dat <- rbind(dat[1:i, ], d, dat[(i+1):nrow(dat), ])
	    i <- i+nrow(d)+1
	    a <- nrow(dat)
	    next
	}
	i <- i+1
    }
    

    if(length(grep("02-29", dat[,1])) > 0){
	dat <- dat[-c(grep("02-29", dat[,1])),]
    }


    return(dat)
}


#' USGS Gauge Data Fetcher
#'
#' Finds USGS stream gauges close to edges, downloads, cleans, and returns a list of dataframes for each gauge
#'
#' @param edges SpatialLinesDataFrame of edges. Used to find gauges that correspond to stream network edges
#' @param gauges SpatialPointsDataFrame of NWIS stream gauges. Download shapefile from nwis website
#' @param aggregateByMonth logical of whether to aggregate stream gauge data by month, used for comparing to monthly simulation data. Defaults to T
#' @param maxDist maximum distance that a stream gauge point can be from a line and still snap to it. Can be calibrated for better snapping accuracy. Units in decimal degrees (I think). Defaulted to .005 decimal degrees
#' @param idField field of edge data frame that contains the identifying field to be used for relating stream gauge points to edges. Defaults to edgeIdField, a parameter set at the beginning of routeWater.r script
#' @param checkGauges logical indicating whether to plot and manually approve of gauge snap befor processing. Defaulted to FALSE
#'
#' @return List of dataframes for each stream gauge
#'
#' @examples
#'  gaugeData <- GetGaugeData(edgesInBounds, nwisGauges, aggregateByMonth=aggregateGaugeDataByMonth, checkGauges=T)
#'
#' @name GetGaugeData
#' @export
GetGaugeData <- function(edges, gauges, aggregateByMonth=T, maxDist=.005, idField=edgeIdField, checkGauges=F){

    gaugesSnapped <<- tryCatch(snapPointsToLines(gauges, edges, maxDist=maxDist, idField=idField), error=function(e){return(c())})

    #print("meow")
    gaugesInBounds <- gauges

    print(paste("Found", nrow(gaugesInBounds), "gauges to process."))

    if(length(gaugesInBounds) == 0){
	return(list())
    }

    gaugeData <- list()
    #For each gauge, append to a list the matrix of gauge data


    for(i in 1:nrow(gaugesInBounds)){

	print(paste("Downloading data for gauge", gaugesInBounds@data[i,1]))
	

	#if(url.exists(paste("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=", gaugesInBounds@data[i, 1], "&period=P10000000W&parameterCd=00060", sep=""))){
       dat <- tryCatch(read.table(paste("http://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=", gaugesInBounds@data[i, 1], "&period=P10000000W&parameterCd=00060", sep=""),  header=TRUE, stringsAsFactors = FALSE), error=function(e){NULL})
	if(is.null(dat)){
	    print(paste("Error downloading data for gauge", gaugesInBounds@data[i,1], "Skipping to next gauge."))
	    next
	}

	#} else {
	#    print(paste(gaugesInBounds@data[i,1], "gauge data URL does not exist."))
	#    next
	#}

	print(paste("Processing gauge", gaugesInBounds@data[i,1]))

	dat <- dat[-c(1),-c(1,2,5)]

	print("here")

	if(as.Date(tail(dat[,1], n=1L)) < as.Date(simStartDate)){
	    print(paste("Data for gauge", gaugesInBounds@data[i,1], "ends before simStartDate at", simStartDate))
	    next
	}

	print("here")
	if(as.Date(dat[1,1]) > as.Date(simEndDate)){
	    #seq(as.Date(simStartDate), by=paste(timeStep, "s", sep=""), len=nrow(surfaceRunoff)+1)[nrow(surfaceRunoff+1)]){
	    #print(paste("Data for gauge", gaugesInBounds@data[i,1], "starts before end of endDate ", seq(as.Date(simStartDate), by=paste(timeStep, "s", sep=""), len=nrow(surfaceRunoff)+1)[nrow(surfaceRunoff+1)]))
	    print(paste("Data for gauge", gaugesInBounds@data[i,1], "starts after simEndDate at", simEndDate))
	    next
	}
	
	print(paste("Data is new enough for gauge", gaugesInBounds@data[i,1]))
	




	if(checkGauges){
	    plot(gauges[gauges@data[,1] == gaugesInBounds@data[i,1],], xlim=c(as.numeric(gaugesInBounds@data[i, 5])-.1, as.numeric(gaugesInBounds@data[i, 5])+.1), ylim=c(as.numeric(gaugesInBounds@data[i, 6])-.1, as.numeric(gaugesInBounds@data[i, 6])+.1), col="blue")
	    points(gaugesInBounds[i, ], col="red")
	    lines(edgesInBounds)
	    answer <- readline("Is gauge a good match? Enter to continue, 'n' to remove gauge")
	    if(answer == "n"){
		answer2 <- readline("Reassign to another edge? 'y' to confirm.")
		if(answer2 == "y"){
		    edgeIdToReassign <- as.numeric(readline("Enter edgeId"))
		    
		    gaugesInBounds@dat[i,8] <- edgeIdToReassign
		} else {
		    next
		}
	    }
	}

	if(any(dat[,2] == "Ice")){
	   dat[dat[,2] == "Ice", 2] <- 0
	}

	dat[,2] <- as.numeric(dat[,2])/35.3146666666666666666666666666666666667

	dat <- tryCatch(cleanDat(dat, simStartDate, simEndDate), error=function(e){NULL})

	if(is.null(dat)){
	    next
	}

	if(aggregateByMonth){
	    print("Aggregating by month")
	    dat <- aggregate(dat[,2], by=list((substr(dat[,1], 1, 7))), sum)
	    daysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
	    for(j in 1:nrow(dat)){
		dat[j,2] <- dat[j, 2]/daysInMonth[as.numeric(substr(dat[j,1], 6, 7))]
	    }
	    dat[,1] <- as.yearmon(dat[,1])
	}


	print(nrow(dat))
	dat <- list(dat)

	#names(dat) <- as.character(gaugesInBounds@data[i, 8])

	if(any(gaugesSnapped@data[,1] == gaugesInBounds@data[i,1])){
	    names(dat) <- as.character(gaugesSnapped@data[gaugesSnapped@data[,1] == gaugesInBounds@data[i,1], 8])
	}else{
	    names(dat) <- NA
	}

	#names(dat) <- tryCatch(snapPointsToLines(gaugesInBounds[i,], edges, maxDist, idField)@data[,8], error=function(e){NA})

	gaugeData <- c(gaugeData, dat)


    }

   return(gaugeData)
}

