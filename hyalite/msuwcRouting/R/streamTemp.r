###################################
#Functions for routing LPF-GUESS Runoff
#
#Created by Jerad Hoy
#Date Created: 7/7/2015
#Date Last Modified:
#
#Sourced into streamNetwork.r file to do actual routing
#


# Routes surface and subsurface water through river network
StreamTemp <- function(edges, catchments, RsurfSnow, RsurfNoSnow, Tair, simFlow, debugMode=F, by="month", outputExtraVars=T){
    
    edges <- AssignContribArea(edges, catchments)

    # Order edges by Shreve order so calculation is in right order
    edges <- edges@data[order(edges@data[, edgeOrderField]),]

	edges[,edgeIdField] <- as.character(edges[,edgeIdField])

	RsurfSnow <- RsurfSnow[,match(edges[, edgeIdField], colnames(RsurfSnow))]

	RsurfNoSnow <- RsurfNoSnow[,match(edges[, edgeIdField], colnames(RsurfNoSnow))]
	Gw <- Gw[,match(edges[, edgeIdField], colnames(Gw))]
	Tair <- Tair[,match(edges[, edgeIdField], colnames(Tair))]


    # Set the timeLength to avoid re-executing nrow function
    timeLength <- nrow(RsurfSnow) 
    # Create seed matrix to use for storing data in results


    seedMatrix <- matrix(0, nrow=timeLength, ncol=ncol(Rsurf),
        dimnames=list(rownames(Rsurf), edges[, edgeIdField]))



    results <- list(
        Twater = seedMatrix,
        TwaterLocal = seedMatrix,
        TwaterInitial = seedMatrix,
    )

    rm(seedMatrix)
    
    print("About to Run")
	spinUpCycles <- 0
	cycleCount <- 0
			
	stepsToLoop <- nrow(RsurfSnow)
	for(timeStep in 1:stepsToLoop){
		#print(paste("Cycle:", cycleCount, "Timestep:",  timeStep))
		# Start time to use for fucntion time-estimates
		start <- Sys.time()

		# Loop through edges in river network
		for(i in 1:nrow(edges)){
			#print(i)

			# Set hydroID of edges so don have to keep subsetting
			hydroID <- edges[i,edgeIdField]
			
			
			qSnow <- RsurfSnow[timeStep, hydroID]


			qNoSnow <- RsurfNoSnow[timeStep, hydroID]
			qGw <- simFlow$qSub[timeStep, hydroID]
			Tgw <-  ###often 1-2*C higher than mean annual air temperature of a region, annual time series
			TairLocal <- Tair[timeStep, hydroID]

			if(by == "month"){
				TairLag <- TairLocal
				lamda <- 1
			} else {
				TairLag #### Need to make some sort of lag for daily air temp
				lamda ####
			}

			qTotal <- (qSnow+qGw+qNoSnow)

			# temperature of the water before effects of air and upstream temperature
			TwaterLocal <- ((Tsnow*qSnow)+(Tgw*qGw)+(lamda*TairLag)(qNoSnow))/qTotal

			#May need to calculated weighted temperatures
			qInUpstream <- simFlow$qOut[timeStep, edges[edges[, edgeNextDownField] == edges[i, edgeIdField], edgeIdField]]

			TwaterIn <- results$Twater[timeStep, edges[edges[, edgeNextDownField] == edges[i, edgeIdField], edgeIdField]]
			TwaterUpstream <- sum(qInUpstream*TwaterIn)/sum(TwaterIn)


			qGw <- simFlow$qSub[timeStep, hydroID]
			qOut <- simFlow$qOut[timeStep, hydroID]

			if(edges[i, edgeOrderField] == 1){
				TwaterInitial <- TwaterLocal
			} else {
				TwaterInitial <- (TwaterUpstream(qOut - qTotal) + TwaterLocal*qTotal)/qOut
			}

			reachTravelTime <- simFlow$TT[timeStep, hydroID] ## TT (hour) travel time of water through the subbasain 
			## Need to modify k
			K <- .5##K (1/h) is a bulk coefficient of heat transfer and ranges from 0 to 1

			if(TairLocal > 0){
				Twater <- TwaterInitial + (TairLocal - Tinitial) * K * (reachTravelTime)
			} else {
				epsilon <- 
				Twater <- TwaterInitial + ((TairLocal + epsilon) - Tinitial) * K * (reachTravelTime)
			}


			# Store values in results list
			results$Twater[timeStep, hydroID] <- Twater

			if(outputExtraVars){
				results$TwaterLocal[timeStep, hydroID] <- TwaterLocal
				results$TwaterInitial[timeStep, hydroID] <- TwaterInitial
			}
			
			if(debugMode){
				# DebugMode in order to debug problems
				print(" ")
				print(paste("Day:",timeStep, "Edge:", hydroID, "Order:", edges[i, edgeOrderField]))
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



		if(cycleCount == 0 & timeStep == 10){

		print(paste("ETA:", 
				#round((Sys.time()-start) * (timeLength*(spinUpCycles1+1)length((timeStep+1):timeLength)/10*(spinUpCycles+1), 2), 
				round((Sys.time()-start) * (timeLength + spinUpCycles*spinUpYears*ifelse(by == "day",365, 12))/10), 
				"seconds or", 
				#round((Sys.time()-start)*length((timeStep+1):timeLength)/10/60*(spinUpCycles+1), 2), 
				round((Sys.time()-start) * (timeLength + spinUpCycles*spinUpYears*ifelse(by == "day",365, 12))/10/60), 
				"minutes. Will finish at", 
				(Sys.time()-start)*length((timeStep+1):timeLength)/10*(spinUpCycles+1)+Sys.time()
			))
		}
    }	
    return(results)
}
