#!/bin/bash

########################
# NetCDF file merge
# 
# Original from https://code.zmaw.de/boards/1/topics/26
# by Tom Akkermans
# tom.akkermans@ees.kuleuven.be

# Modification by Kristen Emmett
# July 2015

# Create or update a grid_merge.txt with info about the lon/lat dimensions
# you want for the master file that will encompass your entire study area

# Make an enlarged file with info from grid_merge.txt
# Use one of your output files with data as a template for variable
# in your master file
#cdo enlarge,SR_grid_merge.txt Subset_1/output_1/stand_daily_subset_1.nc file4.nc

#cdo setrtomiss,-1000000,10E45 file4.nc file5.nc

# Remove temp files
#rm file4.nc

# Start with Subset_1
# Merge enlarged domain with content of output files into a temp file
#let nprocs=32 #number of processor cores per nodes
#let index=1
#let number=5
#let next=6
#let again=7

# Reset counter
#let counter=1

#	while (($counter < $nprocs+1))
#	do
#	cdo mergegrid file$number.nc output_$index/stand_daily_subset_$index.nc file$next.nc
#	cdo setrtomiss,10E4,10E50 file$next.nc file$again.nc
	
#	echo $index
	# Temp file cleanup
#	rm file$number.nc
#	rm file$next.nc
	
	# Reset counters
#	let counter=$counter+1
#	let index=$index+1
#	let number=$number+2
#	let next=$next+2
#	let again=$again+2
#	done

#exit

# Merge enlarged domain with content of first file with a temp file
#let nprocs=32 #number of processor cores per nodes
#let index=1
#let number=5
#let next=6
#let again=7

# For Subset_2 file path and run:

# Merge enlarged domain with content of first file with a temp file
let nprocs=32 #number of processor cores per nodes
let index=33 #first number in output file name in this Subset
let number=69 #set number to the final number file from Subset_1
let next=70 #set to whatever $number is set to plus one (do not do let next=$number+1,
#actually type in the integer)
let again=71 #set to whatever $next is set to plus one

# Reset counter
let counter=1

	while (($counter < $nprocs+1))
	do
	cdo mergegrid file$number.nc Subset_2/output_$index/stand_daily_subset_$index.nc file$next.nc
	cdo setrtomiss,10E4,10E50 file$next.nc file$again.nc
	
	echo $index
	# Temp file cleanup
	rm file$number.nc
	rm file$next.nc
	
	# Reset counters
	let counter=$counter+1
	let index=$index+1
	let number=$number+2
	let next=$next+2
	let again=$again+2
	done

# Change file number here!! file?? whatever you last file created is called
cdo setrtomiss,10E4,10E50 file133.nc merged.nc