rm(list = ls())

graphics.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# forecast.R - Routine for Making Figures 1, 2a, 2b and 8a-8d
# It uses the util.R which is a collection of R functions 
# All the plots will be created in the 'subDir' directory set below
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## load necessary libraries
library(date)
library(hydroGOF)
library(Hmisc)
library(zoo)

## load the R functions we use

source("util.R")

## set the Flag to read the OMNI data base - this is done only once

readDataFlag = T

## set the name of the data set

sDataSet = "OMNI"

## Set plotting on

plotRealizations = "yes"
plotQuantiles = "yes"

## Turn on the Baseline and PR models

baseline = T
pattrec = T

models = c(baseline, pattrec)
smodels = c("Baseline", "Patt. Rec.")

## set pdf to TRUE or FALSE
## If set to FALSE - print to screen
## If set to TRUE the filename is set properly for each event/figure

pdf = TRUE

## set directory name for plots and create it if needed
subDir = "../plots"

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

if (!dir.exists(subDir)) {
	dir.create(subDir)
	cat("\n\n Created Directory for Output Plots: ", subDir, "\n\n")
}


## not used - but can be activated
nsmooth = 0

## for plotting routines
plotRealizations = "yes"
plotQuantiles = "yes"

## To create Figures 1, 2a, 2b, and 10a-d the code will go through events 1-7
## If you would like to createa a single Figure or a subset of Figures change 'eventVec' and figureVec accordingly
## event = 1 Figure 1
## event = 2 Figure 2a
## event = 3 Figure 2b
## event = 4-7 Figures 8a, 8b, 8c and 8d

eventVec =  1:7

figureVec = c("1", "2a", "2b", "8a", "8b", "8c", "8d")

nevents = length(eventVec)


## load the OMNI_M data base

cat("\n\n Loading the OMNI_M 1-hour resolution data base \n\n")

if (readDataFlag == T) {

	readData = readMagV_OMNI() # read in the data
	ddate = readData$ddate
	Bn = readData$Bn
	vr = readData$vr
	np = readData$np
	Temp = readData$Temp
	Br = readData$Br
	Bt = readData$Bt

	## Change outliers to NA's
	Bn[Bn == 999.9] = NA
	vr[vr == 9999] = NA
	np[np == 999.9] = NA
	Temp[Temp == 9999999] = NA
	Br[Br == 999.9] = NA
	Bt[Bt = 999.9] = NA

}


for (ievent in 1:nevents) {
	event = eventVec[ievent]
	cat("\nProcessing Figure: ", figureVec[ievent], "\n")
	if (event == 1) { # Figure 1

		ymd = "2000-09-18"
		filename = paste0(subDir, "/figure1.pdf")
		plotB = "yes"
		plotV = 'no'
		hhour = 4
		mmin = 0
		ssec = 0
		wwindow = 24
		bmax = 20
	}

	if (event == 2) { ## Upper panel of Figure 2

		ymd = "2011-03-30"

		filename = paste0(subDir, "/figure2a.pdf")
		plotB = "yes"
		plotV = 'no'
		hhour = 12
		mmin = 0
		ssec = 0
		wwindow = 24
		bmax = 30
	}

	if (event == 3) { ## Lower panel of Figure 2

		ymd = "2012-06-17"

		filename = paste0(subDir, "/figure2b.pdf")
		plotB = "yes"
		plotV = 'no'
		hhour = 8
		mmin = 0
		ssec = 0
		wwindow = 24
		bmax = 30
	}

	## Four events for Figure 8
	
	if (event == 4) {

		ymd = "1996-08-22" # Top Left panel-a
		filename = paste0(subDir, "/figure8a.pdf")
		plotV = "yes"
		plotB = 'no'
		hhour = 0
		mmin = 0
		ssec = 0
		wwindow = 24
		bmax = 30
	}

	if (event == 5) {

		ymd = "2002-08-20" # Top Right panel-b
		filename = paste0(subDir, "/figure8b.pdf")
		plotV = "yes"
		plotB = 'no'
		hhour = 0
		mmin = 0
		ssec = 0
		wwindow = 24 * 6
		bmax = 30
	}

	if (event == 6) {

		ymd = "2008-7-21" # Bottom left panel-c 
		filename = paste0(subDir, "/figure8c.pdf")
		plotV = "yes"
		plotB = 'no'
		hhour = 0
		mmin = 0
		ssec = 0
		wwindow = 24 * 12
		bmax = 30
	}

	if (event == 7) {

		ymd = "1996-05-30" # Bottom Right panel-d
		filename = paste0(subDir, "/figure8d.pdf")
		plotV = "yes"
		plotB = 'no'
		hhour = 0
		mmin = 0
		ssec = 0
		wwindow = 24 * 40
		bmax = 30
	}

	## Open PDF file if requested by User
	if (pdf == TRUE) {
		pdf(file = filename, width = 8, height = 7)
	} else {
		dev.new()
	}

	## convert the date and time to POSIX value XXX
	date1 = ISOdate(ymd, hhour, mmin, ssec, tz = "UTC")

	## calculate and plot the Bn model results - for Figures 1 and 2a/2b
	
	if (plotB == "yes") {
		plotDataBn = plotMag(ddate, Bn, date1 = date1, wwindow = wwindow, models = models, smodels = smodels, nsmooth = nsmooth, bmax = bmax, sc = sDataSet, plotRealizations = plotRealizations, 
			plotQuantiles = plotQuantiles)
	}


	## calculate and plot the vr model results - for Figure 10a,b,c,d
	
	if (plotV == "yes") {
		plotDatavr = plotVr(ddate, vr, date1 = date1, wwindow = wwindow, models = models, smodels = smodels, nsmooth = nsmooth, sc = sDataSet, plotRealizations = plotRealizations, plotQuantiles = plotQuantiles)
	}

	if (pdf == TRUE) {
		err <- dev.off()
		cat("\n\n For Results Plot See: ", filename, "\n\n")
	}

}

cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
