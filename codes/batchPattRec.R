# batchPattRec.R

##
## Driver for generating stats/results for the patt rec. model - Using Rscript from the Command Line 
## Use this driver with the provided Python script: run-batchPattRec.py
## This driver calls the Batch version for pattern recognition which is in the pattRec.R file
## It also uses the OMIN data reading routine which is on util.R
## This driver can be invoked by the python script which can launch 12 processed at once, one per month
##


rm(list=ls())

## Set default values for all parameters - these are being reset when the script is called
## with different values

## Year
year  = 2000

## Month
month  = 01

## var
whvar = 'Bn'

## Window
wwindow = 24

## Load libraries and required R functions

library('batch')
library('Metrics')
source('pattRec.R')
source("util.R")

## args will be a data frame 
args = parseCommandArgs()

if(length(args) == 0) {
	cat("
	year      year for the analysis
	month     month for the analysis
	whvar     the variable to analyze 
	wwindow    window in hours 
	Example: \n
	Rscript batchPattRec.R year 2010 month 01 whvar Bn wwindow 24
	\n\n
	")
	q(save='no')
} else {
	if ('year'   	%in% names(args))  year = args$year	
	if ('month'   	%in% names(args))  month = args$month	
 }

## start the clock 
start.time <- proc.time()

## call the Batch version of the pattern recongition
## It will apply the procedure for a given variable (whvar) for 
## each data point in a given year/month. 
## Only past data is used 
#

output <- pattRecBatch(year=year, month=month, whvar=whvar,wwindow=wwindow)

# stop the clock        
cat("\n\nElapsed Time: \n\n")
end.time = proc.time()
run.time = end.time-start.time
print(end.time - start.time)





 

