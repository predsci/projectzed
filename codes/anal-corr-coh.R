rm(list=ls())
graphics.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# anal-corr-coh.R - Routine for all panels in Figures 3-7, 9 and 10 
# All the plots will be created in the 'subDir' directory set below
# For different plots we need different data sets and this dictates the
# order that we make the plots.  We load a data set and use it to make all
# the require figures.  
# Each data set has its own directory with csv files in the 'results'
# directory
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(RColorBrewer)
library(hexbin)

## set directory name for plots and create it if needed
subDir = "../plots"

## basename for the results directory 
resultsBase ='../results/'
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")

if (!dir.exists(subDir)) {
	dir.create(subDir)
	cat("\n\n Created Directory for Output Plots: ", subDir, "\n\n")
}

## Current directory
workDir = getwd()

## Remove dataset if it exists 
if(exists("dataset")) {rm(dataset)}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## To create Figures 3, 4, 5a and 6a 
## Set the variable name to Bn and the forecast window to 24 hours

whvar = "Bn"  #Bn, vr, Temp, np, Bt, Br

wwindow = 24

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName) 

file_list <- list.files(pattern=".csv")

cat("\n\nLoading Results for Figures 3, 4, 5a and 6a\n")

for (file in file_list) {
	# if the merged dataset doesn't exist, create it
	if (!exists("dataset")) {
		dataset <- read.csv(file, header = TRUE)
	} else { # if the merged dataset does exist, append to it
		temp_dataset <- read.csv(file, header = TRUE)
		dataset <- rbind(dataset, temp_dataset)
		rm(temp_dataset)
	}
}


## Set working directory back to working directory

setwd(workDir)


## replace the column name  'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 3 - this takes a while 
## create a 5x5 scatterplot

filename = paste0(subDir,'/figure3.pdf')

cat("\nFor Results Plot See: ", filename, "\n")

pdf(file=filename)
plot(dataset[,c(4,5,6,7,8)])
err <- dev.off()

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure  4
## High Density Scatterplot with Binning

x <- dataset$mse
y <- dataset$mseBaseline
mtit <- "Hexagonal Binning: MSE v. MSE-Baseline"


xtit <- expression("MSE" ~(nT^{2}))
ytit <- expression("MSE-Baseline" ~(nT^{2}))


xmax = 10 # for Bn

xsub = ((x <= xmax) & (y <= xmax))
bin<-hexbin(x[xsub], y[xsub], xbins=50,xbnds=c(0,xmax), ybnds=c(0,xmax)) 

# nice color version

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

filename = paste0(subDir,'/figure4.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin, main=mtit,xlab=xtit,ylab=ytit,colramp=rf)
err <- dev.off()



## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure  5a
## Ratio of MSE to MSE-baseline

ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 500

filename = paste0(subDir,'/figure5a.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- dev.off()

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure  6a
## Ratio of LSV vs Predicted correlation
## High Density Scatterplot with Binning
x <- dataset$lsv
y <- dataset$corrPred
mtit <- "Hexagonal Binning: LSV v. Predicted Correlation"
xtit <- "LSV"
ytit <- "Predicted Correlation"
bin<-hexbin(x, y, xbins=50) 
filename = paste0(subDir,'/figure6a.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin,xlab=xtit,ylab=ytit,main=mtit)
err <- dev.off()


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figures 5b and 6b
## Same as Figure 5a but for a forecast window of 6 hours for Bn


## Clean the data set 

if (exists("dataset")){rm(dataset)}

## Set the variable name to Bn and the forecast window to 6 hours

whvar = "Bn"  

wwindow = 6

workDir = getwd()

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName) 

file_list <- list.files(pattern=".csv")

cat("\nLoading Results for Figures 5b and 6b\n")

for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

## Set working directory back to working directory

setwd(workDir)

## replace the column name  'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 5b mse/mseBaseline Ratio

x <- dataset$mse
y <- dataset$mseBaseline
ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 500

filename = paste0(subDir,'/figure5b.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- dev.off()

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure  6b
## Ratio of LSV vs Predicted correlation
## High Density Scatterplot with Binning
x <- dataset$lsv
y <- dataset$corrPred
mtit <- "Hexagonal Binning: LSV v. Predicted Correlation"
xtit <- "LSV"
ytit <- "Predicted Correlation"
bin<-hexbin(x, y, xbins=50) 
filename = paste0(subDir,'/figure6b.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin,xlab=xtit,ylab=ytit,main=mtit)
err <- dev.off()


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 7a and 7b use variable is vr and forecast window is 24 hours
##

## Clean the data set 

if (exists("dataset")){rm(dataset)}


## Set the variable name to Bn and the forecast window to 6 hours

whvar = "vr"  

wwindow = 24

workDir = getwd()

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName) 

file_list <- list.files(pattern=".csv")

cat("\nLoading Results for Figure 7a and 7b\n")

for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

## Set working directory back to working directory

setwd(workDir)

## replace the column name  'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names

## Need to remove all lines with -999.0

rmv = -999.0
dataset <- dataset[!(dataset[,4] %in% rmv),]	

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 7a
## Hexagonal Binning

x <- dataset$mse
y <- dataset$mseBaseline

	xmax = 1e4 
	xtit <- expression("MSE" ~(km^{2}/s^{2}))
	ytit <- expression("MSE-Baseline" ~(km^{2}/s^{2}))


xsub = ((x <= xmax) & (y <= xmax))
bin<-hexbin(x[xsub], y[xsub], xbins=50,xbnds=c(0,xmax), ybnds=c(0,xmax)) 

mtit <- "Hexagonal Binning: MSE v. MSE-Baseline"

# nice colour version

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

filename = paste0(subDir,'/figure7a.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin, main=mtit,xlab=xtit,ylab=ytit,colramp=rf)
err <- dev.off()

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 7b
## Ratio mse/mseBaseline
ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 800

filename = paste0(subDir,'/figure7b.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- dev.off()

## 
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Inset for Figure 7b - variable is 'vr' and forecast period 6 hours

## Clean the data set 

if (exists("dataset")){rm(dataset)}

whvar = "vr"  

wwindow = 6

workDir = getwd()

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName) 

file_list <- list.files(pattern=".csv")

cat("\nLoading Results for Figure 7b-inset\n")

for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

## Set working directory back to working directory

setwd(workDir)

## replace the column name  'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names

## Need to remove all lines with -999.0

rmv = -999.0
dataset <- dataset[!(dataset[,4] %in% rmv),]	

x <- dataset$mse
y <- dataset$mseBaseline

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## For Figure 7b inset  - ratio mse to mseBaseline
ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 800

filename = paste0(subDir,'/figure7b-inset.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- dev.off()


## 
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figures 9a and 9b, variable is 'np' and forecast window is 24 hours
##
if (exists("dataset")){rm(dataset)}

whvar = "np"  

wwindow =24

workDir = getwd()

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName) 

file_list <- list.files(pattern=".csv")

cat("\nLoading Results for Figures 9a and 9b\n")

for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

# Need to remove all lines with -999.0

rmv = -999.0
dataset <- dataset[!(dataset[,4] %in% rmv),]	

## Set working directory back to working directory

setwd(workDir)

## replace the column name  'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# For Figure 9a - Hexagonal Binning

x <- dataset$mse
y <- dataset$mseBaseline

xmax = 50 #use 50 for figure 9a and 20 for 9c
xtit <- expression("MSE" ~(N^{2}/cm^{6}))
ytit <- expression("MSE-Baseline" ~(N^{2}/cm^{6}))	

xsub = ((x <= xmax) & (y <= xmax))
bin<-hexbin(x[xsub], y[xsub], xbins=50,xbnds=c(0,xmax), ybnds=c(0,xmax)) 

# nice colour version

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

filename = paste0(subDir,'/figure9a.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin, main=mtit,xlab=xtit,ylab=ytit,colramp=rf)
err <- err <- dev.off()

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figure 9b - ratio mse to mseBaseline

ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 5000

filename = paste0(subDir,'/figure9b.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- err <- dev.off()

## 
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figures 9c and 9d, variable is 'np' and forecast window is 6 hours
##

if (exists("dataset")){rm(dataset)}

whvar = "np"  

wwindow =6

workDir = getwd()

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName) 

file_list <- list.files(pattern=".csv")

cat("\nLoading Results for Figures 9c and 9d\n")

for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

# Need to remove all lines with -999.0

rmv = -999.0
dataset <- dataset[!(dataset[,4] %in% rmv),]	

## Set working directory back to working directory

setwd(workDir)

## replace the column name  'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figure 9c - Hexagonal Binning 

x <- dataset$mse
y <- dataset$mseBaseline

xmax = 20 #use 50 for figure 9a and 20 for 9c
xtit <- expression("MSE" ~(N^{2}/cm^{6}))
ytit <- expression("MSE-Baseline" ~(N^{2}/cm^{6}))	

xsub = ((x <= xmax) & (y <= xmax))
bin<-hexbin(x[xsub], y[xsub], xbins=50,xbnds=c(0,xmax), ybnds=c(0,xmax)) 

# nice colour version

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

filename = paste0(subDir,'/figure9c.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin, main=mtit,xlab=xtit,ylab=ytit,colramp=rf)
err <- dev.off()

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Figure 9d  - ratio mse to mseBaseline

ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 4000

filename = paste0(subDir,'/figure9d.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- dev.off()



## 
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figures 10a and 10b, variable is 'Temp' and forecast window is 24 hours
##

whvar = "Temp"  #Bn, vr, Temp, np, Bt, Br

wwindow = 24

if (exists("dataset")){rm(dataset)}

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName) 

file_list <- list.files(pattern=".csv")

cat("\nLoading Results for Figures 10a and 10b\n")
 
for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

setwd(workDir)

## Need to remove all lines with -999.0 or NA

rmv = -999.0
dataset <- dataset[!(dataset[,4] %in% rmv),]	
dataset = na.omit(dataset)


## replace 'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names


x <- dataset$mse
y <- dataset$mseBaseline

xmax = 1e10
xtit <- expression("MSE" ~(K^{2}))
ytit <- expression("MSE-Baseline" ~(K^{2}))	

xsub = ((x <= xmax) & (y <= xmax))
bin<-hexbin(x[xsub], y[xsub], xbins=50,xbnds=c(0,xmax), ybnds=c(0,xmax)) 

# nice colour version

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

filename = paste0(subDir,'/figure10a.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin, main=mtit,xlab=xtit,ylab=ytit,colramp=rf)
err <- dev.off()

# Figure 10b 

ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 600

filename = paste0(subDir,'/figure10b.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- dev.off()


## 
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Figures 10a and 10b, variable is 'Temp' and forecast window is 6 hours
##

whvar = "Temp" 

wwindow = 6

if (exists("dataset")){rm(dataset)}

## Name and location of directory with results needed for this choice
dirName = paste0(resultsBase,"corr-coh-",whvar,'-',wwindow,'hr/')

setwd(dirName)  

file_list <- list.files(pattern=".csv")

cat("\nLoading Results for Figures 10c and 10d\n")
 
for (file in file_list){
       
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE)
  }
   
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
 
}

setwd(workDir)

## Need to remove all lines with -999.0 or NA

rmv = -999.0
dataset <- dataset[!(dataset[,4] %in% rmv),]	
dataset = na.omit(dataset)


## replace 'coh' with 'lsv' (Large Scale Variation)
## and mseZero with mseBaseline

dataset.names <- colnames(dataset)
index <- which(dataset.names == 'coh')
dataset.names[index] = 'lsv'
colnames(dataset) <- dataset.names
index <- which(dataset.names == 'mseZero')
dataset.names[index] = 'mseBaseline'
colnames(dataset) <- dataset.names


x <- dataset$mse
y <- dataset$mseBaseline

xmax = 1e10
xtit <- expression("MSE" ~(K^{2}))
ytit <- expression("MSE-Baseline" ~(K^{2}))	

xsub = ((x <= xmax) & (y <= xmax))
bin<-hexbin(x[xsub], y[xsub], xbins=50,xbnds=c(0,xmax), ybnds=c(0,xmax)) 

# nice colour version

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))

filename = paste0(subDir,'/figure10c.pdf')
cat("\nFor Results Plot See: ", filename, "\n")
pdf(file=filename)
plot(bin, main=mtit,xlab=xtit,ylab=ytit,colramp=rf)
err <- dev.off()

# Figure 10d 

ratioMSE = x/y
xr1 = c(0,2.)
nbreak = 1000

filename = paste0(subDir,'/figure10d.pdf')
cat("\nFor Results Plot See: ", filename, "\n\n")
pdf(file=filename)
hist(ratioMSE,breaks=nbreak,xlab="MSE / MSE-Baseline",xlim=xr1,main="Ratio of MSE to MSE-Baseline")
err <- dev.off()
cat("\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
## 
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##



