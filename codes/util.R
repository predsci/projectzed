#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#library routines for forecast driver 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

outlierReplace <- function(dataframe, cols, rows, newValue = NA) {
	if (any(rows)) {
		set(dataframe, rows, cols, newValue)
	}
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

smooth <- function(x, n = 3) {
	filter(x, rep(1/n, n), sides = 2)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

pattRec <- function(ddate, Bn, date1 = date1, wwindow = wwindow, bmax = bmax, plotRealizations = "yes") {

	dn = wwindow/2

	# rather than removing the NA's, interpolate over them: 
	Bn = na.approx(Bn, na.rm = F)

	ntot = length(Bn)
	n1 = min((1:ntot)[ddate == date1])

	eucDist = Bn * 0 + NA

	x1 = Bn[(n1 - 2 * dn):n1]

	## ensure that the data we scan can be used for a window prediction forward - that is we have data for (n1-2*dn)+wwindow
	
	for (i in (1 + dn):(n1 - 2 * dn)) {
		x2 = Bn[(i - dn):(i + dn)]
		tmp = (x2 - x1) * (x2 - x1)
		eucDist[i] = sqrt(sum(tmp))
		#eucDist[i] = dist(rbind(x1,x2),method="euclidean") # The above is almost x10 faster
		}

	eucDistRaw = eucDist
	eucDist[eucDist == 0] = 999999
	eucDist[is.na(eucDist)] = 999999
	npatt = 50 # find top npatt locations
	orderEucDist = order(eucDist, method = "radix")

	## In the case of cor need to order in decreasing and repalce 999 with -999 above
	##orderEucDist = order(eucDist,method='radix',decreasing=TRUE)

	meanBnPred = rep(0, (1 + 2 * dn))
	BnRealizations = matrix(0, length(meanBnPred), npatt)

	for (j in 1:npatt) {
		iEuc = orderEucDist[j]
		meanBnPred = Bn[(iEuc + dn):(iEuc + 3 * dn)] + meanBnPred
		BnRealizations[, j] = Bn[(iEuc + dn):(iEuc + 3 * dn)]
	}

	meanBnPred = meanBnPred/npatt

	if (plotRealizations == "yes") {
		for (j in 1:npatt) {
			iEuc = orderEucDist[j]
			lines(ddate[(n1 - dn * 2):(n1 + dn * 2)], Bn[(iEuc - dn):(iEuc + 3 * dn)], col = "grey", type = "l")
		}
	}

	BnFuture = Bn[n1:(n1 + 2 * dn)]

	lines(ddate[(n1 - 2 * dn):n1], Bn[(n1 - 2 * dn):n1], col = "black", lwd = 1, type = "l")

	corrCoeff = cor(meanBnPred, BnFuture, use = "complete")

	result <- list(meanBnPred = meanBnPred, BnRealizations = BnRealizations)
	return(result)
}



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plotMag <- function(ddate, Bn, date1, wwindow = 24, models, smodels, nsmooth = 0, bmax = 20, sc = "OMNI", plotRealizations = "yes", plotQuantiles = "yes") {

	err = 0 # set the error flag to zero

	# Plot the data
	
	symSize = 0.25
	bleglower = 0 - bmax/2
	cex1 = 0.8
	mlwd = 2
	xmin = date1 - wwindow * 3600
	xmax = date1 + wwindow * 3600
	BnNow = Bn[ddate == date1]
	
	BnBaseline = 0 * Bn
	nBn = length(Bn)

	ipred = ((ddate >= date1) & (ddate <= xmax))
	isub = ((ddate >= xmin) & (ddate <= xmax))
	imodels = (models == T)

	obs51 = 0
	if (min(Bn[ipred]) < -5) {
		obs51 = 100
	}

	plot(ddate[isub], Bn[isub], ylim = c(-bmax, bmax), ylab = "Bn (nT)", cex = symSize, xlab = "Time (Date)", xlim = c(xmin, xmax), type = "n", xaxt = "n", lwd = 1)
	axis.POSIXct(1, at = seq(xmin, xmax, by = "day"), format = "%m/%d/%y")


	# add in some model predictions:
	# Here's the order: baseline,pattrec
	nipred = length(Bn[ipred])
	cmodels = c("red", "purple")
	corrVec = 0 * vector("numeric", length(models))
	probFor = 0 * vector("numeric", length(models))
	MSE = 0 * vector("numeric", length(models))

	if (models[2] == T) {
		arrPattRec = pattRec(ddate, Bn, date1 = date1, wwindow = wwindow, bmax = bmax, plotRealizations = plotRealizations)
		meanBnPred = arrPattRec$meanBnPred
		BnRealizations = arrPattRec$BnRealizations
		nrow = dim(BnRealizations)[1]
		ncol = dim(BnRealizations)[2]
		probForAll = vector("numeric", ncol)

		for (i in 1:(ncol)) {
			if (min(BnRealizations[, i]) < -5) {
				probForAll[i] = 100
			}
		}

		# compute quantiles
		quantArr = matrix(0, 5, nrow)
		for (i in 1:(nrow)) {
			quantOne = quantile(BnRealizations[i, ])
			quantArr[, i] = quantOne
		}
		ypoly = c(quantArr[2, ], rev(quantArr[4, ]))
		xpoly = c(ddate[ipred], rev(ddate[ipred]))
		if (plotQuantiles == "yes") {
			polygon(xpoly, ypoly, col = rgb(0, 1, 0, 0.3), border = NA)
		}
		points(ddate[ipred], meanBnPred, col = cmodels[5], lwd = mlwd, type = "l")
		if (plotQuantiles == "yes") {
			text(xmax - (xmax - xmin)/8, -bmax, "25/75% Quantiles", cex = cex1, col = "green3")
		}
		##if (plotQuantiles=="yes") {text(xmax-(xmax-xmin)/2.75,-bmax,"25/75% Quantiles",cex=cex1,col="green3")}
		corrVec[2] = cor(Bn[ipred], meanBnPred, method = "pearson", use = "complete")
		probFor[2] = sum(probForAll)/ncol
		MSE[2] = mse(meanBnPred, Bn[ipred], na.rm = T)
	}


	if (models[1] == T) {
		lines(ddate[ipred], BnBaseline[ipred], type = "l", col = cmodels[1], lwd = mlwd)
		corrVec[1] = NA # since there is no variance/sd  this call just produces NA cor(Bn[ipred], BnBaseline[ipred], method = "pearson", use = "complete")
		probFor[1] = 0
		MSE[1] = mse(BnBaseline[ipred], Bn[ipred], na.rm = T)
	}

	skill = (1 - MSE/MSE[1]) ##*100.

	lines(ddate[isub], Bn[isub], type = "l", lwd = 1) # plot again to overlay other plots
	lines(c(date1, date1), c(-bmax, bmax), type = "l", col = "red", lwd = 3)
	lines(c(min(ddate) - 2, max(ddate) + 2), c(0, 0), type = "l", lty = 2, lwd = 1)
	lines(ddate[ipred], -5 + BnBaseline[ipred], type = "l", lty = 2, lwd = 3, col = "red")

	legend(xmin, bmax, lty = replicate(length(models[imodels]) + 1, 1), c(sc, smodels[imodels]), lwd = replicate(length(models[imodels]) + 1, 2.5), text.col = c("black", cmodels[imodels]), 
		col = c("black", cmodels[imodels]), cex = cex1, title = "Observations/Models", title.col = "black")
	legend("topright", legend = date1, bty = "n")


	legend(xmin, bleglower, paste(smodels[imodels], ": ", format(corrVec[imodels], digits = 2), sep = ""), text.col = cmodels[imodels], cex = cex1, title = "Corr. Coeff.", title.col = "black")
	legend(xmin + (xmax - xmin)/4, bleglower, paste(smodels[imodels], ": ", format(MSE[imodels], digits = 3), sep = ""), text.col = cmodels[imodels], cex = cex1, title = "MSE", title.col = "black")
	legend(xmin + (xmax - xmin)/1.95, bleglower, paste(smodels[imodels], ": ", format(skill[imodels], digits = 2), sep = ""), text.col = cmodels[imodels], cex = cex1, title = "Skill Score", 
		title.col = "black")
	legend(xmin + (xmax - xmin)/1.33333, bleglower, paste(c(smodels[imodels], "Observations"), ": ", format(c(probFor[imodels], obs51), digits = 3), "%", sep = ""), text.col = c(cmodels[imodels], 
		"black"), cex = cex1, title = "Prob.Forecast (>5/1)", title.col = "black")

	return(err)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plotVr <- function(ddate,vr,date1=date1,wwindow=wwindow,
                  models=models,smodels=smodels,
                  nsmooth=nsmooth,sc="OMNI",plotRealizations= 'yes', 
				  plotQuantiles='yes') {

  
  err = 0 # set the error flag to zero
  # Plot the data
  
  vmin = 200 # set up some default plotting parameters
  vmax = 800
  
  symSize = 0.25
  cex1    = 0.8
  mlwd=2
  xmin = date1 - wwindow*3600.
  xmax = date1 + wwindow*3600.
  vrNow = vr[ddate==date1]
  
  ipred = which((ddate >= date1) & (ddate <= xmax))
  isub  = which((ddate >=xmin) & (ddate <= xmax))
  
  ipast = which((ddate >=xmin) & (ddate <= date1))
  ## The baseline model is the avarge value of 'vr' during the past window
  vrBaseline = 0.0*vr + mean(vr[ipast],na.rm=TRUE)
  imodels = (models == T) 
  
  
   obs5001 = 0.0
  if (max(vr[ipred]) > 500.0) {obs5001 = 100.}
 
  
  plot(ddate[isub],vr[isub],ylim=c(vmin,vmax),
       xlab="Time (Date)",ylab="vr (km/s)",cex=symSize,
       xlim=c(xmin,xmax),type="n",xaxt="n")
  if (wwindow >= 24) {
  	axis.POSIXct(1, at = seq(xmin,xmax, by = "day"), format = "%m/%d/%y")	
  } else {
  	axis.POSIXct(1, at = xmin, format = "%m/%d/%y")
  	axis.POSIXct(3, at = seq(xmin,xmax, by = "hour"), format = "%H")
  }
  

  lines(c(date1,date1),c(vmin,vmax),type="l",col="red",lwd=3)
  lines(c(min(ddate)-2,max(ddate)+2),c(0,0),type="l")
  
  # add in some model predictions:
  # Here's the order: baseline,pattrec
  nipred  = length(vr[ipred])
  cmodels = c("red","purple")
  corrVec = 0.0*vector("numeric",length(models))
  probFor = 0.0*vector("numeric",length(models))
  MSE     = 0.0*vector("numeric",length(models))
       
 if (models[2] == T) {

 	arrPattRec = pattRec(ddate, Bn = vr, date1 = date1, wwindow = wwindow, bmax = vmax, plotRealizations = plotRealizations)
 	meanVrPred = arrPattRec$meanBnPred
 	VrRealizations = arrPattRec$BnRealizations
 	nrow = dim(VrRealizations)[1]
 	ncol = dim(VrRealizations)[2]
 	probForAll = vector("numeric", ncol)

 	nrow = dim(VrRealizations)[1]
 	ncol = dim(VrRealizations)[2]
 	for (i in 1:(ncol)) {
 		if (max(VrRealizations[, i]) > 500) {
 			probForAll[i] = 100
 		}
 	}

 	# compute quantiles
 	quantArr = matrix(0, 5, nrow)
 	for (i in 1:(nrow)) {
 		quantOne = quantile(VrRealizations[i, ])
 		quantArr[, i] = quantOne
 	}

 	ypoly = c(quantArr[2, ], rev(quantArr[4, ]))
 	xpoly = c(ddate[ipred], rev(ddate[ipred]))
 	if (plotQuantiles == "yes") {
 		polygon(xpoly, ypoly, col = rgb(0, 1, 0, 0.3), border = NA)
 		text(xmax - (xmax - xmin)/8, vmin, "25/75% Quantiles", cex = cex1, col = "green3")
 	}
 	points(ddate[ipred], meanVrPred, col = cmodels[5], lwd = mlwd, type = "l")
 	corrVec[2] = cor(vr[ipred], meanVrPred, method = "pearson", use = "complete")
 	probFor[2] = sum(probForAll)/ncol
 	MSE[2] = mse(meanVrPred, vr[ipred], na.rm = T)

 }    

 if (models[1] == T) {
 	dn = wwindow/2
 	ntot = length(vr)
 	n1 = min((1:ntot)[ddate == date1])
 	x1 = vr[(n1 - 2 * dn):n1]
 	vrBaseline = 0 * vr + mean(x1, na.rm = TRUE)
 	lines(ddate[ipred], vrBaseline[ipred], type = "l", col = cmodels[1])
 	corrVec[1] = corrVec[1] = NA # since there is no variance/sd  this call just produces NA cor(Bn[ipred], BnBaseline[ipred], method = "pearson", use = "complete")
 	MSE[1] = mse(vrBaseline[ipred], vr[ipred], na.rm = T)
 }
  
  skill = (1. - MSE/MSE[1]) ##*100.

  lines(ddate[isub],vr[isub],type="l",lwd=1)   #plot again to overlay other plots
  lines(c(date1,date1),c(vmin,vmax),type="l",col="red",lwd=3)
  lines(ddate[ipred],vrBaseline[ipred],type="l",col=cmodels[1])

  dvDrop = 2.6

  legend(xmin,vmax,lty=replicate(length(models[imodels])+1,1),c(sc,smodels[imodels]),
  lwd=replicate(length(models[imodels])+1,2.5),text.col=c("black",cmodels[imodels]),
  col=c("black",cmodels[imodels]),cex=cex1)
  legend("topright",legend=date1,bty='n')
  legend(xmin,vmax/dvDrop,paste(smodels[imodels],":",format(corrVec[imodels],digits=2),sep=""),
  text.col=cmodels[imodels],cex=cex1,title.col="black",title="Corr. Coeff.")
  legend(xmin+(xmax-xmin)/4,vmax/dvDrop,paste(smodels[imodels],": ",format(MSE[imodels],digits=3),sep=""),text.col=cmodels[imodels],cex=cex1,title.col="black",title="MSE (km/sec^2)")
  legend(xmin+(xmax-xmin)/1.95,vmax/dvDrop,paste(smodels[imodels],": ",format(skill[imodels],digits=2),sep=""),text.col=cmodels[imodels],cex=cex1,title.col="black",title="Skill Score (%)")

  return(err)
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

readMagV_OMNI <- function() {
  
  dir = "../data"
  
  err = 0 # set the error flag to zero
  
  # if patt-rec is not set then don't need to read all of the 
  # data...can make it a bit faster. 
  # For now, though, just read the entire dataset
  
  magFile = "omni_m_all_years.dat"
  
  file = paste0(dir,'/',magFile)
  
  arr_omni  <- read.csv(file,header=F,sep="",na.strings = c("-999.9"))
  
  yyear= arr_omni$V1
  dday = arr_omni$V2
  hhour= arr_omni$V3
  Br   = arr_omni$V6
  Bt   = arr_omni$V7
  Bn   = arr_omni$V8
  B    = arr_omni$V9
  vr   = arr_omni$V10
  np   = arr_omni$V13
  Temp = arr_omni$V14
  
  # outlierReplace(arr_mag_today, "arr_mag_today$V10", which(arr_mag_today$V10 > -20), NA)
  
  # convert the year, day, and hour to a POSIX date using the 
  # cron utility
  sdate = paste(yyear,' ',dday,' ',hhour,sep='')
  ddate = strptime(sdate,format="%Y %j %H",tz="UTC")
  err = 0
  result <- list(ddate=ddate,year=yyear,doy=dday,hour=hhour,Br=Br,Bt=Bt,Bn=Bn,B=B,vr=vr,np=np,Temp=Temp,err=err)
  
  return(result)
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



