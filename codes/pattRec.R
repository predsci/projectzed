pattRecBatch <- function(yearOne = NULL, monthOne = NULL, dayOne = NULL, hourOne = NULL, whvar = NULL, wwindow = NULL) {

	## Check to see if there is a directory to put the results in or if it needs to be created
	
	resultsDir = paste0("corr-coh-", whvar, "-", wwindow, "hr")

	if (!dir.exists(resultsDir)) {
		dir.create(resultsDir)
		cat("\n\n Created Directory for Output Plots: ", resultsDir, "\n\n")
	}

	# read in OMNI_M data and attempt to do historical pattern matching and use to make 
	# future prediction

	readDataFlag = T
	write_on = "yes" # yes or no

	if (missing(whvar)) {
		whvar = "Bn"
	}

	if (missing(dayOne)) {
		write_on = "yes" # yes or no
	}

	mmin = 0
	ssec = 0

	if (missing(wwindow)) {
		wwindow = 24
	}

	dn = wwindow/2

	## Number of days in each of the 12 months
	dayMon = c("31", "28", "31", "30", "31", "30", "31", "31", "30", "31", "30", "31")

	if (missing(dayOne)) {
		dayMonMax = dayMon[month]
		dateStart = as.Date(paste(yearOne, "-", monthOne, "-", "01", sep = ""))
		dateEnd = as.Date(paste(yearOne, "-", monthOne, "-", dayMonMax, sep = ""))
	} else {
		dateStart = as.Date(paste(yearOne, "-", monthOne, "-", dayOne, sep = ""))
		dateEnd = dateStart
	}
	print(paste("Date Start:", dateStart, sep = ""))
	print(paste("Date End:", dateEnd, sep = ""))

	deltaDate = as.numeric(as.Date(dateEnd) - as.Date(dateStart)) + 1

	if (missing(hourOne)) {
		hour1 = 1
		hour2 = 24
	} else {
		hour1 = hourOne
		hour2 = hourOne
	}

	dateVect = replicate(24 * deltaDate, -999)
	hourVect = replicate(24 * deltaDate, -999)
	corrPred = replicate(24 * deltaDate, -999)
	corrPrior = replicate(24 * deltaDate, -999)
	coherency = replicate(24 * deltaDate, -999)
	mse = replicate(24 * deltaDate, -999)
	mseZero = replicate(24 * deltaDate, -999)

	icount = 1

	#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
	# Read in the data and clean it - removing missing values

	if (readDataFlag == T) {
		readData = readMagV_OMNI() # read in the data using the routine in util.R
		readDataFlag == F
		ddate = readData$ddate
		year = readData$year
		doy = readData$doy
		hour = readData$hour
		Br = readData$Br
		Bt = readData$Bt
		Bn = readData$Bn
		B = readData$B
		vr = readData$vr
		Temp = readData$Temp
		np = readData$np
		time = year + (doy + hour/24)/365.25

		if (whvar == "Bn") {
			good = Bn < 500
			zeroOffset = mean(Bn[good])
		}

		if (whvar == "Br") {
			good = Br < 500
			zeroOffset = mean(Br[good])
		}

		if (whvar == "Bt") {
			good = Bt < 500
			zeroOffset = mean(Bt[good])
		}


		if (whvar == "vr") {
			good = vr < 5000
			zeroOffset = mean(vr[good])
		}

		if (whvar == "np") {
			good = np < 999.9
			zeroOffset = mean(np[good])
		}

		if (whvar == "Temp") {
			good = Temp < 9999999
			zeroOffset = mean(Temp[good])
		}

		ddate = ddate[good]
		Br = Br[good]
		Bt = Bt[good]
		Bn = Bn[good]
		B = B[good]
		vr = vr[good]
		Temp = Temp[good]
		np = np[good]
		year = year[good]
		doy = doy[good]
		hour = hour[good]
		time = time[good]

		ntot = length(Br)

		readDataFlag = F
	}

	#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
	

	for (idate in 1:(deltaDate)) {
		for (itime in hour1:hour2) {

			perCent = icount/(length(corrPred) + 1)
			if (perCent * 100%%5 == 0) {
				print(paste("Percent Complete:", format(perCent * 100, digits = 3), sep = ""))
			}

			ymd = dateStart + (idate - 1)
			hhour = itime

			# convert the date and time to POSIX value XXX
			date1 = ISOdate(ymd, hhour, mmin, ssec, tz = "UTC")

			#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
			
			n = min((1:ntot)[ddate == date1])

			if (!is.finite(n)) 
				next
			var = Bn
			units = "(nT)"

			if (whvar == "Br") {
				var = Br
				units = "(nT)"
			}

			if (whvar == "Bt") {
				var = Bt
				units = "(nT)"
			}

			if (whvar == "vr") {
				var = vr
				units = "(km/s)"
			}

			if (whvar == "Temp") {
				var = Temp
				units = "(K)"
			}

			if (whvar == "np") {
				var = np
				units = "(N/cm^3)"
			}

			eucDist = var * 0 + NA
			x1 = var[(n - 2 * dn):n]
			tsub = time[(n - 2 * dn):n]

			for (i in (1 + dn):(n - dn)) {
				x2 = var[(i - dn):(i + dn)]
				tmp = (x1 - x2) * (x1 - x2)
				eucDist[i] = sqrt(sum(tmp))
			}

			eucDistRaw = eucDist
			eucDist[eucDist == 0] = 999
			# find top npatt locations
			npatt = 50
			orderEucDist = order(eucDist, method = "radix")


			meanvarPred = rep(0, (1 + 2 * dn))
			meanvarPrior = rep(0, (1 + 2 * dn))

			for (j in 1:npatt) {
				iEuc = orderEucDist[j]
				meanvarPred = var[(iEuc + dn):(iEuc + 3 * dn)] + meanvarPred
				meanvarPrior = var[(iEuc - dn):(iEuc + dn)] + meanvarPrior
			}

			meanvarPred = meanvarPred/npatt
			meanvarPrior = meanvarPrior/npatt
			varFuture = var[n:(n + 2 * dn)]

			corrPred[icount] = cor(meanvarPred, varFuture)
			corrPrior[icount] = cor(meanvarPrior, var[(n - 2 * dn):n])
			coherency[icount] = mean(abs(var[(n - 2 * dn):n]))
			mse[icount] = mse(meanvarPred, varFuture)
			mseZero[icount] = mse(0 * meanvarPred + zeroOffset, varFuture)
			dateVect[icount] = date1
			hourVect[icount] = itime


			icount = icount + 1

		} # closes the first time loop
	} # closes the second time loop



	################################################################
	
	# write out the results to a csv file. Each year/month has a file
	
	if (write_on == "yes") {
		fileOut = paste0(resultsDir, "/corr-coh-", whvar, "-w", wwindow, "-", yearOne, "-", monthOne, ".csv")
		df <- data.frame(dateVect, hourVect, corrPrior, corrPred, coherency, mse, mseZero)
		names(df) <- c("Date", "hour", "corrPrior", "corrPred", "coh", "mse", "mseZero")
		write.csv(df, fileOut)
	}
	################################################################
	
} # closes the function definition statement

################################################################
