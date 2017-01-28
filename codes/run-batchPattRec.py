#!/usr/bin/python

##
## A python driver script launching 12 processes, one per month, for a given year (e.g. 2000 below)
## a given variable (e.g. Bn below) and a given forecasting window (e.g. 24 hours below)
## This can be generalized to run on multiple years and for multiple parameters, forecasting windows
## This script calls the  batchPattRec.R routine which in turn calls pattRecBatch that is in the pattRec.R file
## Python -> batchPattRec.R -> pattRecBatch (in pattRec.R)
## Output is in the form of csv files.  Of file per variable/month/year
## Output will be written into a sub-directory (under current working directory) which is unique for each variable/
## forecasting combination, e.g. for the setting below: "corr-coh-Bn-24hr"
## whvar - is the variable name; Bn, vr, np or Temp
## wwindow - is the forecasting time window in units of hours.  For the manuscript we used 24 and 6 hours
## month and year - determine for which month/year the pattern recognition procedure will be applied.
## batchPattRec.R will apply the procedure for each hour in each day of this month/year.
## Note that you can run batchPattRec.R directly from the command line for a single month/year/variable/forecasting window
## See the batchPattRec.R routine for more details
## To run this script use: ./run-batchPattRec.py > log& 
##

import multiprocessing
from multiprocessing import Process
import os
import numpy
import string
import random

def info(title):
    print title
    print 'module name:',__name__
    if hasattr(os,'getppid'): 
         print 'parent process:',os.getpid()
    print 'process id: ',os.getpid()

def f(year,month):
    
    os.system("Rscript batchPattRec.R whvar Bn wwindow 24 month "+str(month)+" year "+str(year))


year = 2010
month=range(1,13)
nthreads = len(month)
year = numpy.repeat(year,nthreads)

if __name__ == '__main__':
    info('main line')
    p1 = Process(target=f,args=(year[0],month[0],))
    p1.start()
    p2 = Process(target=f,args=(year[1],month[1],))
    p2.start()
    p3 = Process(target=f,args=(year[2],month[2],))
    p3.start()
    p4 = Process(target=f,args=(year[3],month[3],))
    p4.start()
    p5 = Process(target=f,args=(year[4],month[4],))
    p5.start()
    p6 = Process(target=f,args=(year[5],month[5],))
    p6.start()
    p7 = Process(target=f,args=(year[6],month[6],))
    p7.start()
    p8 = Process(target=f,args=(year[7],month[7],))
    p8.start()
    p9 = Process(target=f,args=(year[8],month[8],))
    p9.start()
    p10 = Process(target=f,args=(year[9],month[9],))
    p10.start()
    p11 = Process(target=f,args=(year[10],month[10],))
    p11.start()
    p12 = Process(target=f,args=(year[11],month[11],))
    p12.start()
    

