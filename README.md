# projectzed
A gitHub Repository including all the data and results needed to make figures 1-10 of the ‘Forecasting the Properties of the Solar Wind using Simple Pattern Recognition’ manuscript by Riley et al.
———————————
Requirements
———————————
The R software package (version 3.2 or higher)
The R packages: date, hex bin, hydroGOF, Hmisc, RColorBrewer, zoo (and their dependencies).

———————————
Cloning
———————————
To clone from the common line use:
%git clone 

———————————
Running 
———————————

Change directory to the R codes directory

% cd projectzed/codes

forecast.R will make figures 1, 2a, 2b, and 8a-d 

anal-corr-coh.R will make all the other figures: 3-7, 9 and 10.

In both cases the figures will be created in the ‘plots’ sub-directory.

In both cases you can run the codes either from the command line, e.g.

% Rscript forecast.R

or from within an R session:

> source(‘forecast.R’)

The util.R script includes functions used by forecast.R

——————————————————————
Questions/Problems:
——————————————————————
Please email mbennun@predsci.com
