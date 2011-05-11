####
#### by James Long
#### date April 22, 2011
####
#### used for studying synthetic data
####


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)

source('../noisification_code/Rfunctions.R')
library('randomForest')
library('rpart')
library('xtable')

# set the output graphics folder
graphics = fileOutLoc('figures/')
tables = fileOutLoc('tables/')

# get the data
features = '../../data_processed/synthetic_analysis/sources00001.dat'
tfe = '../../data_processed/synthetic_analysis/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)

# run the code that is used for all noisification analysis
source('../noisification_code/noisification_analysis.R')

# run the code that is used for robust analysis
source('../robust_code/robust.R')


