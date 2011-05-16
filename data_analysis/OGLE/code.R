########## 
########## 
########## ANALYZE DATA IN data_processed/OGLE 
##########
########## by James Long 
########## date: 4/17/2011 
########## 


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)

source('../noisification_code/Rfunctions.R')
source('../denoisification_code/denoisification.R')
source('../denoisification_code/rf_denoise.R')
library('randomForest')
library('rpart')
library('xtable')

# set the output graphics folder
graphics = fileOutLoc('figures/')
tables = fileOutLoc('tables/')
RData = fileOutLoc('RData/')


# get the data
features = '../../data_processed/OGLE/sources00001.dat'
tfe = '../../data_processed/OGLE/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)

# necessary for real data b/c there is no definition of true period
# we DEFINE true_period to be period of clean sources
true_period_df = subset(data1,
  subset=features.source_id==sources.original_source_id,
  select=c("sources.original_source_id",
    "features.freq1_harmonics_freq_0"))
names(true_period_df) = c(names(true_period_df)[1],
       "sources.true_period")
true_period_df[,2] = 1 / true_period_df[,2]
data1$sources.true_period = NULL
data2 = merge(data1,true_period_df)
data1 = data2

## run noisification code
source('../noisification_code/noisification_analysis.R')

## run robust code
source('../robust_code/robust.R')

## run denoisification code
source('../denoisification_code/denoise_code.R')



## mira and beta lyrae
## all clean
## all 20 points

## bl.uma.clean = subset(data1train,(sources.classification %in% c("Beta Lyrae","Multiple Mode Cepheid")) & (sources.original_source_id == features.source_id))
## bl.uma.clean$sources.classification = as.character(bl.uma.clean$sources.classification)
## rp.clean = rpart(rf_formula,data=bl.uma.clean)
## rp.clean


## bl.uma.poor = subset(data1train,(sources.classification %in% c("Beta Lyrae","Multiple Mode Cepheid")) & (row_id == 0) & (features.n_points == 10) & !(contains.random))
## bl.uma.poor$sources.classification = as.character(bl.uma.poor$sources.classification)
## nrow(bl.uma.poor)
## rp.poor = rpart(rf_formula,data=bl.uma.poor)
## rp.poor
