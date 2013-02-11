########## 
########## 
########## FOR VISUALIZING FEATURE ERROR 
########## SEE: src/interp_visualize_feat_error.py
##########      for how to generate data analyzed by
##########      this script
##########
########## by James Long 
########## date: February 11, 2013 
########## 



rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=60)


source('~/Rmodules/Rfunctions.R')
source('../denoisification_code/denoisification.R')
source('../denoisification_code/rf_denoise.R')
library('randomForest')
library('rpart')
library('xtable')
library('np')
library('ks')
require('scatterplot3d')
require('fields')



## get the data
features = '../../data_processed/visualize_feat_error.dat'
tfe = '../../data_processed/visualize_feat_error_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)





nrow(data1)
names(data1)
unique(data1$features.n_points)
data1$sources.survey

plot(log(1/data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude),
     col=as.numeric(data1$sources.survey) + 2,
     pch=20,
     xlab="log(period)",
     ylab="log(amplitude)",
     cex.lab=2,
     cex=1.5)
