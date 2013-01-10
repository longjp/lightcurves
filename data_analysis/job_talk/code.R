########## 
########## 
########## ANALYZING DATA FOR JOB TALK 
##########
########## by James Long 
########## date: January 9, 2013 
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
features = '../../data_processed/job_talk.dat'
tfe = '../../data_processed/job_talk_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)







completes <- data1$features.n_points > 50
data1$number.points <- paste(as.character(data1$features.n_points)," points",sep="")
data1$number.points[completes] = "full time series"


pdf("convergence_frequency.pdf")
DrawKDES(log(data1$features.freq1_harmonics_freq_0),
         as.factor(data1$number.points),
         main.title="",
         xlab="log(frequency)",
         ylab="Density",
         density.colors=NULL,
         location='topright',
         trim=.01,
         xlimits=NULL,
         ylimits=NULL,
         line.types=NULL,
         line.width=2,
         cex.lab=1,
         legend.title=NULL,
         legend.print=TRUE)
dev.off()




pdf("convergence_amplitude.pdf")
DrawKDES(data1$features.amplitude,
         as.factor(data1$number.points),
         main.title="",
         xlab="log(frequency)",
         ylab="Density",
         density.colors=NULL,
         location='topright',
         trim=.01,
         xlimits=NULL,
         ylimits=NULL,
         line.types=NULL,
         line.width=2,
         cex.lab=1,
         legend.title=NULL,
         legend.print=TRUE)
dev.off()
