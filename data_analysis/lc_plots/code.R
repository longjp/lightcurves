## code for producing plots of OGLE and Hipparcos LCs
##
## by James Long
## date Feb 5, 2013

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
features = '../../data_processed/hip_ogle_plot.dat'
tfe = '../../data_processed/hip_ogle_plot_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)





data1[1,]
data1$sources.xml_filename



ii <- 1

ii <- ii + 1


## get hipparcos source ids and plot them
ids <- sample((1:nrow(data1))[data1$sources.survey=="hipparcos"],
              40)
## get fixed ids
ids <- (1:nrow(data1))[data1$sources.survey=="hipparcos"][1:100]

for(ii in ids){
  filename <- data1[ii,"sources.xml_filename"]
  filename <- gsub(".xml","",filename)
  lc.class <- data1[ii,"sources.classification"]
  to_get <- time_flux$source_id==data1[ii,"features.source_id"]
  tfe <- time_flux[to_get,c(2,3,4)]
  tfe[,1] <- tfe[,1] - min(tfe[,1])
  pdf(paste("plots/",filename,".pdf",sep=""),height=4,width=8)
  par(mar=c(4.1,4.1,1.1,.4))
  plotLightCurve(tfe,maintitle=lc.class,
                 yLabel="mags")
  dev.off()
}



ids <- (1:nrow(data1))[data1$sources.survey=="ogle"][1000:1200]

for(ii in ids){
  filename <- data1[ii,"sources.xml_filename"]
  filename <- gsub(".dat","",filename)
  filename <- gsub(pattern="/","",filename)
  filename <- gsub(pattern="\\.\\.","",filename)
  lc.class <- data1[ii,"sources.classification"]
  to_get <- time_flux$source_id==data1[ii,"features.source_id"]
  tfe <- time_flux[to_get,c(2,3,4)]
  tfe[,1] <- tfe[,1] - min(tfe[,1])
  pdf(paste("plots/",filename,".pdf",sep=""),height=4,width=8)
  par(mar=c(4.1,4.1,1.1,.4))
  plotLightCurve(tfe,maintitle=lc.class,
                 yLabel="mags")
  dev.off()
}


filename
