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



## scatterplot with true and estimated features
pdf("true_estimated_features.pdf")
use = (data1$sources.survey != "full")
plot(log(1/data1$features.freq1_harmonics_freq_0[use]),
     log(data1$features.amplitude[use]),
     col="orange",
     pch=1,
     xlab="log(period)",
     ylab="log(amplitude)",
     cex.lab=2,
     cex=1.5,
     lwd=2)

use = (data1$sources.survey == "full")
points(log(1/data1$features.freq1_harmonics_freq_0[use]),
       log(data1$features.amplitude[use]),
       pch=3,
       bg="black",
       cex=1.5,
       cex.lab=2,
       lwd=2)
legend("bottomright",c("True Features","Estimates"),
       pch=c(3,1),col=c("black","orange"),cex=1.5)
dev.off()




## plot original l.c.
reduced <- data1$sources.survey=="reduced"
id <- data1$features.source_id[!reduced]
id

pdf("full_lc.pdf",width=8,height=4)
DrawThreeLightCurves(id,data1,time_flux,
                     plot.unfolded=FALSE,
                     plot.folded.twice=FALSE,
                     point.colors=0)
dev.off()





## plot cadence
head(time_flux)

id <- min(data1$features.source_id[data1$sources.survey=="reduced"])
id

cad <- subset(time_flux,subset=(time_flux$source_id==id),
            select=c("time","error"))
nrow(a)
head(a)



pdf("cadence.pdf",width=8,height=4)
plot(c(min(cad[,1]),max(cad[,1])),
     c(-max(cad[,2]/2),max(cad[,2]/2)),pch=20,col=0,
     ylab="SD Error",xlab="Time (Days)",
     main="Cadence",cex.lab=2)
lw <- .1
segments(cad[,1],-cad[,2]/2,cad[,1],cad[,2]/2,lwd=2)
segments(cad[,1]-lw,-cad[,2]/2,cad[,1]+lw,-cad[,2]/2,lwd=2)
segments(cad[,1]-lw,cad[,2]/2,cad[,1]+lw,cad[,2]/2,lwd=2)
abline(h=0)
dev.off()
