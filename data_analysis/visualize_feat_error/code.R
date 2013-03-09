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


## make separate scatterplot with diff number flux
## scatterplot with true and estimated features
##


use <- data1$sources.survey != "10"
min.p <- min(log(1/data1$features.freq1_harmonics_freq_0[use],
                 base=10))
max.p <- max(log(1/data1$features.freq1_harmonics_freq_0[use],
                 base=10))
min.a <- min(log(data1$features.freq1_harmonics_amplitude_0[use],base=10))
max.a <- max(log(data1$features.freq1_harmonics_amplitude_0[use],base=10))

jj <- 10*(2:8)
for(ii in jj){
  pdf(paste("estimated_features_",ii,".pdf",sep=""))
  use = (data1$sources.survey == as.character(ii))
  par(mar=c(4.5,4.5,.5,.5))
  plot(log(1/data1$features.freq1_harmonics_freq_0[use],
           base=10),
       log(data1$features.freq1_harmonics_amplitude_0[use],
           base=10),
       col="black",
       pch=1,
       xlab="log(period)",
       ylab="log(amplitude)",
       cex.lab=1.5,
       cex.axis=1.3,
       cex=1.3,
       lwd=1.5,
       ylim=c(min.a,max.a),
       xlim=c(min.p,max.p))
  use = (data1$sources.survey == "full")
  points(log(1/data1$features.freq1_harmonics_freq_0[use],
             base=10),
         log(data1$features.freq1_harmonics_amplitude_0[use],
             base=10),
         pch=3,
         col="orange",
         cex=2,
         cex.lab=2,
         lwd=4)
  legend("bottomright",
         c("True Features",
           paste("Estimated with ",as.character(ii),
                 " Measurements",sep="")),
         pch=c(3,1),
         col=c("orange","black"),
         cex=1.5,
         pt.cex=1.5,
         pt.lwd=1.5)
  dev.off()
}




## plot original l.c.
full <- data1$sources.survey=="full"
id <- data1$features.source_id[full]
id

tfe = subset(time_flux,
  subset=(source_id==id),select=c("time","flux","error"))
tfe[,1] = tfe[,1] - min(tfe[,1])
period <- (1 / data1$features.freq1_harmonics_freq_0[id == data1$features.source_id])
tfe[,1] = (tfe[,1] %% period) / period


source('~/Rmodules/Rfunctions.R')

pdf("full_lc.pdf",width=8,height=4)
plotLightCurve(tfe,
               xLabel=paste("Phase  (period=",
                 round(period,3)," days)",sep=""),
               maintitle="",
               point.colors=0,
               yLabel="mags",
               cex.lab=1.5,
               cex.axis=1.5)
line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
line.smu$y = -1 * line.smu$y
lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
line.smu = supsmu(tfe[,1],tfe[,2],
  span=.05,wt=1/tfe[,3],periodic=TRUE,bass=8)
line.smu$y = -1 * line.smu$y
dev.off()





## plot cadence
## head(time_flux)

## id <- min(data1$features.source_id[data1$sources.survey=="reduced"])
## id

## cad <- subset(time_flux,subset=(time_flux$source_id==id),
##             select=c("time","error"))
## nrow(a)
## head(a)



## pdf("cadence.pdf",width=8,height=4)
## par(mar=c(4.5,4.5,.5,.5))
## plot(c(min(cad[,1]),max(cad[,1])),
##      c(-max(cad[,2]/2),max(cad[,2]/2)),pch=20,col=0,
##      ylab="SD Error",xlab="Time (Days)",cex.lab=2,
##      cex.axis=1.5)
## lw <- .1
## segments(cad[,1],-cad[,2]/2,cad[,1],cad[,2]/2,lwd=2)
## segments(cad[,1]-lw,-cad[,2]/2,cad[,1]+lw,-cad[,2]/2,lwd=2)
## segments(cad[,1]-lw,cad[,2]/2,cad[,1]+lw,cad[,2]/2,lwd=2)
## abline(h=0)
## dev.off()
