## analyzing the ogle eclipsing RR lyrae source

##
## TODO:
## 1. right now its hard to identify eclipsing RR from
## its light curve. write R code to extract the largest period
## and plot this function along with the residual function
## for our curve should look clean
##
##


## program setup
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)


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
features = '../../data_processed/eclipse-rr.dat'
tfe = '../../data_processed/eclipse-rr-tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


nrow(data1)
data1$eclipsing <- 1*grepl("02792",data1$sources.xml_filename)


plot(log(1/data1$features.freq1_harmonics_freq_0,base=10),
     log(data1$features.amplitude,base=10),
     col=(data1$eclipsing+1),
     lwd=2)


plot(log(1/data1$features.freq1_harmonics_freq_0,base=10),
     data1$features.p2p_scatter_2praw,
     col=(data1$eclipsing+1),
     lwd=2)


pdf("amplitude_p2p.pdf")
plot(log(data1$features.amplitude,base=10),
     data1$features.p2p_scatter_2praw,
     col=(data1$eclipsing+1),
     lwd=2)
dev.off()

pdf("signifratio_p2p.pdf")
plot(data1$features.freq_signif_ratio_21,
     data1$features.p2p_scatter_2praw,
     col=(data1$eclipsing+1),
     lwd=2)
dev.off()

pdf("signifratio_signif.pdf")
plot(data1$features.freq_signif_ratio_21,
     data1$features.freq_signif,
     col=(data1$eclipsing+1),
     lwd=2)
dev.off()



## observation is quite high on scatter metric
## ::notes::
## 1. eclipsing rr has high freq_signif_ratio_21
## makes sense because there are two periods
##
## 2. also high on p2p_scatter_2praw. probably because
## folded curve has many outlying points due to there
## being a second period
##   



## order observations by p2p, then plot
ords <- order(data1$features.p2p_scatter_2praw,decreasing=TRUE)
i <- 1

i <- i + 1
pdf(paste("rr",i,".pdf",sep=""))
DrawThreeLightCurves(data1$features.source_id[ords[i]],
                     data1,
                     time_flux)
dev.off()



ords <- order(data1$features.p2p_scatter_2praw,decreasing=TRUE)
i <- 1


i <- i + 1
DrawEclipsingRR(data1$features.source_id[ords[i]],
                data1,
                time_flux)




## take a light curve, remove first period, and then plot residuals


2*(1 / data1$features.freq2_harmonics_freq_0[data1$eclipsing==1])

## use estimated period



source('~/Rmodules/Rfunctions.R')
sid <- data1$features.source_id[data1$eclipsing==1]
DrawEclipsingRR(sid,data1,time_flux)

period1 <- 1/data1$features.freq1_harmonics_freq_0[data1$eclipsing==1]

period1
tfe <- subset(time_flux,
              subset=(time_flux$source_id==sid),
              select=c("time","flux","error"))
times_orig <- tfe[,1]



plot(tfe[,1],tfe[,2])
tfe[,1] <- tfe[,1] - min(tfe[,1])
tfe[,1] <- (tfe[,1] %% period1) / period1



line.smu <- supsmu(tfe[,1],tfe[,2],periodic=TRUE)
plot(tfe[,1],tfe[,2])
points(line.smu$x,line.smu$y,col='red')


dev.new()
plot(tfe[,1],tfe[,2] - line.smu$y[rank(tfe[,1])])

tfe[,2] <- (tfe[,2] - line.smu$y[rank(tfe[,1])])

plot(tfe[,1],tfe[,2])
tfe[,1] <- times_orig
plot(tfe[,1],tfe[,2])

period2 <- 2*(1/data1$features.freq2_harmonics_freq_0[data1$eclipsing==1])

tfe[,1] = (tfe[,1] %% period2) / period2
plot(tfe[,1],tfe[,2])




##
source_ids <- data1$features.source_id

i <- 1

i <- i + 1
DrawThreeLightCurves(source_ids[i],
                     data1,
                     time_flux)




pdf("eclipsing.pdf")
DrawThreeLightCurves(source_id,data1,time_flux)
dev.off()

## use eclipse period
eclipse.period = 15.2435
dev.new()
DrawThreeLightCurves(1,
                     data1,
                     time_flux,
                     use.estimated.period=FALSE,
                     period=eclipse.period)
