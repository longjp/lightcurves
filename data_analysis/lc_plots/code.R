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




## local version of DrawThreeLightCurves with
## parameters adjusted for presentation
DrawLCs <- function(source_to_plot,
                    data1,
                    time_flux,
                    smoother=TRUE,
                    point.colors=FALSE,
                    plot.unfolded=TRUE,
                    plot.folded=TRUE,
                    plot.folded.twice=TRUE,
                    par=TRUE,
                    plot.errors=TRUE,
                    use.estimated.period=TRUE,
                    period=1,
                    green=TRUE,
                    title1=NULL,
                    title2=NULL,
                    title3=NULL){
  ## if source id is a list, grab a random l.c. to plot
  print(source_to_plot)
  number.plots = plot.unfolded + plot.folded + plot.folded.twice
  if(par){
    par(mfcol=c(number.plots,1))
  }
  par(mar=c(6.5,4.5,3,1.1))
  tfe <- subset(time_flux,
                subset=(source_id==source_to_plot),
                select=c("time","flux","error"))
  tfe[,1] <- tfe[,1] - min(tfe[,1])
  if (use.estimated.period){
    period <- 2*(1 /
                 data1$features.freq1_harmonics_freq_0[source_to_plot == data1$features.source_id & source_to_plot == data1$sources.original_source_id])
  }
  lc.class <- data1$sources.classification[source_to_plot == data1$features.source_id & source_to_plot == data1$sources.original_source_id]

  ## if titles are NULL, fill them in
  if(is.null(title1)){
    title1 <- "Original Training Time Series"
  }
  if(is.null(title2)){
    title2 <- "Phased Training Time Series"
  }
  if(is.null(title3)){
    title3 <-paste("period=",round(period/2,3),sep="")
  }
  


  ## plot the raw light curve
  if(plot.unfolded){
    plotLightCurve(tfe,
                   maintitle=title1,
                   point.colors=point.colors,
                   plot.errors=plot.errors,
                   cex.main=2,
                   cex.lab=1.3)
  }
  ## shrink margin
  par(mar=c(5.1,4.5,4.0,1.1))
  ## fold on twice estimated period
  tfe[,1] = (tfe[,1] %% period) / period
  if(plot.folded.twice){
    plotLightCurve(tfe,
                   xLabel=paste("Phase  (period=",round(period,3),
                     " days)",sep=""),
                   maintitle=title2,
                   point.colors=point.colors,
                   plot.errors=plot.errors,
                   cex.lab=1.3,
                   cex.main=2)
    if(smoother){
      line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
      line.smu$y = -1 * line.smu$y
      lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
      line.smu = supsmu(tfe[,1],tfe[,2],
        span=.05,wt=1/tfe[,3],periodic=TRUE,bass=8)
      line.smu$y = -1 * line.smu$y
      if(green){
        lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)
      }
    }
  }  
  ## fold on estimated period
  tfe[,1] = (tfe[,1] %% .5) / .5
  if(plot.folded){
    plotLightCurve(tfe,xLabel="Phase",
                   maintitle=title3,point.colors=point.colors,
                   plot.errors=plot.errors)  
    if(smoother){
      line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
      line.smu$y = -1 * line.smu$y
      lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
      line.smu = supsmu(tfe[,1],tfe[,2])
      line.smu$y = -1 * line.smu$y
      if(green){
        lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)
      }
    }
  }
}




## get the data
features = '../../data_processed/hip_ogle_plot.dat'
tfe = '../../data_processed/hip_ogle_plot_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)








## classes for each hipparcos source
## current thay are all erroneously labeled as
## "beta lyrae"

## read in dubath features and construct a dataframe
## with 1 column for hipparcos ID and 1 column for class
filename <- "dubath_features"
dubath.features <- read.table(filename,sep="&",header=TRUE)
table(dubath.features$class)
dubath.names <- levels(dubath.features$class)
dubath.classes <- as.character(dubath.features$class)
dubath.names
our.names <- c("Alpha-2 Canum Venaticorum","Alpha Cygni","Beta Cephei","BE+GCAS","Multiple Mode Cepheid","W Virginus A","W Virginus B","Delta Cepheid","Delta Cepheid First Overtone","Delta Scuti","Delta Scuti Low Amplitude","Algol (Beta Persei)","Beta Lyrae","Ellipsoidal","W Ursae Majoris","Gamma Doradus","Long Period Variable","RR Lyrae AB","RR Lyrae C","RS+BY","RV Tauri","Slowly Pulsating B Star","SX Areitas")
name.associations <- cbind(dubath.names,our.names)
name.associations
for(i in 1:nrow(name.associations)){
  dubath.classes[dubath.classes == name.associations[i,1]] <-
    name.associations[i,2]
}
dubath.features$class <- as.factor(dubath.classes)
names(dubath.features)
dubath.features.classid <- dubath.features[,c(1,2)]
names(dubath.features.classid) <- c("sources.xml_filename","sources.classification")
dubath.features.classid[,1] <- paste("HIP",
                                     dubath.features.classid[,1],
                                     sep="")


## merge dubath.features.classid with hipparcos
## sources in data1.

data2 <- subset(data1,data1$sources.survey=="hipparcos")
data1 <- subset(data1,data1$sources.survey=="ogle")
data2$sources.classification <- NULL
data2$sources.xml_filename <- gsub(".xml","",
                                   data2$sources.xml_filename)
data2 <- merge(data2,dubath.features.classid)
nrow(data2)
head(data2)
data1 <- rbind(data1,data2)









######### FOR PRODUCING GRAPHICS FOR JOB TALK
## get hipparcos source ids and plot them
ids <- (1:nrow(data1))[data1$sources.survey=="hipparcos"][1:200]
source('~/Rmodules/Rfunctions.R')
for(ii in ids){
  filename <- data1[ii,"sources.xml_filename"]
  filename <- gsub(".xml","",filename)
  lc.class <- data1[ii,"sources.classification"]
  to_get <- time_flux$source_id==data1[ii,"features.source_id"]
  tfe <- time_flux[to_get,c(2,3,4)]
  tfe[,1] <- tfe[,1] - min(tfe[,1])
  pdf(paste("job_talk/figures/hip/",
            filename,".pdf",sep=""),
      height=4,width=8)
  par(mar=c(4.1,4.4,1.2,.4))
  plotLightCurve(tfe,maintitle=lc.class,
                 yLabel="mags",
                 cex.lab=2,
                 cex.main=2,
                 cex=.7)
  dev.off()
}





## plot without name
ids <- (1:nrow(data1))[data1$sources.survey=="hipparcos"][1:200]
source('~/Rmodules/Rfunctions.R')
for(ii in ids){
  filename <- data1[ii,"sources.xml_filename"]
  filename <- gsub(".xml","",filename)
  lc.class <- data1[ii,"sources.classification"]
  to_get <- time_flux$source_id==data1[ii,"features.source_id"]
  tfe <- time_flux[to_get,c(2,3,4)]
  tfe[,1] <- tfe[,1] - min(tfe[,1])
  pdf(paste("job_talk/figures/hip/",
            filename,"notitle.pdf",sep=""),
      height=4,width=8)
  par(mar=c(4.1,4.4,1.2,.4))
  plotLightCurve(tfe,maintitle="",
                 yLabel="mags",
                 cex.lab=2,
                 cex.main=2,
                 cex=.7)
  dev.off()

  period <- 2/data1[ii,"features.freq1_harmonics_freq_0"]
  tfe[,1] <- (tfe[,1] %% period) / period
  pdf(paste("job_talk/figures/hip/",
            filename,"folded.pdf",sep=""),
      height=4,width=8)
  par(mar=c(4.1,4.4,1.2,.4))
  plotLightCurve(tfe,maintitle="",
                 yLabel="mags",
                 xLabel=paste("Phase (period = ",
                   round(period,3)," days)",sep=""),
                 cex.lab=2,
                 cex.main=2,
                 cex=.7)
  dev.off()  
}




## get OGLE ids and plot them
is.ogle <- data1$sources.survey=="ogle"
is.rrab <- data1$sources.classification=="RR Lyrae AB"
ids <- (1:nrow(data1))[is.ogle][300:500]
ids2 <- (1:nrow(data1))[(is.ogle & is.rrab)][1:20]
ids <- c(ids,ids2)

for(ii in ids){
  filename <- data1[ii,"sources.xml_filename"]
  filename <- strsplit(as.character(filename),"/")[[1]]
  filename <- gsub(".dat","",filename[length(filename)])
  lc.class <- data1[ii,"sources.classification"]
  to_get <- time_flux$source_id==data1[ii,"features.source_id"]
  tfe <- time_flux[to_get,c(2,3,4)]
  tfe[,1] <- tfe[,1] - min(tfe[,1])
  pdf(paste("job_talk/figures/ogle/",
            filename,".pdf",sep=""),
      height=4,width=8)
  par(mar=c(4.1,4.4,1.2,.4))
  plotLightCurve(tfe,maintitle="?",
                 yLabel="mags",
                 cex.lab=2,
                 cex.main=2,
                 cex=.7)
  dev.off()
}



## plot hip sources unfolded and folded for bstars talk
nrow(data1)
ids <- data1$features.source_id[data1$sources.survey=="hipparcos"]
source('~/Rmodules/Rfunctions.R')


i <- 29

pdf("smoothed.pdf",width=8,height=8)
DrawLCs(ids[i],
        data1,
        time_flux,
        smoother=TRUE,
        point.colors=FALSE,
        plot.unfolded=TRUE,
        plot.folded=FALSE,
        plot.folded.twice=TRUE,
        par=TRUE,
        plot.errors=TRUE,
        use.estimated.period=TRUE,
        period=1,
        green=FALSE,
        title3="")
dev.off()











## FOR QUAL
## choose 10 hipparcos light curves to plot

## plot unfolded
ids <- (1:nrow(data1))[data1$sources.survey=="hipparcos"][10:30]
for(ii in ids){
  filename <- data1[ii,"sources.xml_filename"]
  filename <- gsub(".xml","",filename)
  lc.class <- data1[ii,"sources.classification"]
  to_get <- time_flux$source_id==data1[ii,"features.source_id"]
  tfe <- time_flux[to_get,c(2,3,4)]
  tfe[,1] <- tfe[,1] - min(tfe[,1])
  pdf(paste("figures/",
            filename,".pdf",sep=""),
      height=4,width=8)
  par(mar=c(4.1,4.4,1.2,.4))
  plotLightCurve(tfe,maintitle=lc.class,
                 yLabel="mags",
                 cex.lab=1.5,
                 cex.main=1.8,
                 cex=.7)
  dev.off()

  period <- 2/data1[ii,"features.freq1_harmonics_freq_0"]
  tfe[,1] <- (tfe[,1] %% period) / period
  pdf(paste("figures/",
            filename,"folded.pdf",sep=""),
      height=4,width=8)
  par(mar=c(4.1,4.4,1.2,.4))
  plotLightCurve(tfe,maintitle=lc.class,
                 yLabel="mags",
                 xLabel=paste("Phase (Period = ",
                   round(period,3)," days)",sep=""),
                 cex.lab=1.5,
                 cex.main=1.8,
                 cex=.7)
  dev.off()

  tfe[,1] <- (tfe[,1] %% .5) / .5
  pdf(paste("figures/",
            filename,"foldedhalf.pdf",sep=""),
      height=4,width=8)
  par(mar=c(4.1,4.4,1.2,.4))
  plotLightCurve(tfe,maintitle=lc.class,
                 yLabel="mags",
                 xLabel=paste("Phase (Period = ",
                   round(period/2,3)," days)",sep=""),
                 cex.lab=1.5,
                 cex.main=1.8,
                 cex=.7)
  dev.off()

}

