########## 
########## 
########## VIEW FOLDED CURVES 
##########  
##########
########## by James Long 
########## date: July 26, 2011 
########## 

rm(list=ls(all=TRUE))
source('../Rfunctions.R')



## get the data
features = '../../data_processed/OGLE/sources00001.dat'
tfe = '../../data_processed/OGLE/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)



####
#### good for visualizing unfolded, folded, and smoothed light curves
#### fairly slow
ids = unique(time_flux$source_id)
par(mfcol=c(3,1),ask=TRUE)
ids = ids[sample(1:length(ids))[1:10]]
for(ii in ids){
  tfe = subset(time_flux,subset=source_id==ii,select=c("time","flux","error"))
  tfe[,1] = tfe[,1] - min(tfe[,1])
  period = 2*(1 / data1$features.freq1_harmonics_freq_0[ii == data1$features.source_id &
    ii == data1$sources.original_source_id])
  lc.class = data1$sources.classification[ii == data1$features.source_id &
    ii == data1$sources.original_source_id]
  plotLightCurve(tfe,maintitle=as.character(lc.class))

  ## fold on twice estimated period
  tfe[,1] = (tfe[,1] %% period) / period
  plotLightCurve(tfe,maintitle=period)
  line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
  line.smu = supsmu(tfe[,1],tfe[,2])
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)

  
  ## fold on estimated period
  tfe[,1] = (tfe[,1] %% .5) / .5
  plotLightCurve(tfe,maintitle=period/2)  
  line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
  line.smu = supsmu(tfe[,1],tfe[,2])
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)
}



####
#### smooth and output all OGLE to OGLE_smoothed
####
#### TODO: MOVE THIS FUNCTION b/c DOES NOT INVOLVE VIEWING LCs
ids = unique(time_flux$source_id)
features = '../../data/OGLE_smoothed/'
for(ii in ids){
  print("processing:")
  print(ii)
  tfe = subset(time_flux,subset=source_id==ii,select=c("time","flux","error"))
  tfe[,1] = tfe[,1] - min(tfe[,1])
  period = 2*(1 / data1$features.freq1_harmonics_freq_0[ii == data1$features.source_id &
    ii == data1$sources.original_source_id])
  lc.class = data1$sources.classification[ii == data1$features.source_id &
    ii == data1$sources.original_source_id]
  tfe[,1] = (tfe[,1] %% period) / period
  line.smu = supsmu(tfe[,1],tfe[,2],span=.05,wt=1/tfe[,3],periodic=TRUE,bass=8)
  ## file and write
  ## 1a. data base id
  ## 1. source class
  ## 2. period
  ## 3. times
  ## 4. fluxes
  ## where does the ogle source id go
  filename = paste(features,ii,".dat",sep="")
  cat(ii,"\n",file=filename,sep="")
  cat(as.character(lc.class),"\n",file=filename,sep="",append=TRUE)
  cat(period,"\n",file=filename,sep="")
  cat(line.smu$x,"\n",file=filename,sep=" ",append=TRUE)
  cat(line.smu$y,file=filename,append=TRUE)
}




###
### much faster way of viewing light curves, saves graphs as
### pdfs, keep the pdf displayed and it will update (quickly)
### each time DrawRandomGraph is called

ids = unique(time_flux$source_id)
DrawRandomGraph()


DrawRandomGraph = function(){
  ii = sample(ids,1)
  pdf('curve.pdf')
  par(mfcol=c(3,1))
  tfe = subset(time_flux,
    subset=source_id==ii,select=c("time","flux","error"))
  tfe[,1] = tfe[,1] - min(tfe[,1])
  period = 2*(1 /
    data1$features.freq1_harmonics_freq_0[ii == data1$features.source_id &
    ii == data1$sources.original_source_id])
  lc.class =
    data1$sources.classification[ii == data1$features.source_id &
                                 ii == data1$sources.original_source_id]
  plotLightCurve(tfe,maintitle=as.character(lc.class))

  ## fold on twice estimated period
  tfe[,1] = (tfe[,1] %% period) / period
  plotLightCurve(tfe,maintitle=period)
  line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
  line.smu = supsmu(tfe[,1],tfe[,2],span=.05,wt=1/tfe[,3],periodic=TRUE,bass=8)
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)

  
  ## fold on estimated period
  tfe[,1] = (tfe[,1] %% .5) / .5
  plotLightCurve(tfe,maintitle=period/2)  
  line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
  line.smu = supsmu(tfe[,1],tfe[,2])
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)

  dev.off()
}






