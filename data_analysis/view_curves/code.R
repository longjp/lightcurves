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
features = '../../data_processed/hipparcos/sources00001.dat'
tfe = '../../data_processed/hipparcos/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


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

  ## fold on estimated period
  tfe[,1] = (tfe[,1] %% .5) / .5
  plotLightCurve(tfe,maintitle=period/2)  
  line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
  line.smu$y = -1 * line.smu$y
  lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
}



