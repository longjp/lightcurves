########## 
########## 
########## ::CLASSIFY OGLE USING HIPPARCOS::
########## NOISIFYING FOR NUMBER FLUX, CADENCE, AND PHOTO-ERROR
##########
########## PRODUCE SOME ANALYSIS OF WHAT WENT RIGHT AND
########## WRONG
##########
########## by James Long 
########## date: 8/22/2011
########## 

## load some functions
source("~/Rmodules/Rfunctions.R")
source("functions.R")
library("randomForest")
library("rpart")
library("ROCR")
options(width=50)

Tables = fileOutLoc('')
graphics = fileOutLoc('')

ogle_name = c("Mira","RR Lyrae AB","Classical Cepheid")
hip_name = c("Mira","RR Lyrae, Fundamental Mode",
  "Classical Cepheid")
name_conversion = cbind(ogle_name,hip_name)
name_conversion

## load the OGLE source
features = '../../../data_processed/ogleIIIall-fund.dat'
tfe = '../../../data_processed/ogleIIIall-fund-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)
nrow(data1ogle)
table(data1ogle$sources.classification)

## load the hipparcos sources
features = '../../../data_processed/hip_train_three_class.dat'
tfe = '../../../data_processed/hip_train_three_class_tfe.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)

## get rid of several classes
nrow(data1hip)
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[
  match(sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle$sources.xml_filename = as.factor(
  data1ogle$sources.xml_filename)
data1ogle = na.roughfix(data1ogle)
data1ogle$sources.xml_filename = as.character(
  data1ogle$sources.xml_filename)
data1ogle = RemoveInfinities(data1ogle)





##### FIG 1: DRAW UNDFOLDED HIPPARCOS LC
## i = 0
## i = i + 1
## source_id 65 is nice

hip.cepheids = data1hip$sources.original_source_id[data1hip$sources.noisification=="identity" & data1hip$sources.classification=="Classical Cepheid"]

i = 8
source('~/Rmodules/Rfunctions.R')
tfe = subset(time_flux_hip,
  subset=(source_id==hip.cepheids[i]),
  select=c("time","flux","error"))
tfe

pdf("cepheid_unfolded.pdf",width=12,height=6)
plotLightCurve(tfe,maintitle="")
dev.off()



##### FIG 2: DRAW FOLDED LIGHT CURVE, WITH OGLE SOURCE
## plot folded hipparcos l.c. with hipparcos points in grey
## and folded ogle points over them

source_period = 1/data1hip$features.freq1_harmonics_freq_0[
  data1hip$features.source_id == hip.cepheids[i]]
source_period
folded_times = (tfe[,1] %% source_period) / source_period
tfe_folded = tfe
tfe_folded[,1] = folded_times


### second plot
pdf("folded_cepheid.pdf",width=12,height=6)
plotLightCurve(tfe_folded,maintitle="",point.colors='grey')

line.smu = supsmu(tfe_folded[,1],tfe_folded[,2],periodic=TRUE)
line.smu$y = -1 * line.smu$y
lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)

j = 2
##j = 1 + j
## j = 2 is good
ogle_source = data1ogle$sources.original_source_id[j]
ogle_source_tfe = subset(time_flux_ogle,
  subset=(source_id==ogle_source),
  select=c("time","flux","error"))
nrow(ogle_source_tfe)


func = approxfun(line.smu$x,line.smu$y)

tfe = ogle_source_tfe
tfe[,1] = (ogle_source_tfe$time %% source_period) /
  source_period
tfe[,2] = func(tfe[,1]) + rnorm(n=length(tfe[,3]),
     mean=0,sd=tfe[,3])
points(tfe[,1],tfe[,2],col="blue",cex=1,lwd=3,pch=2)
sd.errors = 1
point.colors = "blue"
width.error.bar = .005
segments(tfe[,1],tfe[,2] - sd.errors*tfe[,3],tfe[,1],tfe[,2] + sd.errors*tfe[,3],point.colors)
segments(tfe[,1]-width.error.bar,tfe[,2] + sd.errors*tfe[,3],tfe[,1] + width.error.bar,tfe[,2] + sd.errors*tfe[,3],point.colors)
segments(tfe[,1]-width.error.bar,tfe[,2] - sd.errors*tfe[,3],tfe[,1] + width.error.bar,tfe[,2] - sd.errors*tfe[,3],point.colors)
## put legend here

dev.off()
### end second plot
###


##### FIG 3: DRAW FLUXES EXTRACTED AT THIS CADENCE
## plot folded hipparcos l.c. with hipparcos points in grey
## and folded ogle points over the

ogle_source_tfe[,2] = -1*tfe[,2]
pdf("ogle_cepheid_unfolded.pdf",width=12,height=6)
plotLightCurve(ogle_source_tfe,maintitle="")
dev.off()
