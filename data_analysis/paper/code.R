########## 
########## 
########## CONTAINS CODE FOR MAKING ALL GRAPHICS FROM PAPER 
########## THAT ARE SELF CONTAINED (ie NOT THE OUTPUT OF
########## A CLASSIFIER)
##########
########## by James Long 
########## date: 9/9/2011 
########## 


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)

source('../Rfunctions.R')
source('../denoisification_code/denoisification.R')
source('../denoisification_code/rf_denoise.R')
library('randomForest')
library('rpart')
library('MASS')
library('xtable')
require(scatterplot3d)
require(fields)




###########
########### distribution of features for well sampled ogle
########### and poorly sampled ogle
###########
features = '../../data_processed/OGLE/sources00001.dat'
tfe = '../../data_processed/OGLE/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


data1$is_original = (data1$sources.original_source_id ==
                     data1$features.source_id)
data1$contains.random = grepl("random",
  data1$sources.noise_args)
data1 = dedupe(data1,
  c("features.n_points","sources.original_source_id",
    "contains.random","is_original")
  )


sum(data1$sources.survey == 'test') / 11
sum(data1$sources.survey == 'train' & data1$features.source_id
    == data1$sources.original_source_id)




feature = "features.freq1_harmonics_freq_0"
Ffeature = function(x){
  return(x)
}



pdf('ogle_sources_frequency_full.pdf')
par(mfcol=c(1,2))
to_use = (data1$features.source_id ==
          data1$sources.original_source_id)
sum(to_use)
Draw3dScatterplot(Ffeature(data1[to_use,feature]),
                  data1$sources.classification[to_use],
                  xlab="Frequency Full Light Curves")

dev.off()

n_points = 50
pdf(paste('ogle_sources_frequency',n_points,'.pdf',sep=""))
to_use = (data1$features.source_id !=
          data1$sources.original_source_id &
          !data1$contains.random &
          data1$row_id == 0 &
          data1$features.n_points == n_points)
sum(to_use)
Draw3dScatterplot(Ffeature(data1[to_use,feature]),
                  data1$sources.classification[to_use],
                  xlab=paste("Frequency with ",n_points,
                    sep=""))
dev.off()







########
########
######## DISTRIBUTION OF MIRA AMPLITUDES IN
######## HIPPARCOS AND OGLE
########
########

source("../Rfunctions.R")
source("functions.R")
library("randomForest")
library("rpart")

Tables = fileOutLoc('tables/')
graphics = fileOutLoc('graphics/')

## load the OGLE source
features = '../../data_processed/ogleIIIall.dat'
tfe = '../../data_processed/ogleIIIall-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)

## load the hipparcos sources
features = '../../data_processed/hip_train_three_class.dat'
tfe = '../../data_processed/hip_train_three_class_tfe.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)



data1hip = subset(data1hip,sources.original_source_id
  ==features.source_id)
nrow(data1hip)


### get max - min time for each source in each survey
time.differences = aggregate(time_flux_ogle$time,
  by=list(time_flux_ogle$source_id),
  function(x){max(x)-min(x)})
data1ogle = merge(data1ogle,time.differences,by.x="features.source_id",by.y="Group.1")

time.differences = aggregate(time_flux_hip$time,
  by=list(time_flux_hip$source_id),
  function(x){max(x)-min(x)})
data1hip = merge(data1hip,time.differences,by.x="features.source_id",by.y="Group.1")



### plot this
time.diffs = c(data1hip$x[data1hip$sources.classification=="Mira"],
  data1ogle$x[data1ogle$sources.classification=="Mira"])
length(time.diffs)
classes = c(rep("hip",length(data1hip$x[data1hip$sources.classification=="Mira"])),
  rep("ogle",length(data1ogle$x[data1ogle$sources.classification=="Mira"])))
length(classes)
DrawKDES(time.diffs,classes,
         ylab="Density",density.colors=NULL,location='topright')

dev.new()
amps = c(data1hip$features.amplitude[data1hip$sources.classification=="Mira"],
  data1ogle$features.amplitude[data1ogle$sources.classification=="Mira"])
classes = c(rep("hip",sum(data1hip$sources.classification=="Mira")),
  rep("ogle",sum(data1ogle$sources.classification=="Mira")))
length(amps)
DrawKDES(amps,classes,
         ylab="Density",density.colors=NULL,location='topright')

plot(time.diffs,amps)



pdf('amplitudes_miras.pdf')
feature = "features.amplitude"
amps = c(data1hip[data1hip$sources.classification=="Mira",feature],
  data1ogle[data1ogle$sources.classification=="Mira",feature])
classes = c(rep("hip",sum(data1hip$sources.classification=="Mira")),
  rep("ogle",sum(data1ogle$sources.classification=="Mira")))
length(amps)
DrawKDES(amps,classes,
         ylab="Density",density.colors=NULL,location='topright')
dev.off()



nrow(data1ogle)
data1ogle = subset(data1ogle,sources.classification=="Mira")
nrow(data1ogle)
to_use = data1ogle$features.amplitude < 6
data1ogle$pointrange = "below25"
data1ogle$pointrange[data1ogle$features.n_points >= 25 &
                     data1ogle$features.n_points < 50] = "25to50"
data1ogle$pointrange[data1ogle$features.n_points >= 50 &
                     data1ogle$features.n_points < 100] = "50to100"
data1ogle$pointrange[data1ogle$features.n_points >= 100] = "geq100"
table(data1ogle$pointrange)
DrawKDES(data1ogle$features.amplitude,data1ogle$pointrange,
         ylab="Density",density.colors=NULL,location='topright')












#####
##### RELATIONSHIP BETWEEN AMPLITUDE AND ESTIMATED FLUX ERROR
##### IN HIPPARCOS AND OGLE
#####


Tables = fileOutLoc('tables/')
graphics = fileOutLoc('graphics/')


ogle_name = c("Mira","RR Lyrae AB","Classical Cepheid")
hip_name = c("Mira","RR Lyrae, Fundamental Mode","Classical Cepheid")
name_conversion = cbind(ogle_name,hip_name)
name_conversion

## load the OGLE source
features = '../../data_processed/ogleIIIall.dat'
tfe = '../../data_processed/ogleIIIall-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)
nrow(data1ogle)

## load the hipparcos sources
features = '../../data_processed/hip_train_three_class.dat'
tfe = '../../data_processed/hip_train_three_class_tfe.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)

## get rid of several classes
nrow(data1hip)
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
data1hip = subset(data1hip,features.source_id == sources.original_source_id)
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle = na.roughfix(data1ogle)
data1ogle = RemoveInfinities(data1ogle)





## ogle - correlate amplitude with number points taken
to_use_ogle = (data1ogle$sources.classification == "Mira" &
          data1ogle$features.amplitude < 6)
plot(data1ogle$features.n_points[to_use_ogle],data1ogle$features.amplitude[to_use_ogle])

flux.error.ogle = aggregate(time_flux_ogle$error,
  by=list(time_flux_ogle$source_id),mean)
head(flux.error.ogle)
data1ogle = merge(data1ogle,flux.error.ogle,by.x="features.source_id",by.y="Group.1")
to_use_ogle = (data1ogle$sources.classification == "Mira" &
          data1ogle$features.amplitude < 6)
plot(log(data1ogle$x[to_use_ogle]),data1ogle$features.amplitude[to_use_ogle])
cor(log(data1ogle$x[to_use_ogle]),data1ogle$features.amplitude[to_use_ogle])
median(log(data1ogle$x[to_use_ogle]))
mean(log(data1ogle$x[to_use_ogle]) > -3)
mean(data1ogle$features.amplitude[to_use_ogle] > 3)


## ogle - correlate amplitude with number points taken
flux.error.hip = aggregate(time_flux_hip$error,
  by=list(time_flux_hip$source_id),mean)
head(flux.error.hip)
data1hip = merge(data1hip,flux.error.hip,by.x="features.source_id",by.y="Group.1")
to_use_hip = (data1hip$sources.classification == "Mira")
plot(log(data1hip$x[to_use_hip]),data1hip$features.amplitude[to_use_hip])
cor(log(data1hip$x[to_use_hip]),data1hip$features.amplitude[to_use_hip])
median(log(data1hip$x[to_use_hip]))






feature = "features.amplitude"
amps = c(data1hip[data1hip$sources.classification=="Mira",feature],
  data1ogle[data1ogle$sources.classification=="Mira",feature])
classes = c(rep("hip",sum(data1hip$sources.classification=="Mira")),
  rep("ogle",sum(data1ogle$sources.classification=="Mira")))
length(amps)


pdf('amplitudes_fluxnoise_miras.pdf',width=12,height=6)
par(mfcol=c(1,2),mar=c(4,4,.5,1))
DrawKDES(amps,classes,
         ylab="Density",xlab='amplitude',density.colors=c(4,1),location='topright')

x = c(log(data1ogle$x[to_use_ogle]),log(data1hip$x[to_use_hip]))
y = c(data1ogle$features.amplitude[to_use_ogle],data1hip$features.amplitude[to_use_hip])
pchs = c(rep(1,sum(to_use_ogle)),rep(2,sum(to_use_hip)))
col1 = c(rep('#00000040',sum(to_use_ogle)),rep(4,sum(to_use_hip)))
random = 1:length(x)
par(mar=c(4,4,.5,1))
plot(y[random],x[random],pch=pchs[random],col=col1[random],xlab="amplitude",
     ylab="log(mean photometric error)")
legend("topleft",c("hip","ogle"),col=c(4,1),
         pch=c(2,1),cex=1)
dev.off()


