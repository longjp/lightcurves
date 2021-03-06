########## 
########## 
########## CONTAINS CODE FOR MAKING ALL GRAPHICS FROM PAPER 
########## THAT ARE SELF CONTAINED (ie NOT THE OUTPUT OF
########## A CLASSIFIER)
##########
########## by James Long 
########## date: 9/9/2011 
########## 


############
############ NOTE: MOST OF THIS CODE IS NO LONGER NEEDED
############ BECAUSE GRAPHICS ARE NOW CREATED IN THEIR
############ OWN FOLDERS.
############


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)

source('~/Rmodules/Rfunctions.R')
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


















####
#### NOW VISUALIZE FEATURES
####
source('../Rfunctions.R')
feature = "features.freq1_harmonics_amplitude_0"
graph_colors = c("black","blue","orange")

Ffeature = function(x){
  return(log(x+1))
}

## get the right subsets
subsamp = runif(nrow(data1ogle)) < .1
sum(data1hip$sources.original_source_id==
    data1hip$features.source_id)
originals = (data1hip$sources.original_source_id==
             data1hip$features.source_id)
sum(originals)


trim = .01
first_vline = .5615
vline = Ffeature(first_vline)
xlab = "log(freq1_harmonics_amplitude_0+1)"
pdf(graphics(paste(sub("features.","",feature),
                   ".pdf",sep="")),height=6,width=12)
par(mfcol=c(1,2))
xmin = quantile(c(Ffeature(data1ogle[subsamp,feature]),
  Ffeature(data1hip[originals,feature])),trim)
xmax = quantile(c(Ffeature(data1ogle[subsamp,feature]),
  Ffeature(data1hip[originals,feature])),1-trim)
DrawKDES(Ffeature(data1hip[originals,feature]),
         data1hip$sources.classification[originals],
         main.title="",
         xlab=xlab,
         location='topright',
         trim=trim,
         xlimits=c(xmin,xmax),
         density.colors=graph_colors)
abline(v=vline,col='grey')
DrawKDES(Ffeature(data1ogle[subsamp,feature]),
         data1ogle$sources.classification[subsamp],
         main.title="",
         xlab=xlab,
         location='topright',
         trim=trim,
         xlimits=c(xmin,xmax),
         density.colors=graph_colors)
abline(v=vline,col='grey')
dev.off()


time_flux = time_flux_ogle
data1 = data1ogle




TempPlots = function(source_id){  
  DrawThreeLightCurves(source_id,smoother=TRUE,plot.unfolded=TRUE,
                       plot.folded=TRUE,plot.folded.twice=TRUE,par=FALSE)
  to_use = data1ogle$sources.classification == "Mira"
  sum(to_use) / sum(data1ogle$sources.classification == "Mira")
  sum(to_use)
  plot(log(data1ogle[to_use,feature]),
       log(data1ogle[to_use,"features.freq1_harmonics_freq_0"]))
  points(log(data1ogle[data1ogle$features.source_id==source_id,feature]),
         log(data1ogle[data1ogle$features.source_id==source_id,
                       "features.freq1_harmonics_freq_0"]),col='red')
  abline(v=log(first_vline),col='grey')
  abline(h=log(1/50),col='grey')

}


to_use = data1ogle$sources.classification == "Mira"
ordered.source.ids = data1ogle$features.source_id[order(data1ogle[to_use,feature])]
i = 0


dev.new()
to_use = (data1hip$sources.classification == "Mira" &
          data1hip$sources.original_source_id == data1hip$features.source_id)
sum(to_use) / sum(data1hip$sources.classification == "Mira")
sum(to_use)
plot(log(data1hip[to_use,feature]),
     log(data1hip[to_use,"features.freq1_harmonics_freq_0"]))


dev.new()
to_use = data1ogle$sources.classification == "Mira"
sum(to_use) / sum(data1ogle$sources.classification == "Mira")
sum(to_use)
plot(log(data1ogle[to_use,feature]),
     log(data1ogle[to_use,"features.freq1_harmonics_freq_0"]))



i = i + 1
matrix1 = matrix(c(1,2,3,1,2,3,4,4,4,4,4,4),3,4,byrow=FALSE)
layout(matrix1)
TempPlots(ordered.source.ids[i])
ordered.source.ids[i]

### interesting ones id = 5521, 5204


is_mira = data1ogle$sources.classification== "Mira"
period_le_20 = data1ogle$features.freq1_harmonics_freq_0 > 1/50
freq_amp = data1ogle[,feature] < first_vline



          

subsamp = (subsamp & data1ogle[,feature] < first_vline &
           data1ogle$sources.classification != "Mira")
originals = (originals & data1hip[,feature] < first_vline &
             data1hip$sources.classification != "Mira")

feature = "features.fold2P_slope_90percentile"
xlab = 'log(fold2P_slope_90percentile+1)'
vline = Ffeature(1.365)
pdf(graphics(paste(sub("features.","",feature),
                   ".pdf",sep="")),height=6,width=12)
par(mfcol=c(1,2))
sum(is.na(c(Ffeature(data1ogle[subsamp,feature]),
            Ffeature(data1hip[originals,feature]))))
xmin = quantile(c(Ffeature(data1ogle[subsamp,feature]),
  Ffeature(data1hip[originals,feature])),trim)
xmax = quantile(c(Ffeature(data1ogle[subsamp,feature]),
  Ffeature(data1hip[originals,feature])),1-trim)
DrawKDES(Ffeature(data1hip[originals,feature]),
         data1hip$sources.classification[originals],
         main.title="",
         xlab=xlab,
         location='topright',
         trim=.0,
         xlimits=c(xmin,xmax),
         density.colors=graph_colors[2:3])
abline(v=vline,col='grey')
DrawKDES(Ffeature(data1ogle[subsamp,feature]),
         data1ogle$sources.classification[subsamp],
         main.title="",
         xlab=xlab,
         location='topright',
         trim=.0,
         xlimits=c(xmin,xmax),
         density.colors=graph_colors[2:3])
abline(v=vline,col='grey')
dev.off()



flux.error.ogle = aggregate(time_flux_ogle$error,
  by=list(time_flux_ogle$source_id),mean)
data1ogle = merge(data1ogle,flux.error.ogle,by.x="features.source_id",by.y="Group.1")
to_use = ((data1ogle$sources.classification == "RR Lyrae AB") &
          (runif(nrow(data1ogle)) < .1))
sum(to_use)
plot(data1ogle[to_use,feature],log(data1ogle$x[to_use]))
cor(data1ogle[to_use,feature],log(data1ogle$x[to_use]))


Ffeature = function(x){
  return(x)
}




pdf('frequency_90percentile_hip_ogle.pdf')
to_use = ((data1ogle$sources.classification == "RR Lyrae AB") &
          (runif(nrow(data1ogle)) < .1))
sum(to_use)
plot(data1ogle[to_use,feature],
     Ffeature(data1ogle$features.freq1_harmonics_freq_0[to_use]),
     xlab="fold2P90percentile",ylab="frequency")
to_use_hip = ((data1hip$sources.original_source_id ==
               data1hip$features.source_id) &
              data1hip$sources.classification == "RR Lyrae AB")
sum(to_use_hip)
points(data1hip[to_use_hip,feature],
       Ffeature(data1hip$features.freq1_harmonics_freq_0[to_use_hip]),col='blue',pch=2)
legend('topright',c('ogle','hip'),col=c('black','blue'),pch=c(1,2))
abline(h=1)
dev.off()





Xfeature = function(x){
  return(log(x))
}

Yfeature = function(x){
  return(log(x))
}



xlim = c(Xfeature(0.001),Xfeature(20))
ylim = c(Yfeature(0.001),Yfeature(8))
ogle_color = '#00000020'


pdf('RRLyraeNoisification.pdf',height=4,width=12)
par(mfcol=c(1,3),mar=c(4,4,.5,1))
to_use = ((data1ogle$sources.classification == "RR Lyrae AB") &
          (runif(nrow(data1ogle)) < .3))
sum(to_use)
plot(Xfeature(data1ogle[to_use,feature]),
     Yfeature(data1ogle$features.freq1_harmonics_freq_0[to_use]),
     xlab="log(fold2P90percentile)",ylab="log(frequency)",
     xlim=xlim,ylim=ylim,col=ogle_color)
to_use_hip = ((data1hip$sources.original_source_id == data1hip$features.source_id) &
              data1hip$sources.classification == "RR Lyrae AB")
sum(to_use_hip)
points(Xfeature(data1hip[to_use_hip,feature]),
       Yfeature(data1hip$features.freq1_harmonics_freq_0[to_use_hip]),col='blue',pch=2)
legend('topleft',c('ogle','hip'),col=c('black','blue'),pch=c(1,2))
##abline(h=Yfeature(1))


to_use = ((data1ogle$sources.classification == "RR Lyrae AB") &
          (runif(nrow(data1ogle)) < .3))
sum(to_use)
plot(Xfeature(data1ogle[to_use,feature]),
     Yfeature(data1ogle$features.freq1_harmonics_freq_0[to_use]),
     xlab="log(fold2P90percentile)",ylab="log(frequency)",
     xlim=xlim,ylim=ylim,col=ogle_color)
to_use_hip = (grepl("all",data1hip$sources.noise_args) &
          grepl("hip",data1hip$sources.noise_args))
sum(to_use_hip)
points(Xfeature(data1hip[to_use_hip,feature]),
       Yfeature(data1hip$features.freq1_harmonics_freq_0[to_use_hip]),col='blue',pch=2)
legend('topleft',c('ogle','hip'),col=c('black','blue'),pch=c(1,2))



to_use = ((data1ogle$sources.classification == "RR Lyrae AB") &
          (runif(nrow(data1ogle)) < .3))
sum(to_use)
plot(Xfeature(data1ogle[to_use,feature]),
     Yfeature(data1ogle$features.freq1_harmonics_freq_0[to_use]),
     xlab="log(fold2P90percentile)",ylab="log(frequency)",
     xlim=xlim,ylim=ylim,col=ogle_color)
to_use_hip = (grepl("all",data1hip$sources.noise_args) &
          grepl("ogle",data1hip$sources.noise_args))
sum(to_use_hip)
points(Xfeature(data1hip[to_use_hip,feature]),
       Yfeature(data1hip$features.freq1_harmonics_freq_0[to_use_hip]),col='blue',pch=2)
legend('topleft',c('ogle','hip'),col=c('black','blue'),pch=c(1,2))
dev.off()











###########
########### show difference in distribution of frequency between RR lyrae and
########### cepheids in OGLE vs hipparcos
###########


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)


## load some functions
source("~/Rmodules/Rfunctions.R")
library("randomForest")
library("rpart")

Tables = fileOutLoc('')
graphics = fileOutLoc('')


ogle_name = c("Mira","RR Lyrae AB","Classical Cepheid")
hip_name = c("Mira","RR Lyrae, Fundamental Mode",
  "Classical Cepheid")
name_conversion = cbind(ogle_name,hip_name)
name_conversion

## load the OGLE source
features = '../../data_processed/ogleIIIall-fund.dat'
tfe = '../../data_processed/ogleIIIall-fund-tfe.dat'
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
table(data1hip$sources.classification[
  data1hip$sources.original_source_id
  == data1hip$features.source_id])
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(
  sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle$sources.xml_filename = as.factor(
  data1ogle$sources.xml_filename)
data1ogle = na.roughfix(data1ogle)
data1ogle$sources.xml_filename = as.character(
  data1ogle$sources.xml_filename)
data1ogle = RemoveInfinities(data1ogle)




hips1 = (data1hip$sources.original_source_id == data1hip$features.source_id &
        data1hip$sources.classification != "Mira")
sum(hips1)
pdf('cepheid_rr_freq_hip.pdf')
par(mar=c(4.2,4,.5,1))
DrawKDES(log(data1hip$features.freq1_harmonics_freq_0[hips1],base=10),
         data1hip$sources.classification[hips1],xlab="log(frequency) ( / day )",
         location='topleft',xlimits=c(-2,.5))
dev.off()



ogles = data1ogle$sources.classification != "Mira"
sum(ogles)
pdf('cepheid_rr_freq_ogle.pdf')
par(mar=c(4.2,4,.5,1))
DrawKDES(log(data1ogle$features.freq1_harmonics_freq_0[ogles],base=10),
         data1ogle$sources.classification[ogles],xlab="log(frequency) ( / day )",
         location='topleft',xlimits=c(-2,.5))
dev.off()








#########
######### for second table, some basic data set statistics
#########

## compute time difference between all successive flux
## measurements
Diffs = function(x){
  x = x[order(x)]
  return(x[2:length(x)] - x[1:(length(x) - 1)])
}


##
## for OGLE
##

## class proportions
table(data1ogle$sources.classification) / nrow(data1ogle)
nrow(data1ogle)
## number flux measurements
number.measurements = aggregate(time_flux_ogle$time,
  list(source_id_num_flux=time_flux_ogle$source_id),length)
length(number.measurements)
dim(number.measurements)
quantile(number.measurements[,2],c(.25,.75))
## time differences
differences = aggregate(time_flux_ogle$time,
  list(source_id_num_flux=time_flux_ogle$source_id),Diffs)
differences = lapply(differences[,2],unlist)
differences = unlist(differences)
quantile(differences,c(.25,.75))


total_diffs_ogle = TotalDiffs(time_flux_ogle)
length(total_diffs_ogle)
quantile(total_diffs_ogle,c(.25,.75))
## photometric error
quantile(time_flux_ogle[,4],c(.25,.75))


##
## for hipparcos
##

## class proportions
to_use = (data1hip$sources.original_source_id ==
          data1hip$features.source_id)
table(data1hip$sources.classification[to_use])
table(data1hip$sources.classification[to_use]) / length(data1hip$sources.classification[to_use])
length(data1hip$sources.classification[to_use])
## number flux measurements
number.measurements = aggregate(time_flux_hip$time,
  list(source_id_num_flux=time_flux_hip$source_id),length)
length(number.measurements)
dim(number.measurements)
quantile(number.measurements[,2],c(.25,.75))
## time differences
differences = aggregate(time_flux_hip$time,
  list(source_id_num_flux=time_flux_hip$source_id),Diffs)
differences = lapply(differences[,2],unlist)
differences = unlist(differences)
quantile(differences,c(.25,.75))
quantile(differences,c(.25,.75))
## photometric error 
quantile(time_flux_hip[,4],c(.25,.75))





