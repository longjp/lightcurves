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















#########
#########
######### 3 CLASS OGLE-HIPPARCOS EXAMPLE FOR PAPER
#########
#########

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
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle = na.roughfix(data1ogle)
data1ogle = RemoveInfinities(data1ogle)


## LOOK AT RANDOM FORESTS
rf_formula = GetFormula(data1hip)
rf_features = rf_formula[[2]]
rf_formula = rf_formula[[1]]
data1hip_fixed = na.roughfix(data1hip)
rf_fit = randomForest(rf_formula,
  data=data1hip_fixed[data1hip_fixed$sources.original_source_id
    ==data1hip_fixed$features.source_id,])
rf_fit
pdf(graphics('naive.pdf'))
varImpPlot(rf_fit,main='Train on Hipparcos to Classify Ogle')
dev.off()

## what are the predictions
pred1 = predict(rf_fit,newdata=data1ogle,type='response')
mean(pred1 != data1ogle$sources.classification)
table(pred1,data1ogle$sources.classification)
PrintConfusionMatrix(data1ogle$sources.classification,pred1,
                     table.name=Tables('naive.tex'))

## forest constructed on ogle data
rf_fit_ogle = randomForest(rf_formula,data=data1ogle)
rf_fit_ogle
PrintConfusionMatrix(data1ogle$sources.classification,
                     rf_fit_ogle$predicted,
                     table.name=Tables('ogle_on_ogle.tex'))
pdf(graphics('ogle_on_ogleVarImp.pdf'))
varImpPlot(rf_fit_ogle,main="Train on Ogle to Classify Ogle")
dev.off()


### NOW EXAMINE RPART
### 100 % accuracy, a super easy problem

rpart_fit = rpart(rf_formula,data=data1hip_fixed[data1hip_fixed$sources.original_source_id
    ==data1hip_fixed$features.source_id,])
predictions = predict(rpart_fit,newdata=data1ogle,type="class")
mean(predictions != data1ogle$sources.classification)
table(data1ogle$sources.classification,predictions)
PrintConfusionMatrix(data1ogle$sources.classification,predictions,
                     table.name=Tables('naive_cart.tex'))

## output confusion matrix


## rownames(rpart_fit$splits) = sub("features.",
##           "",rownames(rpart_fit$splits))
pdf(graphics('hip_classifier_tree.pdf'),width=6,height=6)
par(mar=c(0,0,0,0))
plot(rpart_fit,margin=.12)
text(rpart_fit,use.n=TRUE)
dev.off()


pdf(graphics('hip_classifier_feature_space.pdf'),width=6,
    height=6)
to_use = (data1hip$sources.original_source_id ==
          data1hip$features.source_id)
colors1 = c("black","orange","blue")
pch1 = c(1,2,3)
par(mar=c(4.1,4,2,.2))
plot(log(data1hip$features.freq1_harmonics_amplitude_0[to_use]),
     log(data1hip$features.fold2P_slope_90percentile[to_use]),
     col=colors1[data1hip$sources.classification[to_use]],
     pch=pch1[data1hip$sources.classification[to_use]],
     xlab='log(freq1_harmonics_amplitude)',
     ylab='log(fold2P_slope_90percentile)')
abline(v=log(.5615),col='grey')
lines(c(-10,log(.5615)),rep(log(1.365),2),col='grey')
legend('topright',levels(data1hip$sources.classification),col=colors1,pch=pch1)
dev.off()



###
### what does this look like for ogle data
pdf(graphics('hip_classifier_ogle_feature_space.pdf'),
    width=6,height=6)
nums = table(data1ogle$sources.classification)
rands = runif(nrow(data1ogle))
rr = (data1ogle$sources.classification == "RR Lyrae AB" &
      rands < .02)
ceph = (data1ogle$sources.classification == "Classical Cepheid" &
        rands < .08)
mira = (data1ogle$sources.classification == "Mira" &
        rands < .08)
to_use = rr | ceph | mira
sum(to_use)
to_use = (to_use &
          log(data1ogle$features.freq1_harmonics_amplitude_0)<4)
to_use = (to_use &
          log(data1ogle$features.fold2P_slope_90percentile)>-8)
to_use = sample((1:nrow(data1ogle))[to_use],sum(to_use))

colors1 = c("black","orange","blue")
pch1 = c(1,2,3)
par(mar=c(4.1,4,2,.2))
plot(log(data1ogle$features.freq1_harmonics_amplitude_0[to_use]),
     log(data1ogle$features.fold2P_slope_90percentile[to_use]),
     col=colors1[data1ogle$sources.classification[to_use]],
     pch=pch1[data1ogle$sources.classification[to_use]],
     xlab='log(freq1_harmonics_amplitude)',
     ylab='log(fold2P_slope_90percentile)')
abline(v=log(.5615),col='grey')
lines(c(-10,log(.5615)),rep(log(1.365),2),col='grey')
legend('topright',levels(data1ogle$sources.classification),col=colors1,pch=pch1)
dev.off()









#### get cross validated error for this problem
source('~/Rmodules/Rfunctions.R')
data1hip_good = subset(data1hip_fixed,sources.original_source_id ==
  features.source_id)
nrow(data1hip_good)
predictions = CVrpart(data1hip_good,rf_formula)
sum(levels(data1hip_good$sources.classification)[predictions] != data1hip_good$sources.classification)
mean(levels(data1hip_good$sources.classification)[predictions] != data1hip_good$sources.classification)


## now rpart constructed on ogle data
rpart_fit_ogle = rpart(rf_formula,data=data1ogle)
plot(rpart_fit_ogle,margin=.1)
text(rpart_fit_ogle,all=TRUE,use.n=TRUE)
predictions = predict(rpart_fit_ogle,type='class')
mean(predictions != data1ogle$sources.classification)
predictions = CVrpart(data1ogle,rf_formula)
mean(levels(data1ogle$sources.classification)[predictions]
     != data1ogle$sources.classification)




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
######### 1. mean mag vs period of cepheids OGLE vs hip
######### 2. noisification of cepheid periods
######### 3. noisification of rr lyrae periods
#########

## load some functions
source("~/Rmodules/Rfunctions.R")
library("randomForest")
library("rpart")

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
table(data1ogle$sources.classification)



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




## get mean mag of hipparcos sources
average.flux = aggregate(time_flux_hip$flux,
  by=list(time_flux_hip$source_id),mean)
sd.flux = aggregate(time_flux_hip$flux,
  by=list(time_flux_hip$source_id),sd)


head(average.flux)
names(average.flux) = c("features.source_id","average_flux")
names(sd.flux) = c("features.source_id","sd_flux")
nrow(average.flux)
data1hip.orig = subset(data1hip,
  sources.original_source_id==features.source_id)
nrow(data1hip.orig)
data1hip.orig = merge(data1hip.orig,average.flux)
nrow(data1hip.orig)
names(data1hip.orig)


## get mean magnitude of the cepheids in OGLE
average.flux = aggregate(time_flux_ogle$flux,
  by=list(time_flux_ogle$source_id),mean)
head(average.flux)
names(average.flux) = c("features.source_id","average_flux")
data1ogle = merge(data1ogle,average.flux)
nrow(data1ogle)
names(data1ogle)



to_plot = (data1ogle$sources.classification ==
           "Classical Cepheid")
sum(to_plot)
to_plot_hip = (data1hip.orig$sources.classification==
               "Classical Cepheid")
sum(to_plot_hip)


ymin = min(data1hip.orig$average_flux[to_plot_hip],
  data1ogle$average_flux[to_plot])
ymax = max(data1hip.orig$average_flux[to_plot_hip],
  data1ogle$average_flux[to_plot])

ymin = 0

pdf('cepheids_per_versus_mag_ogle_hip.pdf')
plot(log(1/data1ogle$features.freq1_harmonics_freq_0[to_plot],
         base=10),
     data1ogle$average_flux[to_plot],col="#00000020",
     ylim=c(ymin,ymax),
     xlab="log_10(period)",ylab="mean magnitude measurement")
points(log(1/data1ogle$features.freq1_harmonics_freq_0[
       to_plot & suspicious],base=10),
       data1ogle$average_flux[to_plot & suspicious],
       col="red",pch=2)
points(log(1/data1hip.orig$features.freq1_harmonics_freq_0[
       to_plot_hip],base=10),
       data1hip.orig$average_flux[to_plot_hip],
       col="orange",pch=2)
abline(h=11.5,col='grey')
legend('bottomleft',c('ogle','hip'),pch=c(1,2),
       col=c('black','orange'))
dev.off()




ogles = data1ogle$sources.classification == "RR Lyrae AB"
hips1 = (data1hip$sources.original_source_id ==
         data1hip$features.source_id &
        data1hip$sources.classification == "RR Lyrae AB")
hips2 = (grepl('all',data1hip$sources.noise_args) &
         grepl('hip',data1hip$sources.noise_args) &
         data1hip$sources.classification == "RR Lyrae AB")
sum(ogles)
sum(hips1)
sum(hips2)
pdf('rrlyrae_freq_hip_ogle.pdf')
DrawKDES(c(data1ogle$features.freq1_harmonics_freq_0[ogles],
           data1hip$features.freq1_harmonics_freq_0[hips1],
           data1hip$features.freq1_harmonics_freq_0[hips2]),
         c(rep("ogle",sum(ogles)),rep("hip",sum(hips1)),
         rep("hip noisified",sum(hips2))),
         xlab="frequency ( / day )",
         density.colors=c('black','blue','orange'))
dev.off()


source('~/Rmodules/Rfunctions.R')
ogles = data1ogle$sources.classification == "Classical Cepheid"
hips1 = (data1hip$sources.original_source_id ==
         data1hip$features.source_id &
         data1hip$sources.classification == "Classical Cepheid")
hips2 = (grepl('all',data1hip$sources.noise_args) &
         grepl('hip',data1hip$sources.noise_args) &
         data1hip$sources.classification == "Classical Cepheid")
sum(ogles)
sum(hips1)
sum(hips2)
pdf('cepheid_freq_hip_ogle.pdf')
DrawKDES(c(data1ogle$features.freq1_harmonics_freq_0[ogles],
           data1hip$features.freq1_harmonics_freq_0[hips1],
           data1hip$features.freq1_harmonics_freq_0[hips2]),
         c(rep("ogle",sum(ogles)),rep("hip",sum(hips1)),
         rep("hip noisified",sum(hips2))),
         xlab="frequency ( / day )",
         density.colors=c('black','blue','orange'))
dev.off()




####
feature = "features.p2p_scatter_over_mad"

pdf('p2p_scatter_ogle.pdf')
par(mar=c(4.5,4,.5,.5))
DrawKDES(data1ogle[,feature],
         data1ogle$sources.classification,
         xlab="P2PS")
dev.off()

hips1 = (data1hip$sources.original_source_id ==
         data1hip$features.source_id)
pdf('p2p_scatter_hip_unnoisified.pdf')
par(mar=c(4.5,4,.5,.5))
DrawKDES(data1hip[hips1,feature],
         data1hip[hips1,"sources.classification"],
         xlab="P2PS")
dev.off()


hips2 = (grepl('all',data1hip$sources.noise_args) &
         grepl('hip',data1hip$sources.noise_args))
pdf('p2p_scatter_hip_noisified.pdf')
par(mar=c(4.5,4,.5,.5))
DrawKDES(data1hip[hips2,feature],
         data1hip[hips2,"sources.classification"],
         xlab="P2PS")
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





