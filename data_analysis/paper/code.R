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

to_use_ogle_new = (to_use_ogle &
                   (data1ogle$features.freq1_harmonics_amplitude_0<100))
plot(data1ogle$features.amplitude[to_use_ogle_new],
     log(data1ogle$features.freq1_harmonics_amplitude_0[to_use_ogle_new]))
cor(data1ogle$features.amplitude[to_use_ogle_new],
     log(data1ogle$features.freq1_harmonics_amplitude_0[to_use_ogle_new]))

plot(1/data1ogle$features.freq1_harmonics_freq_0[to_use_ogle],
     log(data1ogle$features.freq1_harmonics_amplitude_0[to_use_ogle]))



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




###
### TODO: Turn into one plot with scatter on top of 
###       density
###

pdf('amplitude_miras.pdf',width=6,height=6)
par(mar=c(4,4,.5,1))
DrawKDES(amps,classes,
         ylab="Density",xlab='amplitude (magnitude)',
         density.colors=c(4,1),location='topright')
dev.off()


pdf('amplitude_vs_fluxnoise_miras.pdf',width=6,heigh=6)
par(mar=c(4,4,.5,1))
x = (c(log(data1ogle$x[to_use_ogle]),
       log(data1hip$x[to_use_hip])))
y = (c(data1ogle$features.amplitude[to_use_ogle],
       data1hip$features.amplitude[to_use_hip]))
pchs = c(rep(1,sum(to_use_ogle)),rep(2,sum(to_use_hip)))
col1 = (c(rep('#00000040',sum(to_use_ogle)),
          rep(4,sum(to_use_hip))))
random = 1:length(x)
plot(y[random],x[random],pch=pchs[random],
     col=col1[random],xlab="amplitude",
     ylab="log(mean photometric error)")
legend("topleft",c("hip","ogle"),col=c(4,1),
       pch=c(2,1),cex=1)
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
source("../Rfunctions.R")
library("randomForest")
library("rpart")

Tables = fileOutLoc('')
graphics = fileOutLoc('')


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
source('../Rfunctions.R')
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
to_use_hip = ((data1hip$sources.original_source_id == data1hip$features.source_id) &
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







to_use = ((data1ogle$sources.classification == "RR Lyrae AB"))
sum(data1ogle$features.freq1_harmonics_freq_0[to_use] < 1) / length(data1ogle$features.freq1_harmonics_freq_0[to_use])


i = 0

source_ids = data1ogle$features.source_id[to_use]
i = i + 1
DrawThreeLightCurves(source_ids[i])
