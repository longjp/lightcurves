########## 
########## 
########## ANALYZE DATA IN data_processed/hipparcos 
##########
########## by James Long 
########## date: 4/17/2011 
########## 



# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)

source('../noisification_code/Rfunctions.R')
source('../denoisification_code/denoisification.R')
source('../denoisification_code/rf_denoise.R')
library('randomForest')
library('rpart')
library('MASS')
library('xtable')
require(scatterplot3d)
require(fields)


# set the output graphics folder
graphics = fileOutLoc('figures/')
tables = fileOutLoc('tables/')
RData = fileOutLoc('RData/')


## get the data
features = '../../data_processed/hipparcos/sources00001.dat'
tfe = '../../data_processed/hipparcos/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


## REMOVE ALL LC'S WITH N_POINTS = 5, THEY ARE HERE
## BECAUSE OF A BUG IN READ_DEBOSSCHER.PY
data1 = subset(data1,features.n_points != 5)


### SOME SUMMARY STATISTICS OF DATA, WRITTEN FOR hipparcos
data1clean = subset(data1,
  features.source_id==sources.original_source_id)
data1cleantrain = subset(data1clean,sources.survey=="train")
data1cleantest = subset(data1clean,sources.survey=="test")
sum(is.na(data1))
sum(colMeans(is.na(data1)) > 0)
sum(nrow(data1))
table(data1clean$sources.classification)
cbind(table(data1cleantrain$sources.classification),
      table(data1cleantest$sources.classification))
mean(data1clean$features.n_points[data1clean$sources.classification
                                  == "Delta Scuti"])
#### CLASS CUTOFF
#### 1. kill classes with fewer than 10 examples
#### 2. kill classes with 0 in training or 0 in test
less.ten = names(table(data1clean$sources.classification))[
  table(data1clean$sources.classification) < 10]
empty.train = names(table(data1cleantrain$sources.classification))[
  table(data1cleantrain$sources.classification) == 0]
empty.test =  names(table(data1cleantest$sources.classification))[
  table(data1cleantest$sources.classification) == 0]
to.kill = union(union(less.ten,empty.train),empty.test)
data1 = subset(data1,!(sources.classification %in% to.kill))
table(data1$sources.classification)
data1$sources.classification = as.factor(
  as.character(data1$sources.classification))
table(data1$sources.classification)


max(table(data1cleantest$sources.classification) / nrow(data1cleantest))


###
### check sizes of data
###
sum(data1$sources.survey == 'test') / 11
sum(data1$sources.survey == 'train' & data1$features.source_id
    == data1$sources.original_source_id)



### BEGIN NOT IMPORTANT
## simple comparison of LDA and random forest on clean data
## this code is not too important
data1clean = subset(data1,
  features.source_id==sources.original_source_id)
data1cleantrain = subset(data1clean,sources.survey=="train")
data1cleantest = subset(data1clean,sources.survey=="test")
nrow(data1clean)
nrow(data1cleantrain)
nrow(data1cleantest)
cbind(table(data1cleantrain$sources.classification),table(data1cleantest$sources.classification))
data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id",
  "features.max_slope","features.min",
  "features.linear_trend","features.max",
  "features.weighted_average","features.median",
  "features.freq1_harmonics_rel_phase_0")
data1_features = data1_features[!(data1_features %in%
  to_remove)]
rf_formula = formula(paste("sources.classification ~ ",
  paste(data1_features,collapse=" + ")))
rf_formula
rf.fit = randomForest(rf_formula,data1cleantrain)
lda.fit = lda(rf_formula,data1cleantrain)
lda.predict = predict(lda.fit,newdata=data1cleantest)
rf.predict = predict(rf.fit,newdata=data1cleantest)
mean(lda.predict$class != data1cleantest$sources.classification)
mean(rf.predict != data1cleantest$sources.classification)
### END NOT IMPORTANT



## necessary for real data b/c there is no definition of true period
## we DEFINE true_period to be period of clean sources
true_period_df = subset(data1,
  subset=features.source_id==sources.original_source_id,
  select=c("sources.original_source_id",
    "features.freq1_harmonics_freq_0"))
names(true_period_df) = c(names(true_period_df)[1],
       "sources.true_period")
true_period_df[,2] = 1 / true_period_df[,2]
data1$sources.true_period = NULL
data2 = merge(data1,true_period_df)
data1 = data2


## run noisification code
source('../noisification_code/noisification_analysis.R',echo=TRUE)

## run robust code
source('../robust_code/robust.R',echo=TRUE)

## run denoisification code
source('../denoisification_code/denoise_code.R',echo=TRUE)


####
#### combine noise / denoise performance comparison with
#### robustness plot
####
load(RData('denoise_code.RData'))
load(RData('randomForestNoisificationResults.RData'))

total.results = matrix(total.results,nrow=1)
total.results
total.results.sd = computeStandardErrors(total.results,500,sderror=1)
errorsSD[2,,] = total.results.sd[1,,]
data.to.plot = errorsSD[c(1,2,4),,]

linecolors = c("black","orange","blue")


load(RData('robustnessNoisificationResults.RData'))

## plot noise / denoise / naive on left plot
## plot robustness of noisification on right plot
pdf(graphics('noiseDenoiseRobust.pdf'),width=12,height=6)
par(mfcol=c(1,2),mar=c(4,4,.5,1))
plotLines(data.to.plot,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,linecolors=linecolors)
legend(60, .5,c("Naive","Denoisification","Noisification"),col=linecolors,lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
plotLines(errorsSD,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,maintitle="")
legend(50, .5,c("Naive","10-Point Noisification","50-Point Noisification","100-Point Noisification"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()

## break up preceding plot into 2 separate plots
pdf(graphics('noiseDenoise.pdf'),width=6,height=6)
par(mar=c(4,4,.5,1))
plotLines(data.to.plot,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,linecolors=linecolors)
legend(60, .5,c("Naive","Denoisification","Noisification"),col=linecolors,lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()
pdf(graphics('robust.pdf'),width=6,height=6)
par(mar=c(4,4,.5,1))
plotLines(errorsSD,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,maintitle="")
legend(50, .5,c("Naive","10-Point Noisification","50-Point Noisification","100-Point Noisification"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()





## some very basic numbers about OGLE data
## train
originals = data1$sources.original_source_id == data1$features.source_id & data1$sources.survey == "train"
sum(originals)
min(data1[originals,"features.n_points"])
max(data1[originals,"features.n_points"])
summary(data1[originals,"features.n_points"])
plot(density(data1[originals,"features.n_points"]))

## test
originals = data1$sources.original_source_id == data1$features.source_id & data1$sources.survey == "test"
sum(originals)
min(data1[originals,"features.n_points"])
max(data1[originals,"features.n_points"])
summary(data1[originals,"features.n_points"])
plot(density(data1[originals,"features.n_points"]))



#### not all that useful plots
num.train = length(unique(
  data1train$sources.original_source_id))
num.test = length(
  unique(data1test$sources.original_source_id))
to.plot = min(num.test,num.train)

for(i in 1:length(points.levels)){

  ## plot the noisy test, clean train
  pdf(graphics(paste('freqTrue',
                     points.levels[i],'.pdf',sep="")))
  d1 = density(data1test$features.freq1_harmonics_freq_0[
    data1test$features.n_points==points.levels[i]][1:to.plot])
  trainplot = data1train$features.freq1_harmonics_freq_0[
    data1train$sources.original_source_id
    == data1train$features.source_id][1:to.plot]
  d2 = density(trainplot)
  maxy = max(d2$y,d1$y)
  plot(d1,xlim=c(-1,5),col='orange',
       lty=1,lwd=2,xlab="Frequency",
       main="",ylim=c(0,maxy))
  lines(d2,col='blue',lty=2,lwd=2)
  legend(2.2,.7,c(paste(points.levels[i],
                        "Flux / Curve Test"),
                  "Well Sampled Training"),
         col=c("orange","blue"),lty=c(1,2),lwd=2)
  dev.off()

  ## plot the noisy test, noisified train
  pdf(graphics(paste('freq',
                     points.levels[i],
                     points.levels[i],
                     '.pdf',sep="")))
  d1 = density(data1test$features.freq1_harmonics_freq_0[
    data1test$features.n_points==points.levels[i]][1:to.plot])
  trainplot = data1train$features.freq1_harmonics_freq_0[
    data1train$row_id==1 & !data1train$contains.random
    & data1train$features.n_points==points.levels[i]][1:to.plot]
  d2 = density(trainplot)
  maxy = max(d2$y,d1$y)
  plot(d1,xlim=c(-1,5),col='orange',lty=1,
       lwd=2,xlab="Frequency",main="",ylim=c(0,maxy))
  lines(d2,col='blue',lty=2,lwd=2)
  legend(2.2,.7,c(paste(points.levels[i],
                        "Flux / Curve Test"),
                  "Noisified Training"),
         col=c("orange","blue"),lty=c(1,2),lwd=2)
  dev.off()
}

stop
## mira and beta lyrae
## all clean
## all 20 points

## bl.uma.clean = subset(data1train,(sources.classification %in% c("Beta Lyrae","Multiple Mode Cepheid")) & (sources.original_source_id == features.source_id))
## bl.uma.clean$sources.classification = as.character(bl.uma.clean$sources.classification)
## rp.clean = rpart(rf_formula,data=bl.uma.clean)
## rp.clean


## bl.uma.poor = subset(data1train,(sources.classification %in% c("Beta Lyrae","Multiple Mode Cepheid")) & (row_id == 0) & (features.n_points == 10) & !(contains.random))
## bl.uma.poor$sources.classification = as.character(bl.uma.poor$sources.classification)
## nrow(bl.uma.poor)
## rp.poor = rpart(rf_formula,data=bl.uma.poor)
## rp.poor




















