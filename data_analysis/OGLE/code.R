########## 
########## 
########## ANALYZE DATA IN data_processed/OGLE 
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
library('xtable')

# set the output graphics folder
graphics = fileOutLoc('figures/')
tables = fileOutLoc('tables/')
RData = fileOutLoc('RData/')


# get the data
features = '../../data_processed/OGLE/sources00001.dat'
tfe = '../../data_processed/OGLE/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)

sum(data1$sources.survey == 'test') / 11
sum(data1$sources.survey == 'train' & data1$features.source_id == data1$sources.original_source_id)



# necessary for real data b/c there is no definition of true period
# we DEFINE true_period to be period of clean sources
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
source('../noisification_code/noisification_analysis.R')




## run robust code
source('../robust_code/robust.R')

## run denoisification code
source('../denoisification_code/denoise_code.R')



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
