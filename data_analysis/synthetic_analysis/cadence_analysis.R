########## 
########## 
########## COMPARE ERRORS WITH MISSPECIFIED CADENCE 
##########  
##########
########## by James Long 
########## date: 7/27/11 
########## 


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)



source('~/Rmodules/Rfunctions.R')
library('randomForest')
library('rpart')
library('xtable')
require('scatterplot3d')
require('fields')




### We make a ton of kdes
MakeKDES = function(feature.name){
  to.select = (!(data1train$contains.random) &
               (data1train$row_id == 0) &
               (data1train$features.n_points == 10))
  feat = data1train[to.select,feature.name]
  classes = data1train[to.select,"sources.classification"]
  filename = paste(sub("features.","",feature.name),
    "KDEs.pdf",sep="")
  pdf(graphics(filename))
  Draw3dScatterplot(feat,classes,
                    xlab=paste(feature.name,
                      " - 10 flux",sep=""),
                    class.cut=.01,slack.level=.1)
  dev.off()
}

good_features = c("features.p2p_scatter_over_mad",
  "features.small_kurtosis",
  "features.p2p_scatter_2praw","features.beyond1std",
  "features.freq1_harmonics_amplitude_0",
  "features.freq1_harmonics_amplitude_1",
  "features.qso_log_chi2nuNULL_chi2nu",      
  "features.percent_difference_flux_percentile",
  "features.qso_log_chi2_qsonu",
  "features.flux_percentile_ratio_mid20",
  "features.freq1_harmonics_rel_phase_3",      
  "features.flux_percentile_ratio_mid80",      
  "features.skew",      
  "features.median_buffer_range_percentage",   
  "features.fold2P_slope_90percentile",        
  "features.median_absolute_deviation",      
  "features.flux_percentile_ratio_mid35",
  "features.freq1_harmonics_freq_0",      
  "features.flux_percentile_ratio_mid65",
  "features.freq_signif",
  "features.std",                  
  "features.scatter_res_raw",        
  "features.fold2P_slope_10percentile",      
  "features.flux_percentile_ratio_mid50",
  "features.percent_amplitude",
  "features.amplitude",       
  "features.p2p_scatter_pfold_over_mad",
  "features.p2p_ssqr_diff_over_var",
  "features.stetson_j",
  "features.stetson_k")




###
### SYNTHETIC TEST
###

## set the output graphics folder
graphics = fileOutLoc('figures/syn')
tables = fileOutLoc('tables/syn')
RData = fileOutLoc('RData/syn')

features = '../../data_processed/cadence_comparison/sources00001.dat'
tfe = '../../data_processed/cadence_comparison/tfe00001.dat'
data1total = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


## explore data1total a bit
table(data1total$sources.survey)


## get synthetic train
data1 = subset(data1total,sources.survey %in% c("synthetic_train","synthetic_test"))
data1$sources.survey = as.character(data1$sources.survey)
data1$sources.survey[data1$sources.survey == "synthetic_train"] = "train"
data1$sources.survey[data1$sources.survey == "synthetic_test"] = "test"
data1$sources.survey = as.factor(data1$sources.survey)
nrow(data1)
table(data1$sources.survey)

## run the code that is used for all noisification analysis
source('../noisification_code/noisification_analysis.R')
load(RData('randomForestNoisificationResults.RData'))
errorsSDcorrectCadence = errorsSD

plotLines(errorsSDcorrectCadence,points.levels)


### make a ton of kdes
for(i in 1:length(good_features)){
  MakeKDES(good_features[i])
}





## hipparcos

## set the output graphics folder
graphics = fileOutLoc('figures/hip')
tables = fileOutLoc('tables/hip')
RData = fileOutLoc('RData/hip')

## get hipparcos train
data1 = subset(data1total,sources.survey %in% c("hipparcos_train","synthetic_test"))
data1$sources.survey = as.character(data1$sources.survey)
data1$sources.survey[data1$sources.survey == "hipparcos_train"] = "train"
data1$sources.survey[data1$sources.survey == "synthetic_test"] = "test"
data1$sources.survey = as.factor(data1$sources.survey)
nrow(data1)
table(data1$sources.survey)

## run the code that is used for all noisification analysis
source('../noisification_code/noisification_analysis.R')
load(RData('randomForestNoisificationResults.RData'))
errorsSDwrongCadence = errorsSD



dim(errorsSDwrongCadence)
errorsSDnew = array(0,dim=c(4,10,3))

errorsSDnew[1,,] = errorsSDcorrectCadence[1,,]
errorsSDnew[2,,] = errorsSDcorrectCadence[4,,]
errorsSDnew[3,,] = errorsSDwrongCadence[1,,]
errorsSDnew[4,,] = errorsSDwrongCadence[4,,]


pdf(graphics('synTestCadence.pdf'))
plotLines(errorsSDnew,points.levels,ylab="Error Rate",xlab="Number Flux Test Set",maintitle="Synthetic Cadence for Test Data")
legend("topright",c("Synthetic Cadence Naive","Synthetic Cadence Noisified","Hipparcos Cadence Naive","Hipparcos Cadence Noisified"),col=c(1,2,3,4),lwd=2,cex=1,title="Training Sets",pch=1:4)
dev.off()





### make a ton of kdes
for(i in 1:length(good_features)){
  MakeKDES(good_features[i])
}






###
### HIPPARCOS TEST
###


## set the output graphics folder
graphics = fileOutLoc('figures/hip2')
tables = fileOutLoc('tables/hip2')
RData = fileOutLoc('RData/hip2')

## get synthetic train
data1 = subset(data1total,sources.survey %in% c("hipparcos_train","hipparcos_test"))
data1$sources.survey = as.character(data1$sources.survey)
data1$sources.survey[data1$sources.survey == "hipparcos_train"] = "train"
data1$sources.survey[data1$sources.survey == "hipparcos_test"] = "test"
data1$sources.survey = as.factor(data1$sources.survey)
nrow(data1)
table(data1$sources.survey)

## run the code that is used for all noisification analysis
source('../noisification_code/noisification_analysis.R')
load(RData('randomForestNoisificationResults.RData'))
errorsSDcorrectCadence = errorsSD

plotLines(errorsSDcorrectCadence,points.levels)






## synthetic

## set the output graphics folder
graphics = fileOutLoc('figures/syn2')
tables = fileOutLoc('tables/syn2')
RData = fileOutLoc('RData/syn2')

## get synthetic train
data1 = subset(data1total,sources.survey %in% c("synthetic_train","hipparcos_test"))
data1$sources.survey = as.character(data1$sources.survey)
data1$sources.survey[data1$sources.survey == "synthetic_train"] = "train"
data1$sources.survey[data1$sources.survey == "hipparcos_test"] = "test"
data1$sources.survey = as.factor(data1$sources.survey)
nrow(data1)
table(data1$sources.survey)

## run the code that is used for all noisification analysis
source('../noisification_code/noisification_analysis.R')
load(RData('randomForestNoisificationResults.RData'))
errorsSDwrongCadence = errorsSD

dim(errorsSDwrongCadence)
errorsSDnew = array(0,dim=c(4,10,3))


errorsSDnew[3,,] = errorsSDcorrectCadence[1,,]
errorsSDnew[4,,] = errorsSDcorrectCadence[4,,]
errorsSDnew[1,,] = errorsSDwrongCadence[1,,]
errorsSDnew[2,,] = errorsSDwrongCadence[4,,]

pdf(graphics('hipTestCadence.pdf'))
plotLines(errorsSDnew,points.levels,ylab="Error Rate",xlab="Number Flux Test Set",maintitle="Hipparcos Cadence for Test Data")
legend("topright",c("Synthetic Cadence Naive","Synthetic Cadence Noisified","Hipparcos Cadence Naive","Hipparcos Cadence Noisified"),col=c(1,2,3,4),lwd=2,cex=1,title="Training Sets",pch=1:4)
dev.off()

