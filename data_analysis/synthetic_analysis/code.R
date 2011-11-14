####
#### by James Long
#### date April 22, 2011
####
#### used for studying synthetic data
####


# filter variables with fewer than 5 unique responses

# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)


source('~/Rmodules/Rfunctions.R')
source('../denoisification_code/denoisification.R')
source('../denoisification_code/rf_denoise.R')
library('randomForest')
library('rpart')
library('xtable')
require('scatterplot3d')
require('fields')



## set the output graphics folder
graphics = fileOutLoc('figures/')
tables = fileOutLoc('tables/')
RData = fileOutLoc('RData/')

## get the data
features = '../../data_processed/simulated_data.dat'
tfe = '../../data_processed/simulated_data_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


## run the code that is used for all noisification analysis
source('../noisification_code/noisification_analysis.R')

## run the code that is used for robust analysis
source('../robust_code/robust.R')

## run denoisification
source('../denoisification_code/denoise_code.R')

#########
######### EXAMINE CLASS DENSITIES
#########


data1clean = subset(data1,subset=(sources.original_source_id==features.source_id))
nrow(data1clean)


data1train = subset(data1,subset=(sources.survey=="train"))
contains.random = grepl("random",data1train$sources.noise_args)
data1train$contains.random = contains.random
data1train$is_original = 1*(data1train$sources.original_source_id ==
  data1train$features.source_id)
data1train = dedupe(data1train,
  c("features.n_points","sources.original_source_id",
    "contains.random","is_original")
  )

to.select = ((data1train$features.n_points == 20) &
             (data1train$row_id == 0) &
             (!data1train$contains.random))
data1noisy = subset(data1train,subset=to.select)
nrow(data1noisy)



data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id",
  "features.max_slope","features.min",
  "features.linear_trend","features.max",
  "features.weighted_average","features.median",
  "features.freq1_harmonics_rel_phase_0")
data1_features = data1_features[!(data1_features %in%
  to_remove)]
data1_features

length(table(data1noisy$sources.classification))
length(table(data1clean$sources.classification))


source('../noisification_code/scatterplotVertical.R')

data1_features = c("features.freq1_harmonics_freq_0","features.amplitude","features.freq_varrat","features.flux_percentile_ratio_mid65","features.flux_percentile_ratio_mid50")

par(mfrow=c(1,2),ask=TRUE)
for(i in data1_features){
  classes = data1clean$sources.classification
  classes2 = data1noisy$sources.classification
  feat = data1clean[,i]
  feat2 = data1noisy[,i]
  if(length(unique(feat)) > 100 & length(unique(feat2)) > 100 &
     sum(is.na(feat)) == 0 & sum(is.na(feat2)) == 0){
    Draw3dScatterplot(feat,classes,i,class.cut=.02)
    Draw3dScatterplot(feat2,classes2,i,class.cut=.02)
  }
  else {
    print("==== WARNING ===")
    print("DID NOT DRAW FIG")
  }
  print("name of feature is")
  print(i)
  print("max clean feature is:")
  print(max(feat))
  print("max noisy feature is:")
  print(max(feat2))
}




#### POSTERIOR PROBABILITIES
#### examine posterior probabilities of noisified classifiers
####

load(RData('randomForestNoisificationResults.RData'))
summary(rfResults)
class(rfResults)
dim(rfResults)
class(rfResults[4,1][[1]])
summary(rfResults[4,1][[1]])

rfResults[4,1][[1]]$loss.exp
rfResults[4,1][[1]]$loss.exp.shrunk



for(i in 1:10){
class.names = colnames(rfResults[4,i][[1]]$class.predictions)
whichcolumns = vapply(rfResults[4,i][[1]]$true.class,
  function(x){which.max(x==class.names)},numeric(1)) - 1
correct.class = 1:nrow(
  rfResults[4,i][[1]]$class.predictions
  ) + whichcolumns*nrow(rfResults[4,i][[1]]$class.predictions)
probs.correct.class =
  rfResults[4,i][[1]]$class.predictions[correct.class]
print(mean(probs.correct.class))
print(mean(GetProbCorrectClass(rfResults[4,i][[1]]$class.predictions,
                          rfResults[4,i][[1]]$true.class)))
}

length(whichcolumns)
class(whichcolumns)

class(rfResults[4,1][[1]]$class.predictions)
head(rfResults[4,1][[1]]$class.predictions)

d1 = density(apply(rfResults[4,10][[1]]$class.predictions,1,max))
d2 = density(apply(rfResults[1,10][[1]]$class.predictions,1,max))
plot(d1,col='blue',lwd=2,xlab="Max Posterior Probability",main="Density of Maximum Posterior Probability")
lines(d2,col='orange',lty=2,lwd=2)



class(rfResults[4,1][[1]][[1]])


summary(rfResults[4,1][[1]][[2]])

class(rfResults[4,1][[1]][[2]])
dim(rfResults[4,1][[1]][[2]])

names(rfResults)
summary(rfResults[[[1]])

summary(rfResults[4,1])
rfResults[4,1])

one = "1"
a = list(one="1")
summary(a)



#### END POSTERIOR PROBABILITIES






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




####
#### look how noisification changes distributions
####

num.train = length(unique(
  data1train$sources.original_source_id))
num.test = length(
  unique(data1test$sources.original_source_id))
to.plot = min(num.test,num.train)

for(i in 1:length(points.levels)){

  ## plot the noisy test, clean train
  pdf(graphics(paste('freqBoth',
                   points.levels[i],'.pdf',sep="")),width=12,height=6)
  par(mfcol=c(1,2),mar=c(4,4,1,1))
  d1 = density(data1test$features.freq1_harmonics_freq_0[
    data1test$features.n_points==points.levels[i]][1:to.plot])
  trainplot = data1train$features.freq1_harmonics_freq_0[
    data1train$sources.original_source_id
    == data1train$features.source_id][1:to.plot]
  d2 = density(trainplot)
  d3 = density(data1test$features.freq1_harmonics_freq_0[
    data1test$features.n_points==points.levels[i]][1:to.plot])
  trainplot = data1train$features.freq1_harmonics_freq_0[
    data1train$row_id==1 & !data1train$contains.random
    & data1train$features.n_points==points.levels[i]][1:to.plot]
  d4 = density(trainplot)
  maxy = max(d2$y,d1$y,d3$y,d4$y)

  plot(d1,xlim=c(-1,5),col='orange',
       lty=1,lwd=2,xlab="Frequency",
       main="",ylim=c(0,maxy))
  lines(d2,col='blue',lty=2,lwd=2)
  legend(1,.9*maxy,c(paste(points.levels[i],
                        "Flux / Curve Test"),
                  "Well Sampled Training"),
         col=c("orange","blue"),lty=c(1,2),lwd=2,cex=1.3)

  plot(d3,xlim=c(-1,5),col='orange',lty=1,
       lwd=2,xlab="Frequency",main="",ylim=c(0,maxy))
  lines(d4,col='blue',lty=2,lwd=2)
  legend(1,.9*maxy,c(paste(points.levels[i],
                        "Flux / Curve Test"),
                  "Noisified Training"),
         col=c("orange","blue"),lty=c(1,2),lwd=2,cex=1.3)
  dev.off()
}











######## get formula
source('../noisification_code/Rfunctions.R')
rf_formula = GetFormula(data1)
rf_formula[[1]]
rf_formula[[2]]
rf_features = sub("features.","",rf_formula[[2]])
rf_features = rf_features[order(rf_features)]

outputX = xtable(as.matrix(rf_features),caption="Features") 
print(outputX,type='latex',file=tables('features.tex'),table.placement="H",include.rownames=FALSE,append=FALSE)
