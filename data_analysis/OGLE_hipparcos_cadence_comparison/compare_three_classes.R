########## 
########## 
########## COMPARE DISTRIBUTIONS OF FEATURES FOR LIGHTCURVES
########## FROM OGLE AND HIPPARCOS
##########
########## by James Long 
########## date: 8/22/2011
########## 

## load some functions
source("../Rfunctions.R")
library("randomForest")
library("rpart")

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




#########
######### PRELIMINARY CLASSIFICATION
#########
source('../Rfunctions.R')
## LOOK AT RANDOM FORESTS
rf_formula = GetFormula(data1hip)
rf_features = rf_formula[[2]]
rf_formula = rf_formula[[1]]
data1hip_fixed = na.roughfix(data1hip)
rf_fit = randomForest(rf_formula,
  data=data1hip_fixed[data1hip_fixed$sources.original_source_id
    ==data1hip_fixed$features.source_id,])
rf_fit
dev.new()
varImpPlot(rf_fit)

## what are the predictions
pred1 = predict(rf_fit,newdata=data1ogle,type='response')
mean(pred1 != data1ogle$sources.classification)
table(pred1,data1ogle$sources.classification)


## forest constructed on ogle data
rf_fit_ogle = randomForest(rf_formula,data=data1ogle)
rf_fit_ogle
varImpPlot(rf_fit_ogle)


## some analysis of error
incorrectly_classified = predictions != data1ogle$sources.classification
DrawKDES(data1ogle$features.n_points,data1ogle$sources.classification,
         main.title="",xlab="feat",ylab="Density",density.colors=NULL)
dev.new()
DrawKDES(data1ogle[incorrectly_classified,"features.n_points"],
         data1ogle[incorrectly_classified,"sources.classification"],
         main.title="",xlab="feat",ylab="Density",density.colors=NULL)

### NOW EXAMINE RPART
### 100 % accuracy, a super easy problem
rpart_fit = rpart(rf_formula,data=data1hip_fixed[data1hip_fixed$sources.original_source_id
    ==data1hip_fixed$features.source_id,])
plot(rpart_fit,margin=.1)
text(rpart_fit,all=TRUE,use.n=TRUE)
predictions = predict(rpart_fit,newdata=data1ogle,type="class")
mean(predictions != data1ogle$sources.classification)
table(predictions,data1ogle$sources.classification)

## now rpart constructed on ogle data
rpart_fit_ogle = rpart(rf_formula,data=data1ogle)
plot(rpart_fit_ogle,margin=.1)
text(rpart_fit_ogle,all=TRUE,use.n=TRUE)

rm(data1hip_fixed)






#########
######### now do full analysis using noisified classifier
#########


source('../Rfunctions.R')
source('functions.R')
Tables = fileOutLoc('tables/')
graphics = fileOutLoc('graphics/')
points.levels = (1:10)*10
number.classifiers = 5
class.names = levels(data1ogle$sources.classification)
number.classes = length(unique(data1ogle$sources.classification))


### set up the data frame
cadence = "hip"
data1 = GetNoisified(data1hip,cadence)
rfClassifiers = GenerateClassifiers(data1,points.levels,rf_features,rf_formula)

### see which features the different classifiers are
### using
imp.to.make = c(1,5,10)
for(i in imp.to.make){
  pdf(graphics(paste('varImpPlot',cadence,points.levels[i],".pdf",sep="")))
  varImpPlot(rfClassifiers[[i]][[1]])
  dev.off()
}


class.ratios = table(data1ogle$sources.classification) / table(data1hip$sources.classification[data1hip$sources.original_source_id == data1hip$features.source_id])

source('functions.R')
predictions = GetPredictions(data1ogle,points.levels,
  number.classifiers,number.classes,class.names,class.ratios)
predictions = t(predictions)
head(predictions)

pred.rotated = Rotate(predictions,10,0)
head(pred.rotated)
pred.rotated.projected = Project(pred.rotated)
head(pred.rotated.projected)
random.order = sample(1:nrow(pred.rotated.projected))
dev.new()
plot(pred.rotated.projected[random.order,],col=data1ogle$sources.classification[random.order])



predictions.class = class.names[apply(predictions,1,which.max)]
summary(rf_fit_ogle)
noisification.disagree = predictions.class != rf_fit_ogle$predicted


table(data1ogle$sources.classification[noisification.disagree])
random.order = sample(1:nrow(data1ogle))
plot(log(data1ogle$features.amplitude)[random.order],
     log(data1ogle$features.freq1_harmonics_freq_0)[random.order],
     col=(1+noisification.disagree)[random.order])


### the ones that classifier trained on ogle and noisified classifier disagree,
### where are most sources in the period, amplitude chart
### rerun using only period and amplitude


mean(predictions != data1ogle$sources.classification)



source('../Rfunctions.R')
PrintConfusionMatrix(data1ogle$sources.classification,
                     predictions,Tables(paste(cadence,'.tex',sep='')))






## confusion matrix (hip unsmoothed on ogle, hip smoothed to hip on ogle,
##    hip smoothed to ogle on ogle)
## probability adjusted and unadjusted
## two cadences + no noisification
## print the plot that john suggested


## visualize some stuff 
boxplot(data1$features.n_point ~ data1$sources.survey,ylab=
        "Number of Flux Measurements")



which_obs = data1$sources.survey %in% c("ogle","hip")
feature = "features.flux_percentile_ratio_mid20"
Ffeature = function(feature_values){
  return(feature_values)
}
source('../Rfunctions.R')
DrawKDES((Ffeature(data1[which_obs,feature])),
         data1$sources.survey[which_obs],
         xlab=feature)


