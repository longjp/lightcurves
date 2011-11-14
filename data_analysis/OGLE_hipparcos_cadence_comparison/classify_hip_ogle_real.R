########## 
########## 
########## ::CLASSIFY OGLE USING HIPPARCOS::
########## NOISIFYING FOR NUMBER FLUX, CADENCE, AND PHOTO-ERROR
##########
########## by James Long 
########## date: 8/22/2011
########## 

## load some functions
source("~/Rmodules/Rfunctions.R")
source("functions.R")
library("randomForest")
library("rpart")

Tables = fileOutLoc('tables/')
graphics = fileOutLoc('graphics/')

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
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle$sources.xml_filename = as.factor(
  data1ogle$sources.xml_filename)
data1ogle = na.roughfix(data1ogle)
data1ogle$sources.xml_filename = as.character(
  data1ogle$sources.xml_filename)
data1ogle = RemoveInfinities(data1ogle)

to_use = ((data1ogle$sources.classification ==
           "Classical Cepheid") &
          (runif(nrow(data1ogle)) < 1))
sum(to_use)
plot(data1ogle[to_use,feature],
     Ffeature(data1ogle$features.freq1_harmonics_freq_0[to_use]),
     xlab="fold2P90percentile",ylab="frequency")
to_use_hip = ((data1hip$sources.original_source_id ==
               data1hip$features.source_id) &
              data1hip$sources.classification == "RR Lyrae AB")
sum(to_use_hip)
points(data1hip[to_use_hip,feature],
       Ffeature(data1hip$features.freq1_harmonics_freq_0[
                    to_use_hip]),col='blue',pch=2)
legend('topright',c('ogle','hip'),col=c('black','blue'),
       pch=c(1,2))
abline(h=1)



#########
######### PRELIMINARY CLASSIFICATION
#########
## LOOK AT RANDOM FORESTS
rf_formula = GetFormula(data1hip)
rf_features = rf_formula[[2]]
rf_formula = rf_formula[[1]]
data1hip_fixed = na.roughfix(data1hip)
rf_fit = randomForest(rf_formula,
  data=data1hip_fixed[data1hip_fixed$sources.original_source_id
    ==data1hip_fixed$features.source_id,])
rf_fit

pdf('varImp_hip_on_ogle.pdf')
varImpPlot(rf_fit,main='Train on Hipparcos to Classify Ogle')
dev.off()

to_use = (data1hip$sources.original_source_id ==
          data1hip$features.source_id)
plot(log(data1hip[to_use,"features.freq1_harmonics_freq_0"]),
     log(100+data1hip[to_use,
                      "features.fold2P_slope_10percentile"]),
     col=data1hip$sources.classification[to_use])

dev.new()
plot(log(data1hip[to_use,"features.freq1_harmonics_freq_0"]),
     data1hip[to_use,"features.fold2P_slope_90percentile"],
     col=data1hip$sources.classification[to_use])


to_use = sample(1:nrow(data1ogle),nrow(data1ogle),replace=FALSE)
dev.new()
plot(log(data1ogle[to_use,"features.freq1_harmonics_freq_0"]),
     log(100 + data1ogle[to_use,"features.fold2P_slope_10percentile"]),
     col=data1ogle$sources.classification[to_use])

dev.new()
plot(log(data1ogle[to_use,"features.freq1_harmonics_freq_0"]),
     data1ogle[to_use,"features.fold2P_slope_90percentile"],
     col=data1ogle$sources.classification[to_use])



## what are the predictions
pred1 = predict(rf_fit,newdata=data1ogle,type='response')
mean(pred1 != data1ogle$sources.classification)
table(data1ogle$sources.classification,pred1)
table(data1ogle$sources.classification[!suspicious],pred1[!suspicious])

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



### implement john's idea TODO:point1
### classifier on ogle cepheids vs. noisified ogle sources
data1hip.noise.hip = subset(data1hip,
  (grepl('all',data1hip$sources.noise_args) &
   grepl('hip',data1hip$sources.noise_args) &
   sources.classification == "Classical Cepheid"))
data1hip.noise.hip$sources.classification = "hip"
nrow(data1hip.noise.hip)
data1hip.noise.ogle = subset(data1hip,
  (grepl('all',data1hip$sources.noise_args) &
   grepl('ogle',data1hip$sources.noise_args) &
   sources.classification == "Classical Cepheid"))
data1hip.noise.ogle$sources.classification = "ogle"
nrow(data1hip.noise.ogle)
data1hip.cepheids = rbind(data1hip.noise.hip,
  data1hip.noise.ogle)
data1hip.cepheids$sources.classification = as.factor(
  data1hip.cepheids$sources.classification)
nrow(data1hip.cepheids)
cepheid_classifier = rpart(rf_formula,data=data1hip.cepheids)
plot(cepheid_classifier)
text(cepheid_classifier)
cepheid_classifier


## differentiate noisified cepheids in hip from ogle
data1hip.cepheids = subset(data1hip,
  (grepl('all',data1hip$sources.noise_args) &
   grepl('hip',data1hip$sources.noise_args) &
   sources.classification == "Classical Cepheid"))
data1hip.cepheids$sources.classification = "hip"
nrow(data1hip.cepheids)

data1ogle.cepheids$sources.classification = "ogle"
nrow(data1ogle.cepheids)
data1cepheids = rbind(data1hip.cepheids,
  data1ogle.cepheids)
data1cepheids$sources.classification = as.factor(
  data1cepheids$sources.classification)
nrow(data1cepheids)
cepheid_classifier = rpart(rf_formula,data=data1cepheids)
plot(cepheid_classifier,margin=.1)
text(cepheid_classifier,use.n=TRUE)
cepheid_classifier

cepheid_classifier = randomForest(rf_formula,data=data1cepheids)
cepheid_classifier
dev.new()
varImpPlot(cepheid_classifier)






##### Do misclassified points look like they come from
##### hip noisified, not ogle
data1hip.noise.hip = subset(data1hip,
  (grepl('all',data1hip$sources.noise_args) &
   grepl('hip',data1hip$sources.noise_args)))
nrow(data1hip.noise.hip)
rf.clean.hip = randomForest(rf_formula,data=data1hip.noise.hip)
rf.clean.hip
dev.new()
varImpPlot(rf.clean.hip)
predictions = predict(rf.clean.hip,newdata=data1ogle)
table(data1ogle$sources.classification,predictions)




##### Do misclassified points look like they come from
##### hip noisified, not ogle TODO: point2
data1hip.noise.hip = subset(data1hip,
  (grepl('all',data1hip$sources.noise_args) &
   grepl('hip',data1hip$sources.noise_args)))
nrow(data1hip.noise.hip)
dev.new()
plot(log(data1hip.noise.hip$features.fold2P_slope_90percentile),
     log(data1hip.noise.hip$features.freq1_harmonics_freq_0),
     col=data1hip.noise.hip$sources.classification)

dev.new()
plot(log(data1ogle$features.fold2P_slope_90percentile),
     log(data1ogle$features.freq1_harmonics_freq_0),
     col=data1ogle$sources.classification)

mean((log(data1ogle$features.freq1_harmonics_freq_0)<0)[data1ogle$sources.classification == "RR Lyrae AB"])


nrow(data1hip.noise.hip)
rf.clean.hip = rpart(rf_formula,data=data1hip.noise.hip)
rf.clean.hip
dev.new()
plot(rf.clean.hip,margin=.1)
text(rf.clean.hip,use.n=TRUE)

varImpPlot(rf.clean.hip)
predictions = predict(rf.clean.hip,newdata=data1ogle,type='class')
table(data1ogle$sources.classification,predictions)




data1hip.clean = subset(data1hip,sources.original_source_id == features.source_id)
nrow(data1hip.clean)
rf.clean.hip = rpart(rf_formula,data=data1hip.clean)
rf.clean.hip
plot(rf.clean.hip,margin=.1)
text(rf.clean.hip,use.n=TRUE)
predictions = predict(rf.clean.hip,newdata=data1ogle,type='class')






############
############
############ NOISIFICATION ANALYSIS
############
############

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
  pdf(graphics(paste('varImpPlot',cadence,
                     points.levels[i],".pdf",sep="")))
  varImpPlot(rfClassifiers[[i]][[1]])
  dev.off()
}



## unadjusted priors
class.ratios = rep(1,number.classes)      
predictions = GetPredictions(data1ogle,points.levels,
  number.classifiers,number.classes,class.names,class.ratios)
table(data1ogle$sources.classification,predictions)
PrintConfusionMatrix(data1ogle$sources.classification,
                     predictions,
                     Tables(paste(cadence,
                                  'no_adjust.tex',sep='')))

table(data1ogle$sources.classification,predictions)
mean(data1ogle$sources.classification!=predictions)











## adjusting priors
class.ratios = (table(data1ogle$sources.classification) / table(data1hip$sources.classification[data1hip$sources.original_source_id == data1hip$features.source_id]))
predictions = GetPredictions(data1ogle,points.levels,
  number.classifiers,number.classes,class.names,class.ratios)
PrintConfusionMatrix(data1ogle$sources.classification,
                     predictions,
                     Tables(paste(cadence,'adjust.tex',sep='')))





data1 = data1hip[(grepl('all',data1hip$sources.noise_args) &
  grepl('hip',data1hip$sources.noise_args)),]
nrow(data1)
data1 = RemoveInfinities(data1)
rf1 = randomForest(rf_formula[1][[1]],data=data1)
varImpPlot(rf1)
predictions = predict(rf1,newdata=data1ogle)
table(data1ogle$sources.classification,predictions)
table(data1ogle$sources.classification[!suspicious],predictions[!suspicious])

data1classical = data1ogle[data1ogle$sources.classification == "Classical Cepheid" &
  !suspicious,]
nrow(data1classical)
data1classical.hip = subset(data1,sources.classification=="Classical Cepheid")
nrow(data1classical.hip)
source('../Rfunctions.R')
rf_formula = GetFormula(data1hip)
rf_formula[2]
data1classical$sources.classification = "ogle"
data1classical.hip$sources.classification = "hip"

cepheids = rbind(data1classical[,c(rf_formula[2][[1]],"sources.classification")],
  data1classical.hip[,c(rf_formula[2][[1]],"sources.classification")])
cepheids$sources.classification = as.factor(cepheids$sources.classification)
a_rpart = rpart(rf_formula[1][[1]],data=cepheids)
plot(a_rpart,margin=.1)
text(a_rpart,all=TRUE,use.n=TRUE)


a_rf = randomForest(rf_formula[1][[1]],data=cepheids)
a_rf
dev.new()
varImpPlot(a_rf)

#### using ogle cadence
                          

points.levels = (1:10)*10
number.classifiers = 5
class.names = levels(data1ogle$sources.classification)
number.classes = length(unique(data1ogle$sources.classification))


### set up the data frame
cadence = "ogle"
data1 = GetNoisified(data1hip,cadence)
rfClassifiers = GenerateClassifiers(data1,points.levels,rf_features,rf_formula)

### see which features the different classifiers are
### using
imp.to.make = c(1,5,10)
for(i in imp.to.make){
  pdf(graphics(paste('varImpPlot',cadence,
                     points.levels[i],".pdf",sep="")))
  varImpPlot(rfClassifiers[[i]][[1]])
  dev.off()
}


### try using on a few - 2 amplitude, frequency, 2P features


## unadjusted priors
class.ratios = rep(1,number.classes)      
predictions = GetPredictions(data1ogle,points.levels,
  number.classifiers,number.classes,class.names,class.ratios)
PrintConfusionMatrix(data1ogle$sources.classification,
                     predictions,
                     Tables(paste(cadence,'no_adjust.tex',sep='')))
table(data1ogle$sources.classification,predictions)
table(data1ogle$sources.classification[!suspicious],predictions[!suspicious])
mean(data1ogle$sources.classification!=predictions)
mean(data1ogle$sources.classification[!suspicious]!=predictions[!suspicious])




## adjusting priors
class.ratios = (table(data1ogle$sources.classification) / table(data1hip$sources.classification[data1hip$sources.original_source_id == data1hip$features.source_id]))
predictions = GetPredictions(data1ogle,points.levels,
  number.classifiers,number.classes,class.names,class.ratios)
PrintConfusionMatrix(data1ogle$sources.classification,
                     predictions,
                     Tables(paste(cadence,'adjust.tex',sep='')))






