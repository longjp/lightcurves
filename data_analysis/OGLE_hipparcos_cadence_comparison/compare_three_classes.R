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

## load the OGLE miras
features = '../../data_processed/ogleIII.dat'
tfe = '../../data_processed/ogleIII-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)

## load the hipparcos
features = '../../data_processed/hipparcos/sources00001.dat'
tfe = '../../data_processed/hipparcos/tfe00001.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)


## get rid of several classes
nrow(data1hip)
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)
data1hip = subset(data1hip,
  sources.original_source_id == features.source_id)
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources
sources = name_conversion[match(sources,name_conversion[,"hip_name"]),"ogle_name"]
sources
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)





source('../Rfunctions.R')
## LOOK AT RANDOM FORESTS
data1 = data1hip
rf_formula = GetFormula()
rf_formula = rf_formula[[1]]
data1hip = na.roughfix(data1hip)
rf_fit = randomForest(rf_formula,data=data1hip)
rf_fit
varImpPlot(rf_fit)

data1ogle = na.roughfix(data1ogle)
predictions = predict(rf_fit,newdata=data1ogle,type="response")
sum(is.na(predictions))
mean(predictions != data1ogle$sources.classification)



### NOW EXAMINE RPART
### 100 % accuracy, a super easy problem
rpart_fit = rpart(rf_formula,data=data1hip)
plot(rpart_fit,margin=.1)
text(rpart_fit,all=TRUE,use.n=TRUE)
predictions = predict(rpart_fit,newdata=data1ogle,type="class")
mean(predictions != data1ogle$sources.classification)



## about 15% error in both cases, need to examine these a bit
incorrectly_classified = predictions != data1ogle$sources.classification



hist(data1ogle[incorrectly_classified,"features.n_points"])
table(data1ogle[incorrectly_classified,"sources.classification"])
summary(data1ogle[,"features.n_points"])


for(i in unique(data1hip$sources.classification)){
  print(i)
  print(summary(data1hip[data1hip$sources.classification==i,"features.n_points"]))
}




data1 = data1hip
time_flux = time_flux_hip
names(time_flux)

sources = data1$features.source_id


source('../Rfunctions.R')


pdf('acurve.pdf')
DrawThreeLightCurves(plot.folded.twice=FALSE)
dev.off()


stop



### analze periods for all hipparcos sources here



## delete a bunch of junk rows
## data1ogle$sources.xml_filename = NULL
## data1ogle$sources.c1 = NULL
## data1ogle$sources.c2 = NULL
## data1ogle$sources.e1 = NULL
## data1ogle$sources.e2 = NULL
## data1hip$sources.c1 = NULL
## data1hip$sources.c2 = NULL
## data1hip$sources.e1 = NULL
## data1hip$sources.e2 = NULL
## data1hip$sources.classification = as.character(
##   data1hip$sources.classification)
## data1ogle$sources.classification = as.character(
##   data1ogle$sources.classification)


## make sure survey ogle and hip are levels in
## both data sets
nrow(data1hip)
nrow(data1ogle)
data1 = rbind(data1ogle,data1hip)
nrow(data1)
data1$sources.survey = as.factor(data1$sources.survey)












##
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


