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

## load the OGLE miras
features = '../../data_processed/ogleIII-miras.dat'
tfe = '../../data_processed/ogleIII-miras-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)

## load the hipparcos
features = '../../data_processed/hipparcos/sources00001.dat'
tfe = '../../data_processed/hipparcos/tfe00001.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)


## get rid of all noisified miras in hipparcos
nrow(data1hip)
data1hip = subset(data1hip,sources.classification=="Mira")
data1hip = subset(data1hip,sources.original_source_id ==
  features.source_id)
nrow(data1hip)
data1hip$sources.classification = as.factor(as.character(
  data1hip$sources.classification))
data1hip$sources.survey = "hip"


## delete a bunch of junk rows
data1ogle$sources.xml_filename = NULL
data1ogle$sources.c1 = NULL
data1ogle$sources.c2 = NULL
data1ogle$sources.e1 = NULL
data1ogle$sources.e2 = NULL
data1hip$sources.c1 = NULL
data1hip$sources.c2 = NULL
data1hip$sources.e1 = NULL
data1hip$sources.e2 = NULL
data1hip$sources.classification = as.character(
  data1hip$sources.classification)
data1ogle$sources.classification = as.character(
  data1ogle$sources.classification)


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


source('../Rfunctions.R')
rf_formula = GetFormula(c("features.p2p_scatter_pfold_over_mad",
  "features.p2p_scatter_over_mad","features.freq_signif"))
rf_formula = rf_formula[[1]]
data1 = na.roughfix(data1)
data1$sources.classification = as.factor(data1$sources.survey)
table(data1$sources.classification)

rf_fit = randomForest(rf_formula,data=data1)
rf_fit

pdf('hipparcos_versus_ogle_miras_minus1.pdf')
varImpPlot(rf_fit)
dev.off()
