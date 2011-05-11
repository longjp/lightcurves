####
#### by James Long
#### date May 3, 2011
####
#### used for studying denoisification on synthetic data
####


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)

source('../noisification_code/Rfunctions.R')
library('randomForest')
library('rpart')
library('xtable')

# set the output graphics folder
graphics = fileOutLoc('figures/')
tables = fileOutLoc('tables/')

# get the data
features = '../../data_processed/synthetic_analysis/sources00001.dat'
tfe = '../../data_processed/synthetic_analysis/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)

names(data1)
head(data1)
sum(is.na(data1))

names(time_flux)
head(time_flux)
sum(is.na(time_flux))

print("number of values imputed in data1:")
print(sum(is.na(data1)))


data1 = na.roughfix(data1) # this isn't fair, uses better data
data1[data1==Inf] = 0


###
### IMPORTANT:: Divide into test and training.
###  Following code is very sensitive to whether
###  this is done right.
###
data1test = subset(data1,subset=(sources.survey=="test"))
data1train = subset(data1,subset=(sources.survey=="train"))

# cut the test down to 1/10 size
ids = (unique(data1test$sources.original_source_id))[1:25]
length(ids)
data1test = subset(data1test,subset=(data1test$sources.original_source_id %in% ids))
nrow(data1test)



nrow(data1test)
nrow(data1train)
contains.random = grepl("random",data1train$sources.noise_args)
data1train$contains.random = contains.random
data1train = dedupe(data1train,
  c("features.n_points","sources.original_source_id",
    "contains.random")
  )
length(unique(data1train$row_id))
table(data1train$row_id)

### CRITICAL:: This is how period plots determine what
### obs to use. If this is not set correctly clean test
### curves may be used in graphs. Usually
### points.levels = c(10,20,30,40,50,60,70,80,90,100)
points.levels = unique(data1test$features.n_points[
  data1test$features.source_id !=
  data1test$sources.original_source_id])
points.levels = points.levels[order(points.levels)]
points.levels

### ALSO IMPORTANT:: Have an assigned order for classes
class.names = names(table(data1$sources.classification))

WrapLines = function(){
   nNN = 5
   neighbors = array(0,dim=c(length(unique(data1test$sources.original_source_id)),nNN,length(points.levels)))
   imp.variables = c("features.freq1_harmonics_freq_0","features.amplitude")
   ##imp.variables = c("features.beyond1std","features.max_slope","features.skew","features.amplitude","features.freq1_harmonics_freq_0")
   ##imp.variables = c("features.p2p_scatter_over_mad","features.small_kurtosis","features.p2p_scatter_2praw","features.beyond1std","features.max_slope","features.qso_log_chi2nuNULL_chi2nu","features.percent_difference_flux_percentile","features.qso_log_chi2_qsonu","features.flux_percentile_ratio_mid20","features.flux_percentile_ratio_mid35","features.flux_percentile_ratio_mid80","features.flux_percentile_ratio_mid50","features.skew","features.fold2P_slope_10percentile","features.amplitude","features.p2p_scatter_pfold_over_mad","features.stetson_j","features.stetson_k","features.medperc90_2p_p","features.freq1_harmonics_freq_0","features.median_buffer_range_percentage","features.std","features.freq1_harmonics_rel_phase_1","features.scatter_res_raw","features.freq_signif")

   ## now do euclidean distance
   for(i in 1:(dim(neighbors)[3])){

     ## get the test data looking pretty
     data1test.temp = subset(data1test,
       features.n_points==points.levels[i])
     data1test.temp = data1test.temp[
       order(data1test.temp$sources.original_source_id),]
     data1test.temp = subset(data1test.temp,select=imp.variables)
     data1test.temp = as.matrix(data1test.temp)

     ## get the training data looking pretty
     data1train.temp = subset(data1train,
       features.n_points == points.levels[i] &
       !contains.random)
     data1train.temp = data1train.temp[
       order(data1train.temp$sources.original_source_id),]
     ordered.train.ids = data1train.temp$sources.original_source_id
     data1train.temp = subset(data1train.temp,select=imp.variables)
     data1train.temp = as.matrix(data1train.temp)

     ## standardize everything
     ecdf.list = apply(data1train.temp,2,ecdf)
     for(j in 1:length(ecdf.list)){
       data1train.temp[,j] = ecdf.list[[j]](data1train.temp[,j])
       data1test.temp[,j] = ecdf.list[[j]](data1test.temp[,j])
     }

     ## compute the distances for each test observation
     for(j in 1:(dim(neighbors)[1])){
       print(j)
       current.vec = data1test.temp[j,]
       distances = apply(data1train.temp,
         1,function(z) { sum((z - current.vec)^2)})
       neighbors[j,,i] = ordered.train.ids[
                  order(distances) < (nNN + .5)]
     }

   ## report on progress
   print("done with round:")
   print(i)

 }
 return(neighbors)
}

neighbors = WrapLines()


## use the correct features -> TODO: move so that both
## noisification and denoisification use same code
data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id",
  "features.max_slope","features.min",
  "features.linear_trend","features.max",
  "features.weighted_average","features.median")
data1_features = data1_features[!(data1_features %in%
  to_remove)]
rf_formula = formula(paste("sources.classification ~ ",
  paste(data1_features,collapse=" + ")))
rf_formula

## construct a forest on the clean data
data1train.temp = subset(data1train,
  sources.original_source_id == features.source_id)
rf = randomForest(rf_formula,data=data1train.temp)

rf.predictions = predict(rf,newdata=data1train.temp,type='prob')
rownames(rf.predictions) = data1train.temp$features.source_id

classification = function(ids){
  preds = rf.predictions[as.character(ids),]
  most.likely = colnames(preds)[which.max(apply(preds,2,mean))]
  return(most.likely)
}

## get the class predictions based on the nearest neighbors
ans = apply(neighbors,c(1,3),classification)
dim(ans)
head(ans)

## get the truth
data1test.temp = subset(data1test,
  subset=features.n_points == points.levels[1])
truth = data1test.temp[order(
  data1test.temp$sources.original_source_id),
  c("sources.classification")]

## compute and display the error
errors = apply(ans,2,function(x) { mean(x != truth)})
print("the errors are:")
print(errors)

## essentially becomes a NN classifier

##
## we should compare to naive, and 1-noisified RF
##

