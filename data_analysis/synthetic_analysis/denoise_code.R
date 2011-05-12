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






####
#### denoisification by RF regression
####

imp.variables = c("features.p2p_scatter_over_mad","features.small_kurtosis","features.p2p_scatter_2praw","features.qso_log_chi2nuNULL_chi2nu","features.percent_difference_flux_percentile","features.qso_log_chi2_qsonu","features.flux_percentile_ratio_mid20","features.flux_percentile_ratio_mid35","features.flux_percentile_ratio_mid80","features.flux_percentile_ratio_mid50","features.skew","features.fold2P_slope_10percentile","features.amplitude","features.p2p_scatter_pfold_over_mad","features.stetson_j","features.stetson_k","features.medperc90_2p_p","features.freq1_harmonics_freq_0","features.std","features.freq1_harmonics_rel_phase_1","features.scatter_res_raw","features.freq_signif")



GetFormula = function(){
  data1_features = names(data1)[grep("features.*",names(data1))]
  to_remove = c("features.n_points","features.source_id",
    "features.max_slope","features.min",
    "features.linear_trend","features.max",
    "features.weighted_average","features.median")
  data1_features = data1_features[!(data1_features %in%
    to_remove)]
  rf_formula = formula(paste("sources.classification ~ ",
    paste(data1_features,collapse=" + ")))
  return(rf_formula)
}
  


##
## predict noisy version of clean curves, specifically
## predict each of the features in y.vars, using the
## features x.vars
##
PredictResponses = function(x.temp,y.temp,x.vars,y.vars){
  rfs = list()
  for(i in 1:length(y.vars)){
    print(sprintf("run %s / %s",i,length(y.vars)))
    rfs[[i]] = randomForest(x.temp[,x.vars],
         y.temp[,y.vars[i]],
         nodesize=1)
  }
  y.hat = matrix(0,nrow=nrow(x.temp),ncol=length(y.vars))
  y.sds = matrix(0,nrow=nrow(x.temp),ncol=length(y.vars))
  for(i in 1:length(y.vars)){
    y.hat[,i] = predict(rfs[[i]])
    preds = predict(rfs[[i]],newdata=x.temp,predict.all=TRUE)[[2]]
    y.sds[,i] = apply(preds,1,sd) / .33
  }
  return(list(rfs,y.hat,y.sds))
}



## y = (# test) x (# features) from test
## y.hat = (# train) x (#features) from train
## output (# train) x (# test) x (# features)
##    where differences are taken between every
##    pair of training and test obs
YminusYhat = function(y,y.hat){
  result = array(0,dim=c(nrow(y.hat),nrow(y),ncol(y.hat)))
  for(i in 1:(dim(result)[2])){
    v.y = y[i,]
    result[,i,] = t(apply(y.hat,1,function(x){abs(x - v.y)}))
  }
  return(result)
}

## TEST: YminusYhat - seems to work
a = matrix(c(1,2,3,4),ncol=2)
a
b = matrix(c(10,1,15,8,0,12),ncol=2)
b
c = YminusYhat(a,b)
dim(c)
c[,1,]
c[,2,]


## divide all the differences in big.raw by
## their standard deviations
StandardizeBigRaw = function(big.raw,sds){
  big.norm = array(0,dim=dim(big.raw))
  if(!is.matrix(sds)){
    print('sds must be a matrix')
    return(0)
  }
  ## if a single standard deviation for all obs, just repeat it
  if(dim(sds)[1] == 1){
    sds = matrix(rep(sds,dim(big.raw)[1]),
      nrow=dim(big.raw)[1],byrow=TRUE)
  }
  ## make sure sds conforms to dimensions of big.raw
  if(!identical(dim(sds),dim(big.raw)[c(1,3)])){
    print("sds wrong dimensions")
    stop
  }
  for(i in 1:(dim(big.raw)[2])){
    big.norm[,i,] = big.raw[,i,] / sds
  }
  return(big.norm)
}

## TEST: StandardizeBigRaw - seems to work
c[,1,]
c[,2,]
sds = array(c(.5,1),dim=c(1,2))
sds
dim(sds)
c.norm = StandardizeBigRaw(c,sds)
dim(c)
dim(c.norm)
c.norm[,1,]
c[,1,]
c.norm[,2,]
c[,2,]

##
## tie all the function together
##

## get the correctly noisified training data
Denoise = function(data1train.clean,
                   data1train.temp,
                   data1test.temp,
                   imp.variables){

  ## are we good?
  if(!identical(data1train.temp$sources.original_source_id,
            data1train.clean$sources.original_source_id)){
    print("clean training and temp training don't match")
    return(0)
  }

  ## convert data frames to matrices so things work fast
  data1train.clean.matrix = as.matrix(data1train.clean[,imp.variables])
  data1train.temp.matrix = as.matrix(data1train.temp[,imp.variables])
  data1test.temp.matrix = as.matrix(data1test.temp[,imp.variables])


  ## BEGIN: generate pyx
  
  ## do all the randomForest regressions
  stuff = PredictResponses(data1train.clean.matrix,
    data1train.temp.matrix,imp.variables,imp.variables)

  ## get all the residuals
  big.raw = YminusYhat(data1test.temp.matrix,stuff[[2]])

  ## get standard errors and normalize big.raw
  sds = matrix(colMeans(stuff[[3]]),nrow=1)
  big.norm = StandardizeBigRaw(big.raw,sds)

  ## take the product across all the features i.e.
  ## p(y_1|x) x . . . x p(y_m|x) = p(y|x)
  ## should be (#train) x (# test)
  ## 1 column for each test
  pyx = apply(big.norm,c(1,2),function(x){prod(dnorm(x))})

  ## END: Generate pyx
  
  ## construct rf on clean data and get probabilities
  rf_formula = GetFormula()
  rf.clean = randomForest(rf_formula,data=data1train.clean)
  pzx = predict(rf.clean,type='vote')
  class.names = colnames(pzx)

  ## change pyx and pzx into predictions
  predictions = class.names[apply(pyx,2,function(x)
    {which.max(apply(pzx,2,function(y){sum(x*y)}))})]
  result = mean(predictions !=
           data1test.temp$sources.classification)
  return(result)
}


###
### run Denoise function
###

## get the clean training set
data1train.clean = subset(data1,
  features.source_id == sources.original_source_id
  & sources.survey == 'train')
length(unique(data1train.clean$sources.original_source_id))
nrow(data1train.clean)
data1train.clean = data1train.clean[
  order(data1train.clean$sources.original_source_id),]
nrow(data1train.clean)
data1train.clean$sources.original_source_id[1:10]

## store the results for each run of denoise
total.results = rep(0,length(points.levels))

## run denoise a bunch of times, need to reselect
## noisified training and test sets each time
for(i in 1:length(points.levels)){
  ## get the correct training data
  data1train.temp = subset(data1train,
    features.n_points == points.levels[i] &
    row_id==0 & !contains.random)
  data1train.temp = data1train.temp[
    order(data1train.temp$sources.original_source_id),]

  ## get the correct test set
  data1test.temp = subset(data1,
    sources.survey=='test' & features.n_points == points.levels[i])

  ## report on this run
  print("==================")
  print(sprintf("run %s / %s",i,length(points.levels)))
  print("==================")


  ## get all the results
  total.results[[i]] = Denoise(data1train.clean,data1train.temp,
                 data1test.temp,imp.variables)
}



  

###
### what sort of things should I analyze in this algorithm ?
### 
### 1. what features are most important for predicting
###    which features?
###     - what about trying just real feature to predict noisy
###     - might be possible to get really good estimates of
###        standard error that way
### 2. how realistic are my estimates of SE?
### 3. stability to using different sets of features for p(y|x)?
### 4. what are sizes of p(y|x_i)'s, is a single x_i dominating?
### 5. do individual errors follow a normal dist?
###     i.e. predicting y_feat1 using x_1, . . . ,x_n,
###    


















imp.variables = c("features.p2p_scatter_over_mad","features.small_kurtosis","features.p2p_scatter_2praw","features.qso_log_chi2nuNULL_chi2nu","features.percent_difference_flux_percentile","features.qso_log_chi2_qsonu","features.flux_percentile_ratio_mid20","features.flux_percentile_ratio_mid35","features.flux_percentile_ratio_mid80","features.flux_percentile_ratio_mid50","features.skew","features.fold2P_slope_10percentile","features.amplitude","features.p2p_scatter_pfold_over_mad","features.stetson_j","features.stetson_k","features.medperc90_2p_p","features.freq1_harmonics_freq_0","features.std","features.freq1_harmonics_rel_phase_1","features.scatter_res_raw","features.freq_signif")
imp.variables = imp.variables[1:5]
for(i in 1:length(imp.variables)){
  print(imp.variables[i])
  print(length(unique(data1train.temp[,imp.variables[i]])))
}
































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

