####
#### by James Long
#### date May 12, 2011
####
#### used for studying denoisification
####


library('randomForest')
library('rpart')
library('xtable')

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

imp.variables = c("features.p2p_scatter_over_mad",
  "features.small_kurtosis",
  "features.p2p_scatter_2praw",
  "features.qso_log_chi2nuNULL_chi2nu",
  "features.percent_difference_flux_percentile",
  "features.qso_log_chi2_qsonu",
  "features.flux_percentile_ratio_mid20",
  "features.flux_percentile_ratio_mid35",
  "features.flux_percentile_ratio_mid80",
  "features.flux_percentile_ratio_mid50",
  "features.skew",
  "features.fold2P_slope_10percentile",
  "features.amplitude",
  "features.p2p_scatter_pfold_over_mad",
  "features.stetson_j",
  "features.stetson_k",
  "features.medperc90_2p_p",
  "features.freq1_harmonics_freq_0",
  "features.std",
  "features.freq1_harmonics_rel_phase_1",
  "features.scatter_res_raw",
  "features.freq_signif")

## actually want to use all features in denoisification process
data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id",
  "features.max_slope","features.min",
  "features.linear_trend","features.max",
  "features.weighted_average","features.median")
imp.variables = data1_features[!(data1_features %in%
  to_remove)]


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






###
### run Denoisification function
###

## get the clean training set
data1train.clean = subset(data1,
  features.source_id == sources.original_source_id
  & sources.survey == 'train')
data1train.clean = data1train.clean[
  order(data1train.clean$sources.original_source_id),]
train.clean.m = as.matrix(data1train.clean[,imp.variables])

## store the results for each run of denoise
total.results = rep(0,length(points.levels))

## run denoise a bunch of times, need to reselect
## noisified training and test sets each time
for(i in 1:length(points.levels)){
  ## report on this run
  print("==================")
  print(sprintf("run %s / %s",i,length(points.levels)))
  print("==================")

  ## get the right data
  data.obj = PrepareDataForDenoise(i,imp.variables)
  data.obj$train.clean.m = train.clean.m
  data.obj$x.vars = imp.variables
  data.obj$y.vars = imp.variables
  
  
  ## get all the results
  error = Denoisification(data1train.clean,
                 data.obj$test.class,
                 data.obj,
                 DenoiseRF)
  total.results[i] = error$error
}


## save current results
save.image(file=RData('denoise_code.RData'))

## get results from noisification
load('RData/randomForestNoisificationResults.RData')

total.results = matrix(total.results,nrow=1)
total.results
total.results.sd = computeStandardErrors(total.results,500,sderror=1)
errorsSD[2,,] = total.results.sd[1,,]
data.to.plot = errorsSD[c(1,2,4),,]
  
pdf(graphics('denoiseNoiseComp.pdf'))
plotLines(data.to.plot,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,maintitle="Random Forests")
legend(60, .5,c("Naive","RF Denoisification","5 x Noisification RF"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()



##
## Study 20 flux noise case in detail
##

data.obj = PrepareDataForDenoise(2,imp.variables)
data.obj$train.clean.m = train.clean.m
data.obj$x.vars = imp.variables
data.obj$y.vars = imp.variables
  
  
everything.20 = Denoisification(data1train.clean,
  data.obj$test.class,
  data.obj,
  DenoiseRF,
  return.all=TRUE)

save.image(file=RData('denoise_code.RData'))

class(everything.20)
summary(everything.20)
summary(everything.20$denoise.results)
summary(everything.20$denoise.results$muyx)
summary(everything.20$denoise.results$muyx$rfs)

## a few of these are interesting
## it appears that 50 median diff
## is very good at predicting
## other median diff
rfs = everything.20$denoise.results$muyx$rfs

for(i in 1:length(imp.variables)){
  pdf(graphics(paste("denoiseVarImp",i,".pdf",sep="")))
  varImpPlot(rfs[[i]],main=imp.variables[i])
  dev.off()
}

summary(everything.20)

Ratio = function(x,quant=.98){
  f1 = ecdf(x)
  top = x[f1(x) > quant]
  ratio = min(top) / max(top)
  return(ratio)
}


ds = apply(everything.20$denoise.results$muyx$y.res,
  2,function(x){Ratio(x,.997)})
ds.density = density(ds)
pdf(graphics('pxyRatiosDensityTop2.pdf'))
plot(ds.density,main='',xlab='Ratio largest (across i) p(y|x_i) to 2nd largest (across i) p(y|x_i)')
dev.off()

ds = apply(everything.20$denoise.results$muyx$y.res,
  2,function(x){Ratio(x,.99)})
ds.density = density(ds)
pdf(graphics('pxyRatiosDensityTop5.pdf'))
plot(ds.density,main='',xlab='Ratio largest (across i) p(y|x_i) to 5th largest (across i) p(y|x_i)')
dev.off()

ds = apply(everything.20$denoise.results$muyx$y.res,
  2,function(x){Ratio(x,.9)})
ds.density = density(ds)
pdf(graphics('pxyRatiosDensityTop50.pdf'))
plot(ds.density,main='',xlab='Ratio largest (across i) p(y|x_i) to 50th largest (across i) p(y|x_i)')
dev.off()



summary(everything.20)
summary(everything.20$denoise.results$muyx$y.hat)




for(i in 1:length(everything.20[[2]])){
  d1 = density(everything.20$denoise.results$muyx$y.res[,i])
  pdf(graphics(paste("residuals",i,".pdf",sep="")))
  plot(d1,lwd=2,main=paste(imp.variables[i],"Residuals"),sub="Dotted Line is Normal Fit")
  curve(dnorm(x,mean=0,sd=sd(everything.20$denoise.results$muyx$y.res[,i])),col="red",add=T,lwd=2,lty=2)
  dev.off()
}




save.image(file=RData('denoise_code.RData'))
