####
#### by James Long
#### date April 14, 2011
####
####

# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)

source('Rfunctions.R')
library('randomForest')
library('rpart')

# set the output graphics folder
graphics = graphics_output('figures/')


# get the data
features = '../../data_processed/synthetic_analysis/sources00001.dat'
tfe = '../../data_processed/synthetic_analysis/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)

# basic summary statistics
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
points.levels = unique(data1test$features.n_points[data1test$features.source_id != data1test$sources.original_source_id])
points.levels = points.levels[order(points.levels)]
points.levels

### ALSO IMPORTANT:: Have an assigned order for classes
class.names = names(table(data1$sources.classification))

  
####
#### EXAMINE THE PERIOD
####


# FUNCTION:: Determine if period is correct
isPeriodCorrect = function(true_periods,estimated_periods,
                multiples=c(1,1/2),sensitivity=.05){
  correct = rep(FALSE,length(true_periods))
  for(i in multiples){
    correct = correct | (abs(1 - 
      estimated_periods / (i * true_periods))
      < sensitivity)
  }
  return(correct)
}


# TEST:: of isPeriodCorrect function
# how often is period correct for given number flux
numberFlux = 50
data1temp = subset(data1test,features.n_points == 50)
results = isPeriodCorrect(data1temp$sources.true_period,1/data1temp$features.freq1_harmonics_freq_0,multiples=c(1,1/2),sensitivity=.05)
sum(results)


## true period vs esimated period for each # flux measurements
for(i in 1:length(points.levels)){
  graphic_name = paste("logTrueVsLogEst",points.levels[i],
    "points.pdf",sep="")
  data1temp = subset(data1test,features.n_points == points.levels[i])
  pdf(graphics(graphic_name))
  plot(log(data1temp$sources.true_period),
       log(1/data1temp$features.freq1_harmonics_freq_0),
       col=data1temp$sources.classification,
       xlab="log(True Period)",ylab="log(Estimated Period)")
  abline(0,1,col='grey')
  dev.off()
}

# plot of y = log(est period / true period)
# jitter along x-axis so we can appreciate density
ratios = log(1/(data1test$sources.true_period * data1test$features.freq1_harmonics_freq_0))
pdf(graphics("periodRatioByNumberFlux.pdf"))
plot(c(min(points.levels)-5,max(points.levels)+5),
     c(min(ratios),max(ratios)),col=0,
     xlab="Number of Flux Measurements",
     ylab="log(Estimated Period / True Period)")
for(i in points.levels){
  data1temp = subset(data1test,features.n_points == i)
  colTrue = 1 + 1*isPeriodCorrect(data1temp$sources.true_period,1/data1temp$features.freq1_harmonics_freq_0,multiples=c(1,1/2),sensitivity=.05)
  points(i + rnorm(nrow(data1temp),sd=.5),
         log(1 / (data1temp$features.freq1_harmonics_freq_0
                  * data1temp$sources.true_period)),
         col='#00000020')
}
dev.off()
  

### create correct period as a function of
### number flux for each class
# computations
periodCorrect = matrix(0,
  nrow=length(table(data1test$sources.classification)),
  ncol=length(points.levels))
for(i in 1:nrow(periodCorrect)){
  data1testClass = subset(data1test,
    sources.classification == class.names[i])
  for(j in 1:ncol(periodCorrect)){
    data1temp = subset(data1testClass,
      features.n_points == points.levels[j])
    periodCorrect[i,j] = sum(isPeriodCorrect(
                   data1temp$sources.true_period,
                   1/data1temp$features.freq1_harmonics_freq_0)
                          / nrow(data1temp) )
  }
}
periodWrong = 1 - periodCorrect
classTable = table(data1test$sources.classification[
  data1test$features.n_points==points.levels[1]])
classTable = classTable[class.names]
n = t(sapply(classTable,function(x) { rep(x,ncol(periodWrong))}))
periodWrongSE = computeStandardErrors(periodWrong,n)

# display the results
pdf(graphics('correctPeriodVersusNumberFluxByClass.pdf'))
plotLines(periodWrongSE,points.levels,
          xlab="Number Flux Measurements",
          ylab="Fraction Incorrect Period")
class.names = names(table(data1test$sources.classification))
legend(50, 1,class.names,col=1:length(class.names),
       lwd=2,cex=1,title="Classes")
dev.off()


# for each point in results matrix I have
# 1. a classifier (contains variable importance / tree)
#   - a list of classifiers in the case of 5-point classifier?
# 2. error rate
# 3. predictions on test
# 4. truth for test values




######
###### COMPARISON:: noisification methods
######

# create formula
data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id",
  "features.max_slope","features.min",
  "features.linear_trend","features.max")
data1_features = data1_features[!(data1_features %in%
  to_remove)]
rf_formula = formula(paste("sources.classification ~ ",
  paste(data1_features,collapse=" + ")))
rf_formula
# TODO: make way to print rf_formula w/ 1 arg per line




#### FUNCTION:: switch in and out of
#### using random forest and CART
classifier = function(which_classifier,training,test){
  output = list()
  if(which_classifier == "cart"){
    rp = rpart(rf_formula,data=training)
    output[[1]] = rp
    predictions = predict(rp,newdata=test)
    output[[2]] = predictions
  }
  if(which_classifier == "randomForest"){
    rf = randomForest(rf_formula,data=training)
    output[[1]] = rf
    predictions = predict(rf,newdata=test,type='prob')
    output[[2]] = predictions
  }
  if(which_classifier != "randomForest" & which_classifier != 'cart'){
    print("error:which_classifier must be randomForest or cart")
  }
  return(output)
}

### test of classifier
data1test.temp = subset(data1test,features.n_points==10)
nrow(data1test.temp)
data1train.temp = subset(data1train,features.n_points==10 & contains.random == FALSE & row_id == 0)
nrow(data1train.temp)
output = classifier('randomForest',data1train.temp,data1test.temp)
dim(output[[2]])
class(output[[2]])


classifierOutput = function(data.train,data.test,which.classifier){
  n.classifiers = length(unique(data.train$row_id))
  print("the number of classifiers is:")
  print(n.classifiers)
  class.predictions = array(0,c(n.classifiers,nrow(data.test),
      length(levels(data.train$sources.classification))))
  classifierList = list()
  for(i in 1:n.classifiers){
    data.current = subset(data.train,subset=(row_id == i-1))
    # return a list [classifier,n x p matrix of probability pred]
    classOut = classifier(which.classifier,data.current,data.test)
    classifierList[[i]] = classOut[[1]]
    class.predictions[i,,] = classOut[[2]][,class.names]    
  }
  class.predictions = apply(class.predictions,c(2,3),mean) 
  colnames(class.predictions) = class.names
  print("column names for class.predictions")
  print(colnames(class.predictions))
  max.class = colnames(class.predictions)[apply(class.predictions,
    1,which.max)]
  true.class = data.test$sources.classification
  error = mean(max.class != true.class)
  # classifier is a list of classifiers
  # class.predictions, rows = obs in test, cols = p(class for obs)
  # max.class = name of predicted class of each test obs
  # true.class = the true class for each obs
  # error = mean(max.class != true.class)
  return(list(classifierList,class.predictions,max.class,
              true.class,error))
}


### use classifer output to get everything we want
theresults = array(list(),dim=c(4,length(points.levels)))
which.classifier = 'randomForest'
for(i in 1:length(points.levels)){
  # set the test data -> it's the same for every method
  data1test.temp = subset(data1test,
    features.n_points == points.levels[i])

  # method1: first the naive classifier
  data1train.temp = subset(data1train,
    sources.original_source_id == features.source_id)
  theresults[1,i] = list(classifierOutput(data1train.temp,
                 data1test.temp,which.classifier))
  # method2: the random classifier
  data1train.temp = subset(data1train,
    features.n_points == points.levels[i] & contains.random)
  print("the number of randoms is:")
  print(nrow(data1train.temp))
  theresults[2,i] = list(classifierOutput(data1train.temp,
                 data1test.temp,which.classifier))
  # method3: the 1-point classifier
  data1train.temp = subset(data1train,
    features.n_points == points.levels[i] & row_id == 0)
  print("the number of 1-pointers is:")
  print(nrow(data1train.temp))
  theresults[3,i] = list(classifierOutput(data1train.temp,
                 data1test.temp,which.classifier))
  # method4: the 5-point classifier
  data1train.temp = subset(data1train,
    features.n_points == points.levels[i])
  print("the number of 5-pointers is:")
  print(nrow(data1train.temp))
  theresults[4,i] = list(classifierOutput(data1train.temp,
                 data1test.temp,which.classifier))
}



length(theresults[1,1][[1]][[1]])
theresults[1,1][[1]][[5]]



length(theresults[1,2][[1]][[1]])
theresults[2,1][[1]][[5]]






plot(rpart.naive,margin=.2)
text(rpart.naive,use.n=TRUE)

pdf('10_ordered_flux.pdf')
plot(trees[[1]],margin=.2,uniform=TRUE,main="10 Ordered Flux Measurements")
text(trees[[1]],use.n=TRUE)
dev.off()

pdf('random_tree_10_points.pdf')
plot(random.trees[[1]],margin=.2,uniform=TRUE,main='Tree Constructed on 10 Randomly Selected Flux Measurements')
text(random.trees[[1]],use.n=TRUE)
dev.off()

########
######## analysis of classifiers
########

###
### produce graphic for error rates
###






data1$sources.noisification[1:10]

no.noise = data1$sources.noisification == "identity"
noise = data1$sources.noisification == "cadence_noisify"
mean((data1$sources.true_period[no.noise] - (1 / data1$features.freq1_harmonics_freq_0[no.noise]))^2)

mean((data1$sources.true_period[noise] - (1 / data1$features.freq1_harmonics_freq_0[noise]))^2)



no.noise = subset(data1,subset=(sources.noisification=="identity"),select=c("sources.true_period","sources.original_source_id","features.freq1_harmonics_freq_0"))
noise = subset(data1,subset=(sources.noisification=="cadence_noisify"),select=c("sources.original_source_id","features.freq1_harmonics_freq_0"))
names.no.noise = names(no.noise)
names.no.noise[length(names.no.noise)] = "noise_free_freq"
names(no.noise) = names.no.noise

noise.comparison = merge(no.noise,noise)
noise.comparison$noise_free_freq = 1 / noise.comparison$noise_free_freq
noise.comparison$features.freq1_harmonics_freq_0 = 1 / noise.comparison$features.freq1_harmonics_freq_0

plot(noise.comparison$sources.true_period,noise.comparison$features.freq1_harmonics_freq_0,ylim=c(0,10))
abline(0,3,col='grey')
abline(0,2,col='grey')
abline(0,1,col='grey')
abline(0,1/2,col='grey')
abline(0,1/3,col='grey')


plot(noise.comparison$sources.true_period,noise.comparison$noise_free_freq)
abline(0,3,col='grey')
abline(0,2,col='grey')
abline(0,1,col='grey')
abline(0,1/2,col='grey')
abline(0,1/3,col='grey')


# plot period versus true period
plot(


##### to write for this file
# 1. simple classifier
# 2. guess of true period as a function of number of points
#    - maybe # wrong is a better measure
#    - or # not at some harmonic
# 3. visualize tfe's with true_period and guessed
# 4. classifier performance as a function of number points

# to deliver:
# 1. error rate as a function of points for:
# - naive
# - noisification 1 x
# - noisification 2 x
# - noisification 5 x
# - random noisification (random subset of points)
#   this is the natural extreme of non-matching
# 2. a few confusion matrices
# 3. correct period as a function of # of points (relate w/ 1.)
#        true period vs. incorrect period on a log scale
# video showing how classes separate over time
# 4. some work on matching cadences
# - in N-W a good idea, other smoothers
# 5. lots of images of curves so we can discuss parameters
# 6. as we approach using full curves we expect the gap in prediction between noisification 1x and noisification 5x to shrink




###
### examine folded curves - need to make this very general
###

fold_curve = function(times,freq){
	number_periods = times * freq
	folded = number_periods - floor(number_periods)
	return(folded)
}

obs_vec = (1:nrow(data1))[data1$features.source_id == data1$sources.original_source_id]
par(mfcol=c(2,1),ask=TRUE)
for(i in obs_vec){
  relevant_curves = subset(time_flux,subset=(source_id==data1$sources.original_source_id[i]))[,2:3]
  first_30 = 1*(rank(relevant_curves[,1]) < 30) + 1
  plot(relevant_curves[,1],relevant_curves[,2],main=data1[i,"sources.classification"],ylab='Flux',col=first_30)
  folded_times = fold_curve(relevant_curves[,1],data1[i,"features.freq1_harmonics_freq_0"])
  plot(folded_times,relevant_curves[,2],ylab='Flux',col=first_30)
}


