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



##
## divide into test and training
##
data1test = subset(data1,subset=(sources.survey=="test" &
  sources.noisification == 'cadence_noisify'))
data1train = subset(data1,subset=sources.survey=="train")
contains.random = grepl("random",data1train$sources.noise_args)
data1train$contains.random = contains.random


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
points.levels = unique(data1test$features.n_points)
points.levels = points.levels[order(points.levels)]
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
points.levels = unique(data1test$features.n_points)
points.levels = points.levels[order(points.levels)]
periodCorrect = matrix(0,nrow=length(table(data1test$sources.classification)),ncol=length(unique(data1test$features.n_points)))
for(i in 1:nrow(periodCorrect)){
  data1testClass = subset(data1test,
    sources.classification == names(table(sources.classification))[i])
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
###### comparison of noisification methods
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



# NAIVE
data1train.naive = subset(data1train,subset=(sources.noisification
  == 'identity'))
print(nrow(data1train.naive))
rpart.naive = rpart(rf_formula,data=data1train.naive)
for(i in 1:ncol(results)){
  n.points.iter = points.levels[i]
  data1test.sub = subset(data1test,subset=features.n_points==
    n.points.iter)
  #print(nrow(data1test.sub))
  #print(head(data1test.sub))
  predictions = predict(rpart.naive,newdata=data1test.sub,
    type='class')
  results[1,i] = mean(predictions !=
           data1test.sub$sources.classification)
}
results

# RANDOM
data1train.random = subset(data1train,subset=contains.random)
random.trees = list()
for(i in 1:ncol(results)){
  n.points.iter = points.levels[i]
  data1train.random.current = subset(data1train.random,
    subset=features.n_points==n.points.iter)
  rpart.random = rpart(rf_formula,data=data1train.random.current)
  random.trees[[i]] = rpart.random
  data1test.sub = subset(data1test,
    subset=features.n_points==n.points.iter)
  predictions = predict(rpart.random,newdata=data1test.sub,
    type='class')
  results[2,i] = mean(predictions !=
           data1test.sub$sources.classification)
}

results


#
# now for 1 points and 4 point classifiers
# - have to label curves
data1train.first = subset(data1train,subset=(!contains.random
  & sources.noisification != 'identity'))
data1train.first = dedupe(data1train.first,
  c("features.n_points","sources.original_source_id"))
length(unique(data1train.first$row_id))
table(data1train.first$row_id)


#### write this so I can switch in and out of
#### using random forest and CART
classifier = function(which_classifier,training,test){
  if(which_classifier == "cart"){

  }
  if(which_classifier == "randomForest"){

  }
  return(predictions)
}


classifierOutput = function(data.train,data.test,which.classifier){
  n.classifiers = length(unique(data.train$row_id))
  class.predictions = array(0,c(n.classifiers,nrow(data.test),
      length(levels(data.train$sources.classification))))
  classifierList = list()
  for(i in 1:n.classifiers){
    data.current = subset(data.train,subset=(row_id == i-1))
    # return a list [classifier,n x p matrix of probability pred]
    classOut = classifier(which.classifier,data.current,data.test)
    classifierList = classOut[[1]]
    class.predictions[i,,] = classOut[[2]]    
  }
  class.predictions = apply(class.predictions,c(2,3),mean) 
  max.class = colnames(predictions)[apply(class.predictions,
    1,which.max)]
  true.class = data.test$sources.classification
  error = mean(max.class != true.class)
  # classifier is a list of classifiers
  # class.predictions, rows = obs in test, cols = p(class for obs)
  # max.class = name of predicted class of each test obs
  # true.class = the true class for each obs
  # error = mean(max.class != true.class)
  return(list(classifier,class.predictions,max.class,
              true.class,error))
}


# 1 and 4 point classification
trees = list()
for(i in 1:ncol(results)){
  n.points.iter = points.levels[i]
  train.current = subset(data1train.first,
    subset=(features.n_points==n.points.iter))
  test.current = subset(data1test,
    subset=(features.n_points==n.points.iter))
  print(nrow(train.current))
  print(nrow(test.current))
  info1 = NPointClassifier(train.current,test.current,1)
  trees[[i]] = info1[[1]][[1]]
  info2 = NPointClassifier(train.current,test.current,5)
  results[3,i] = info1[[4]]
  results[4,i] = info2[[4]]
}

results



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

stop
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


