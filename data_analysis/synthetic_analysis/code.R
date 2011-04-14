####
#### by James Long
#### date April 1, 2011
####
####

#
# to do:
# 1. to the 1-point classifier into a general n-point classifier
# 2. write some testing for Rfunctions.R file
#


# clear out history
rm(list=ls(all=TRUE))

source('Rfunctions.R')
library('randomForest')
library('rpart')

# get the features
data1 = read.table('sources00001.dat',sep=';',header=TRUE)
# get the tfe
time_flux = read.table('tfe00001.dat',sep=';',header=TRUE)


names(data1)
head(data1)
sum(is.na(data1))

names(time_flux)
head(time_flux)
sum(is.na(time_flux))


data1 = na.roughfix(data1)


rpart.naive = randomForest(rf_formula,data=data1train.naive)






#### write this so I can switch in and out of
#### using random forest and CART
classifier = function(which_classifier,training,test){
  if(which_classifier == "cart"){

  }
  if(which_classifier == "randomForest"){

  }
  return(predictions)
}


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

# find out what points we are using
points.levels = unique(data1$features.n_points)
points.levels = points.levels[order(points.levels)][1:
  (length(points.levels) - 1)]

results = matrix(0,nrow=4,ncol=length(points.levels))
data1test = subset(data1,subset=(sources.survey=="test" &
  sources.noisification == 'cadence_noisify'))
data1train = subset(data1,subset=sources.survey=="train")
contains.random = grepl("random",data1train$sources.noise_args)
data1train$contains.random = contains.random

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

NPointClassifier = function(data.train,data.test,n.classifiers){
  class.predictions = array(0,c(n.classifiers,nrow(data.test),
      length(levels(data.train$sources.classification))))
  trees = list()
  for(i in 1:n.classifiers){
    data.current = subset(data.train,subset=(row_id == i-1))
    rpart.current = rpart(rf_formula,data=data.current)
    trees[[i]] = rpart.current
    predictions = predict(rpart.current,
      newdata=data.test,type='prob')
    class.predictions[i,,] = predictions
  }
  class.predictions = apply(class.predictions,c(2,3),mean) 
  max.class = colnames(predictions)[apply(class.predictions,
    1,which.max)]
  error = mean(max.class != data.test$sources.classification)
  return(list(trees,class.predictions,max.class,error))
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

pdf('error_rates.pdf')
plot(points.levels, results[1,], ylim=c(0,max(results)),type="l", xlab="Number Flux Measurements", ylab="Error",main="Error Rates",lwd=2,lty=1)
for(i in 2:nrow(results)){
  print(i)
  points(points.levels,results[i,],type='l',col=i,lwd=2,lty=i)
}
legend(40, .5,c("No Adjustment","Random Selection","Noisified","Noisified 5x"),col=1:4,lty=c(1,2,3,4),lwd=2,cex=1.5,title="Classifiers")
dev.off()


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


