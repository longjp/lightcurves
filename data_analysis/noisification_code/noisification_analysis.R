########## 
########## 
########## CODE FOR ANALYSIS OF NOISIFIED DATA 
########## 
##########
########## by James Long 
########## date: APRIL 22, 2011 
########## 

#### TODO's:
#### 1. check to see if graphics, tables, and RData have been created
####    if not warn user and then dump everything in pwd


## basic summary statistics
names(data1)
head(data1)
sum(is.na(data1))

names(time_flux)
head(time_flux)
sum(is.na(time_flux))

print("number of values imputed in data1:")
print(sum(is.na(data1)))


data1 = na.roughfix(data1) ## this isn't fair, uses better data
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


## TEST:: of isPeriodCorrect function
## how often is period correct for given number flux
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
       pch=(1:length(unique(data1temp$sources.classification)
                     ))[data1temp$sources.classification],
       xlab="log(True Period)",ylab="log(Estimated Period)")
  abline(0,1,col='grey')
  dev.off()
}

## plot of y = log(est period / true period)
## jitter along x-axis so we can appreciate density
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

## display the results
pdf(graphics('correctPeriodVersusNumberFluxByClass.pdf'))
plotLines(periodWrongSE,points.levels,
          xlab="Number Flux Measurements",
          ylab="Fraction Incorrect Period")
class.names = names(table(data1test$sources.classification))
legend(50, 1,class.names,col=1:length(class.names),
       lwd=2,cex=1,title="Classes",pch=1:length(class.names))
dev.off()


## for each point in results matrix I have
## 1. a classifier (contains variable importance / tree)
##   - a list of classifiers in the case of 5-point classifier?
## 2. error rate
## 3. predictions on test
## 4. truth for test values




######
###### COMPARISON:: noisification methods
######

## create formula
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
## TODO: make way to print rf_formula w/ 1 arg per line




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
    stop
    return(NULL)
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
    ## return a list [classifier,n x p matrix of probability pred]
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
  ## classifier is a list of classifiers
  ## class.predictions, rows = obs in test, cols = p(class for obs)
  ## max.class = name of predicted class of each test obs
  ## true.class = the true class for each obs
  ## error = mean(max.class != true.class)
  return(list(classifierList,class.predictions,max.class,
              true.class,error))
}




### use classifer output to get everything we want

computeResults = function(which.classifier){
  theresults = array(list(),dim=c(4,length(points.levels)))
  for(i in 1:length(points.levels)){
    ## set the test data -> it's the same for every method
    data1test.temp = subset(data1test,
      features.n_points == points.levels[i])
    print("the size of the test set is:")
    print(nrow(data1test.temp))
  
    ## method1: first the naive classifier
    data1train.temp = subset(data1train,
      sources.original_source_id == features.source_id)
    print("the number of naives is:")
    print(nrow(data1train.temp))
    theresults[1,i] = list(classifierOutput(data1train.temp,
                data1test.temp,which.classifier))
    ## method2: the random classifier
    data1train.temp = subset(data1train,
      features.n_points == points.levels[i] & contains.random)
    print("the number of randoms is:")
    print(nrow(data1train.temp))
    theresults[2,i] = list(classifierOutput(data1train.temp,
                data1test.temp,which.classifier))
    ## method3: the 1-point classifier
    data1train.temp = subset(data1train,
      features.n_points == points.levels[i] & row_id == 0 &
      !contains.random)
    print("the number of 1-pointers is:")
    print(nrow(data1train.temp))
    theresults[3,i] = list(classifierOutput(data1train.temp,
                data1test.temp,which.classifier))
    ## method4: the 5-point classifier
    data1train.temp = subset(data1train,
      features.n_points == points.levels[i] & !contains.random)
    print("the number of 5-pointers is:")
    print(nrow(data1train.temp))
    theresults[4,i] = list(classifierOutput(data1train.temp,
                data1test.temp,which.classifier))
  }
  return(theresults)
}


cartResults = computeResults('cart')
rfResults = computeResults('randomForest')


## print plot with cart errors
errors = apply(cartResults,c(1,2),function(x){x[[1]][[5]]})
n = matrix(500,nrow=4,ncol=length(points.levels))
errorsSD = computeStandardErrors(errors,n)

pdf(graphics('cartNoisificationComparison.pdf'))
plotLines(errorsSD,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,maintitle="CART")
legend(50, .5,c("Naive","Random","1 x Noise","5 x Noise"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()

## save cart results
readme = "errorsSD contains results from cart run using naive, 1x noise, 5x noise, and random noisifications. its 3rd dim contains standard errors. points.levels is the x-axis associated with this vector e.g.
pdf(graphics('cartNoisificationComparison.pdf'))
plotLines(errorsSD,points.levels,xlab='Number of Flux Measurements',ylab='Error',ymin=0,maintitle='CART')
legend(50, .5,c('Naive','Random','1 x Noise','5 x Noise'),col=1:length(class.names),lwd=2,cex=1,title='Classifiers',pch=1:length(class.names))
dev.off()"
save(readme,errorsSD,points.levels,
     class.names,file=RData('cartNoisificationResults.RData'))

## print plot with rf errors
errors = apply(rfResults,c(1,2),function(x){x[[1]][[5]]})
n = matrix(500,nrow=4,ncol=length(points.levels))
errorsSD = computeStandardErrors(errors,n)

pdf(graphics('rfNoisificationComparison.pdf'))
plotLines(errorsSD,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,maintitle="Random Forests")
legend(50, .5,c("Naive","Random","1 x Noise","5 x Noise"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()

## save random forest results
readme = "errorsSD contains results from randomForest run using naive, 1x noise, 5x noise, and random noisifications. its 3rd dim contains standard errors. points.levels is the x-axis associated with this vector e.g.
pdf(graphics('rfNoisificationComparison.pdf'))
plotLines(errorsSD,points.levels,xlab='Number of Flux Measurements',ylab='Error',ymin=0,maintitle='Random Forests')
legend(50, .5,c('Naive','Random','1 x Noise','5 x Noise'),col=1:length(class.names),lwd=2,cex=1,title='Classifiers',pch=1:length(class.names))
dev.off()"
save(readme,errorsSD,points.levels,
     class.names,file=RData('randomForestNoisificationResults.RData'))


########
######## analysis of classifiers
########

###
### print several trees
###
pdf(graphics('cartNaive.pdf'),width=10,height=5)
plotTree(cartResults[1,1][[1]][[1]][[1]],maintitle="Naive CART Tree")
dev.off()

pdf(graphics('cart10Contiguous.pdf'),width=10,height=5)
plotTree(cartResults[3,1][[1]][[1]][[1]],maintitle="10-Flux Contiguous CART Tree")
dev.off()

pdf(graphics('cart50Contiguous.pdf'),width=10,height=5)
plotTree(cartResults[3,5][[1]][[1]][[1]],maintitle="50-Flux Contiguous Tree")
dev.off()

pdf(graphics('cart100Contiguous.pdf'),width=10,height=5)
plotTree(cartResults[3,10][[1]][[1]][[1]],maintitle="100-Flux Contiguous Tree")
dev.off()



#### Examine error rate for 1 x Noisification RF along with
#### accuracy of period estimation, this code is very 
#### sensitive

periodWrongAve = apply(periodWrong,2,mean)
periodWrongAve = matrix(periodWrongAve,nrow=1,
  ncol=prod(length(periodWrongAve)))
n = matrix(500,ncol=prod(dim(periodWrongAve)),nrow=1)
periodWrongAveSE = computeStandardErrors(periodWrongAve,n)
errors = apply(rfResults,c(1,2),function(x){x[[1]][[5]]})
n = matrix(500,nrow=4,ncol=length(points.levels))
errorsSD = computeStandardErrors(errors,n)
toPlot = array(0,dim=c(3,dim(errorsSD)[2],3))
toPlot[1,,] = periodWrongAveSE[1,,]
toPlot[2,,] = errorsSD[1,,]
toPlot[3,,] = errorsSD[3,,]

pdf(graphics('periodClassifierError.pdf'))
plotLines(toPlot,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,maintitle="Classifier Accuracy and Period Accuracy")
legend(50, .75,c("Period","Naive RF","1x Noisification RF"),col=1:dim(toPlot)[1],lwd=2,cex=1,pch=1:dim(toPlot)[1])
dev.off()



##
## apply 10-point, 50-point, and naive across all data
##

## TODO
## should check this to make sure this agrees with
## the simulation I end up doing
robustCheck = list()
robustCheck[[1]] = rfResults[1,1][[1]][[1]][[1]]
robustCheck[[2]] = rfResults[3,1][[1]][[1]][[1]]
robustCheck[[3]] = rfResults[3,5][[1]][[1]][[1]]
robustCheck[[4]] = rfResults[3,10][[1]][[1]][[1]]
robustError = matrix(0,nrow=4,ncol=length(points.levels))
for(i in 1:ncol(robustError)){
  data1temp = subset(data1test,features.n_points == points.levels[i])
  print(nrow(data1temp))
  for(j in 1:nrow(robustError)){
    predictions = predict(robustCheck[[j]],newdata=data1temp)
    robustError[j,i] = mean(
                 predictions != data1temp$sources.classification)
  }
}


n = matrix(500,nrow=4,ncol=length(points.levels))
errorsSD = computeStandardErrors(robustError,n)

pdf(graphics('robustError.pdf'))
plotLines(errorsSD,points.levels,xlab="Number of Flux Measurements",ylab="Error",ymin=0,maintitle="Are Noisified Classifiers Robust?")
legend(50, .5,c("Naive","10-Point Noisification","50-Point Noisification","100-Point Noisification"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()

## save random forest results
readme = "errorsSD contains results from examining how robust randomForest noisification is. naive, 50 pt noisification, 10 pt, and 100 pt are tried across all values of points.levels. its 3rd dim contains standard errors. points.levels is the x-axis associated with this vector e.g.
pdf(graphics('rfNoisificationComparison.pdf'))
plotLines(errorsSD,points.levels,xlab='Number of Flux Measurements',ylab='Error',ymin=0,maintitle='Random Forests')
legend(50, .5,c('Naive','Random','1 x Noise','5 x Noise'),col=1:length(class.names),lwd=2,cex=1,title='Classifiers',pch=1:length(class.names))
dev.off()"
save(readme,errorsSD,points.levels,
     class.names,file=RData('robustnessNoisificationResults.RData'))




### how do these three classifiers perform on
### well sampled curves
data1temp = subset(data1test,!(features.n_points %in% points.levels))
errorOnClean = rep(0,length(robustCheck))
names(errorOnClean) = c("Naive","10-Point","50-Point","100-Point")
for(i in 1:length(robustCheck)){
  predictions = predict(robustCheck[[i]],newdata=data1temp)
  errorOnClean[i] = mean(predictions
                != data1temp$sources.classification)
}

## add standard errors to errorOnClean
ses = 2*sqrt(errorOnClean * ( 1 - errorOnClean) / length(predictions))
CI = paste("(",round(errorOnClean - ses,2),',',round(errorOnClean + ses,2),")",sep="")
errorOnCleanDF = as.data.frame(errorOnClean)
names(errorOnCleanDF) = "error"
errorOnCleanDF$error = round(errorOnCleanDF$error,2)
errorOnCleanDF$CI = CI
errorOnCleanDF



## print table in nice form
outputX = xtable(errorOnCleanDF,digits=2,caption="Error on Noise Free Test") 
print(outputX,type='latex',file=tables('errorOnWellSamples.txt'),table.placement="H",include.rownames=TRUE,append=FALSE)




num.train = length(unique(
  data1train$sources.original_source_id))
num.test = length(
  unique(data1test$sources.original_source_id))
to.plot = min(num.test,num.train)

for(i in 1:length(points.levels)){

  ## plot the noisy test, clean train
  pdf(graphics(paste('freqTrue',
                     points.levels[i],'.pdf',sep="")))
  d1 = density(data1test$features.freq1_harmonics_freq_0[
    data1test$features.n_points==points.levels[i]][1:to.plot])
  trainplot = data1train$features.freq1_harmonics_freq_0[
    data1train$sources.original_source_id
    == data1train$features.source_id][1:to.plot]
  d2 = density(trainplot)
  d3 = density(data1test$features.freq1_harmonics_freq_0[
    data1test$features.n_points==points.levels[i]][1:to.plot])
  trainplot = data1train$features.freq1_harmonics_freq_0[
    data1train$row_id==1 & !data1train$contains.random
    & data1train$features.n_points==points.levels[i]][1:to.plot]
  d4 = density(trainplot)
  maxy = max(d2$y,d1$y,d3$y,d4$y)

  plot(d1,xlim=c(-1,5),col='orange',
       lty=1,lwd=2,xlab="Frequency",
       main="",ylim=c(0,maxy))
  lines(d2,col='blue',lty=2,lwd=2)
  legend(2.2,.9*maxy,c(paste(points.levels[i],
                        "Flux / Curve Test"),
                  "Well Sampled Training"),
         col=c("orange","blue"),lty=c(1,2),lwd=2)
  dev.off()

  ## plot the noisy test, noisified train
  pdf(graphics(paste('freq',
                     points.levels[i],
                     points.levels[i],
                     '.pdf',sep="")))
  plot(d3,xlim=c(-1,5),col='orange',lty=1,
       lwd=2,xlab="Frequency",main="",ylim=c(0,maxy))
  lines(d4,col='blue',lty=2,lwd=2)
  legend(2.2,.9*maxy,c(paste(points.levels[i],
                        "Flux / Curve Test"),
                  "Noisified Training"),
         col=c("orange","blue"),lty=c(1,2),lwd=2)
  dev.off()
}









####
#### get measure of variable importance
#### 

rfClassifiers = rfResults[3,]
rfClassifiers = lapply(rfClassifiers,function(x){ x[[1]][[1]] } )
pdf(graphics('varImp10Pt.pdf'))
varImpPlot(rfClassifiers[[1]],main="10 Flux 1x Noisification")
dev.off()
pdf(graphics('varImp100Pt.pdf'))
varImpPlot(rfClassifiers[[10]],main="100 Flux 1x Noisification")
dev.off()


###
### variable importance, an attempt to generate some really
### nice varImp plots, didn't work, going to stick with easy
### way for now
###
class(rfClassifiers)
class(rfClassifiers[[1]])
summary(rfClassifiers[[1]])
names = c()
for(i in 1:length(rfClassifiers)){
  names = union(names,rownames(rfClassifiers[[i]]$importance))
}

impNames = c()
topNumber = 5
whichClassifiers = c(1,5,10)
for(i in whichClassifiers){
  theorder = order(rfClassifiers[[i]]$importance[,1],decreasing=TRUE)
  impNames = union(impNames,
    rownames(rfClassifiers[[i]]$importance)[theorder[1:topNumber]])
}
impNames
importanceMatrix = matrix(0,nrow=length(impNames),
  ncol=length(rfClassifiers))
rownames(importanceMatrix) = impNames
for(i in 1:length(rfClassifiers)){
  importanceMatrix[,i] = rfClassifiers[[i]]$importance[impNames,1]
}
impNames = sub("features.","",impNames)
linecolors = c(2,4,2,4,2,2,2,2,2)
plotLines(importanceMatrix,points.levels,linecolors=linecolors)





###
### examine folded curves - need to make this very general
###

