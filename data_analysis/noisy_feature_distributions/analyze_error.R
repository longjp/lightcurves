#######
####### analyze error from debosscher data
#######
####### by James Long
####### created Feb 13, 2011    last modified Feb 22, 2011
#######


rm(list = ls(all = TRUE))

library('randomForest')

# get the data
tfe = read.table("tfe.txt",sep=";",header=TRUE)
features = read.table("features.txt",sep=";",header=TRUE)
source.ids = data.frame(features$features.source_id,
  features$sources.classification)
names(source.ids) = c("source_id","class")
data1 = merge(tfe,source.ids,by=c("source_id"))


ApplyCurves = function(curve.of.interest,
                       num.flux.measurements,
                       function.names){
  curve.of.interest = curve.of.interest[order(curve.of.interest$time),]
  result = t(sapply(function.names, function(x){ sapply(num.flux.measurements,function(y){x(curve.of.interest$flux[1:y])})}))
  return(result)
}


# feature functions
Skew = function(x){
  numerator =  mean((x - mean(x))^3)
  denominator = mean((x - mean(x))^2)^(3/2)
  return(numerator / denominator)
}

Beyond1Std = function(x){
  return(sum(abs(x - mean(x)) > sd(x)) / length(x))
}

Kurtosis = function(x){
  numerator = mean((x - mean(x))^4)
  denominator = mean((x - mean(x))^2)^2
  return((numerator / denominator) - 3)
}

MedianBufferRangePercentage = function(x){
  return( mean( abs(x - median(x)) < .2*(max(x) - min(x)) )   )
}

MedianAbsoluteDeviation = function(x){
  return(median(abs(x - median(x))))
}
  
PercentAmplitude = function(x){
  numerator = max(max(x) - median(x),median(x) - min(x))
  denominator = (max(x) - min(x))
  return( numerator / denominator )
}

Std = function(x){
  return(sd(x))
}

FluxPercentileRatioMid20 = function(x){
  quants = quantile(x,probs=c(.95,.05,.6,.4))
  return((quants[3] - quants[4]) / (quants[1] - quants[2]))
}

FluxPercentileRatioMid35 = function(x){
  quants = quantile(x,probs=c(.95,.05,.675,.325))
  return((quants[3] - quants[4]) / (quants[1] - quants[2]))
}

FluxPercentileRatioMid50 = function(x){
  quants = quantile(x,probs=c(.95,.05,.75,.25))
  return((quants[3] - quants[4]) / (quants[1] - quants[2]))
}

FluxPercentileRatioMid65 = function(x){
  quants = quantile(x,probs=c(.95,.05,.825,.175))
  return((quants[3] - quants[4]) / (quants[1] - quants[2]))
}

FluxPercentileRatioMid80 = function(x){
  quants = quantile(x,probs=c(.95,.05,.9,.1))
  return((quants[3] - quants[4]) / (quants[1] - quants[2]))
}



# select some subset of curves to use - at least 100 points
curves = data1
n.points = table(curves$source_id)
enough.points = as.numeric(names(n.points[n.points > 100]))
curves = subset(curves,subset=source_id %in% enough.points)
nrow(curves)
source.ids = unique(curves$source_id)
table1 = table(features$sources.class[features$features.source_id %in% source.ids])
large.classes = names(table1[table1 > 50])
curves = subset(curves,subset=class %in% large.classes)
nrow(curves)
source.ids = unique(curves$source_id)
length(source.ids)



# function we are applying to the curves
function.names = c(Skew,Beyond1Std,FluxPercentileRatioMid80,Kurtosis,MedianBufferRangePercentage,MedianAbsoluteDeviation,PercentAmplitude,Std,FluxPercentileRatioMid20,FluxPercentileRatioMid35,FluxPercentileRatioMid50,FluxPercentileRatioMid65,FluxPercentileRatioMid80)
num.flux.measurements = 5 * (1:20)

# create array to store results
truth = matrix(0,nrow=length(source.ids),ncol=length(function.names))
results = array(0, dim = c(length(unique(curves$source_id)),length(function.names),length(num.flux.measurements)), dimnames = c('curve','feature','number.flux'))

# give names to the elements in each dimension



# compute all the stuff
for(i in 1:length(source.ids)){
  print(i)
  curve.of.interest = subset(curves,
                             subset=source_id==source.ids[i])
  truth[i,] = sapply(function.names,
                     function(x) { x(curve.of.interest$flux)})
  results[i,,] = ApplyCurves(curve.of.interest,
                            num.flux.measurements,
                            function.names)
}  

# get differences from truth
difference = array(0, dim = c(length(unique(curves$source_id)),length(function.names),length(num.flux.measurements)), dimnames = c('curve','feature','number.flux'))
for(i in 1:length(num.flux.measurements)){
  difference[,,i] = results[,,i] - truth
}

# variances sanity check
var.diffs = apply(difference,c(2,3),var)
dim(var.diffs)
row.maxes = apply(var.diffs,1,max)
var.diffs.normalized = var.diffs / row.maxes
plot(c(0,0,max(num.flux.measurements)),c(0,1,0),col=0)
for(i in 1:nrow(var.diffs.normalized)){
  lines(num.flux.measurements,var.diffs.normalized[i,],ylab="Normalized Variance",xlab="Number Flux Measurements")
}


# now construct classifiers
source.classes = sapply(source.ids,function(x){curves$class[curves$source_id == x][1]})
source.classes = factor(as.character(source.classes))
table(source.classes) / length(source.classes)
max(table(source.classes) / length(source.classes))



training = runif(length(source.classes)) < .75
rf.truth = randomForest(truth[training,],source.classes[training])
summary(rf.truth)
mean(rf.truth$predicted != source.classes[training])

mean(predict(rf.truth,truth[!training,]) != source.classes[!training])


err.rate.clean = rep(0,dim(results)[3])
err.rate.noisified = rep(0,dim(results)[3])
for(i in 1:(dim(results)[3])){
  print(i)
  rf = randomForest(results[training,,i],source.classes[training])
  predictions = predict(rf,newdata=results[!training,,i])
  err.rate.noisified[i] = mean(predictions != source.classes[!training])
  err.rate.clean[i] = mean(predict(rf.truth,results[!training,,i]) != source.classes[!training])
}

ymin = min(err.rate.clean,err.rate.noisified) * .9
ymax = max(err.rate.clean,err.rate.noisified) * 1.1
plot(num.flux.measurements,err.rate.clean,type='l',ylim=c(ymin,ymax))
lines(num.flux.measurements,err.rate.noisified,col='green')



d1 = density(difference[,1])
d2 = density(difference[,2])
dlast = density(difference[,-1])
plot(d1)
lines(d2)
lines(dlast)


















####### start of code
n = length(classes)
p = length(table(classes))

#  log amplitude
feat = log10(features[,8])

gr = seq(-3,max(feat)+.2,length.out=2000)
sp.grid = cbind(rep(gr,p),sort(rep(1:p,2000)),rep(0,p*2000))

cl.num = as.numeric(classes)
for(ii in 1:p){
de = density(feat[cl.num==ii],n = 2000,from=min(gr),to=max(gr),bw=.1)
sp.grid[((ii-1)*2000 +1) : (2000*ii), 3] = de$y
}
require(scatterplot3d)
require(fields)
tc = paste(tim.colors(n=p)[c(seq(1,p,3),seq(2,p,3),seq(3,p,3))],"60",sep="")
col = tc[sp.grid[,2]]

pdf("/Users/jwrichar/Documents/CDI/TCP/debosscher/plots/classHists_amplitude.pdf",height=8,width=10)
layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(1.5,8), heights=c(2,2))
par(mar=c(3,0,0,0))
s3d = scatterplot3d(sp.grid[,1],(p+1)-sp.grid[,2],sp.grid[,3],type='n',color=col,pch='',box=F,angle=90,scale.y=5,axis=F,y.ticklabs=sort(levels(classes),decreasing=T),xlim=c(-1,0),lab=c(6,p),grid=FALSE,mar=c(5,0,0,0))
text(s3d$xyz.convert(rep(0.1,p), (1:p)+.4, rep(0,p)),labels =sort(levels(classes),decreasing=T),cex=.8,col='gray10',pos=2)

s3d = scatterplot3d(sp.grid[,1],(p+1)-sp.grid[,2],sp.grid[,3],type='h',color=col,pch='',box=F,angle=90,scale.y=5,axis=F,y.ticklabs=sort(levels(classes),decreasing=T),xlim=c(min(sp.grid[,1]),1),lab=c(6,p),mar=c(5,0,0,0))
axis(1,labels=(-6:2)/2,at=-6:2)
title(xlab="log freq1_harmonics_amplitude_0",cex.lab=1.5)
dev.off()





