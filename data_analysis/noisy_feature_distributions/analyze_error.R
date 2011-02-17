#######
####### analyze error from debosscher data
#######
####### by James Long
####### created Feb 13, 2011    last modified Feb 14, 2011
#######


# get the data
tfe = read.table("tfe.txt",sep=";",header=TRUE)
classes = read.table("class_output.txt",sep=";")
names(classes) = c("source_id","classification")
data1 = merge(tfe,classes,by=c("source_id"))


ApplyCurves = function(curve.of.interest,
                       num.flux.measurements,
                       function.name){
  result = rep(0,length(num.flux.measurements))
  curve.of.interest = curve.of.interest[order(
    curve.of.interest$time),]
  for(i in 1:length(result)){
    result[i] = function.name(
            curve.of.interest$flux[1:num.flux.measurements[i]])
  }
  return(result)
}

Skew = function(x){
  return((sum((x - mean(x))^3) / (sd(x) * (length(x) - 1) / length(x))^3) / length(x))
}

FluxPercentileRatioMid20 = function(x){
  quants = quantile(x,probs=c(.95,.05,.6,.4))
  return((quants[3] - quants[4]) / (quants[1] - quants[2]))
}

FluxPercentileRatioMid80 = function(x){
  quants = quantile(x,probs=c(.95,.05,.9,.1))
  return((quants[3] - quants[4]) / (quants[1] - quants[2]))
}


curves = data1
num.flux.measurements = 10 + ((0:18) * 5)
results = matrix(0,nrow=length(unique(curves$source_id)),
                 ncol=length(num.flux.measurements))
source.ids = unique(curves$source_id)
rownames(results) = source.ids
truth = rep(0,length(source.ids))
function.name = FluxPercentileRatioMid80

for(i in 1:length(source.ids)){
  curve.of.interest = subset(curves,
                             subset=source_id==source.ids[i])
  truth[i] = function.name(curve.of.interest$flux)
  results[i,] = ApplyCurves(curve.of.interest,
                            num.flux.measurements,
                            function.name)
}  
difference = results - truth




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





