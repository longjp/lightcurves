########## 
########## 
########## LIGHT CURVE IMAGES FOR QUAL TALK 
##########   
##########
########## by James Long 
########## date: 4/23/2011 
########## 

options(width=50)
library('foreign')


features = '../../data_processed/OGLE/sources00001.dat'
tfe = '../../data_processed/OGLE/tfe00001.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)

## how much OGLE data
sum(data1$sources.original_source_id == data1$features.source_id)
origs = data1$sources.original_source_id == data1$features.source_id
table(data1$sources.classification[origs])

fold_curve = function(times,freq){
	number_periods = times * freq
	folded = number_periods - floor(number_periods)
	return(folded)
}


## get several types of curves
obs_vec = (1:nrow(data1))[data1$features.source_id
                          == data1$sources.original_source_id]
originals = data.frame(obs_vec,data1$sources.classification[obs_vec])
class.names = names(table(originals[,2]))
curves.to.plot = vapply(class.names,function(x) {
  originals[originals[,2] == x,1][1]},0)
curves.to.plot

names(curves.to.plot) = gsub("[() ,]","",names(curves.to.plot))
for(i in 1:length(curves.to.plot)){
  pdf(paste(names(curves.to.plot)[i],".pdf",sep=""),width=7,height=4)
  plotCurve(curves.to.plot[i])
  dev.off()
}


curve.num = 25
relevant_curves = subset(time_flux,
  subset=(source_id==
          data1$sources.original_source_id[curve.num]))[,2:4]
names(relevant_curves)
dim(relevant_curves)
relevant_curves[,1] = relevant_curves[,1] - min(relevant_curves[,1])

pdf('curve_to_features.pdf',width=5,height=3)
##par(mar=c(1,1,1,1))
plotLightCurve(relevant_curves,
     yLabel='m',
     maintitle=data1$sources.classification[curve.num],
     xLabel="Time (Days)")
dev.off()


### not using axes or anything
plotCurve = function(i,reverse=TRUE){
  par(mfcol=c(2,1))
  relevant_curves = subset(time_flux,
    subset=(source_id==data1$sources.original_source_id[i]))[,2:3]
  par(mar=rep(.1,4))
  if(reverse) relevant_curves[,2] = -relevant_curves[,2]
  plot(relevant_curves[,1],relevant_curves[,2],
       ylab='Flux',pch=19,cex=.5,axes=FALSE)
  box("plot")
  folded_times = fold_curve(relevant_curves[,1],
    data1[i,"features.freq1_harmonics_freq_0"]/2)
  par(mar=rep(.1,4))
  plot(folded_times,relevant_curves[,2],axes=FALSE,
       pch=19,cex=.5)
  box("plot")
  box("outer", lty="solid", col="black",lwd=8)
}


source('../noisification_code/Rfunctions.R')
##
## plot an unfolded and a folded lightcurve
##
curve.no = 120
ID = data1$sources.original_source_id[curve.no]
relevant_curves = subset(time_flux,
  subset=(source_id==ID))[,2:4]
period = 1 / data1$features.freq1_harmonics_freq_0[
  data1$features.source_id == ID]
curve.class = data1$sources.classification[
  data1$features.source_id == ID]

## print info
curve.class
period
period = 2*period
dim(relevant_curves)

## normalize
relevant_curves[,1] = relevant_curves[,1] - min(relevant_curves[,1])
dim(relevant_curves)
names(relevant_curves)

pdf("unfolded.pdf",width=7,height=3)
plotLightCurve(relevant_curves,maintitle="",
               xLabel="Time (Days)",reverse=TRUE)
dev.off()
relevant_curves[,1] = (relevant_curves[,1] %% period) / period
relevant_curves2 = relevant_curves
relevant_curves2[,1] = relevant_curves2[,1] + 1
relevant_curves = rbind(relevant_curves,relevant_curves2)
pdf("folded.pdf",width=7,height=3)
plotLightCurve(relevant_curves,maintitle="",xLabel="Phase",
               reverse=TRUE)
dev.off()




###
### two images, how do we use well sampled curves
### to classify poorly sampled curves?
###
## a well sampled curve
relevant_curves = subset(time_flux,
  subset=(source_id==data1$sources.original_source_id[10]))[,2:4]
relevant_curves[,1] = relevant_curves[,1] - min(relevant_curves[,1])

pdf("well_sampled_lightcurve.pdf",width=7,height=3)
plotLightCurve(relevant_curves,maintitle="",xLabel="Time (Days)")
dev.off()

## a poorly sampled curve
relevant_curves = subset(time_flux,
  subset=(source_id==data1$sources.original_source_id[100]))[,2:4]
relevant_curves = relevant_curves[1:25,]
relevant_curves[,1] = relevant_curves[,1] - min(relevant_curves[,1])

pdf("poorly_sampled_lightcurve.pdf",width=7,height=3)
plotLightCurve(relevant_curves,xLabel="Time (Days)",maintitle="")
dev.off()


minidata = data1[1:5,c("sources.classification","features.freq1_harmonics_freq_0","features.max_slope","features.skew","features.amplitude")]
names(minidata) = c("class","frequency","max_slope","skew","amp.")
minidata


###
### show what can go wrong with classification
### poorly sampled curves may still be separable,
### but not by classifier constructed on training set
### TODO: check all this and output plots

library('rpart')
classes = c("W Ursae Majoris","Multiple Mode Cepheid")
originals = subset(data1,sources.original_source_id ==
  features.source_id,survey=train)
table(originals$sources.classification)
originals = subset(originals,sources.classification %in% classes)
nrow(originals)
table(originals$sources.classification)
originals$sources.classification = as.factor(as.character(
  originals$sources.classification))
data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id",
  "features.max_slope","features.min",
  "features.linear_trend","features.max",
  "features.weighted_average","features.median")
data1_features = data1_features[!(data1_features %in%
  to_remove)]
rf_formula = formula(paste("sources.classification ~ ",
  paste(data1_features,collapse=" + ")))
tree = rpart(rf_formula,data=originals)


contains.random = grepl("random",data1$sources.noise_args)
data1$contains.random = contains.random
data1 = dedupe(data1,
  c("features.n_points","sources.original_source_id",
    "contains.random")
  )
flux20 = subset(data1,features.n_points==20 & sources.survey=="train" & !contains.random & row_id == 1)
nrow(flux20)
tree20 = rpart(rf_formula,data=flux20)
tree20
