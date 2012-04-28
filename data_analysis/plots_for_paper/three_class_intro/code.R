########## 
########## 
########## GENERATE INTRO FIGURES FROM PAPER 
########## THREE CLASS HIP / OGLE PROBLEM 
##########
########## by James Long 
########## date: 12/10/2011 
########## 


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)

## load some functions
source("~/Rmodules/Rfunctions.R")
library("randomForest")
library("rpart")


Tables = fileOutLoc('')
graphics = fileOutLoc('')


ogle_name = c("Mira","RR Lyrae AB","Classical Cepheid")
hip_name = c("Mira","RR Lyrae, Fundamental Mode",
  "Classical Cepheid")
name_conversion = cbind(ogle_name,hip_name)
name_conversion

## load the OGLE source
features = '../../../data_processed/ogleIIIall-fund.dat'
tfe = '../../../data_processed/ogleIIIall-fund-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)
nrow(data1ogle)

## load the hipparcos sources
features = '../../../data_processed/hip_train_three_class.dat'
tfe = '../../../data_processed/hip_train_three_class_tfe.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)

## get rid of several classes
table(data1hip$sources.classification)
nrow(data1hip)
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle = na.roughfix(data1ogle)
data1ogle = RemoveInfinities(data1ogle)

########### NOT NECESSARY FOR PRODUCING GRAPHIC
###########
## ## LOOK AT RANDOM FORESTS
## rf_formula = GetFormula(data1hip)
## rf_features = rf_formula[[2]]
## rf_formula = rf_formula[[1]]
## data1hip_fixed = na.roughfix(data1hip)
## rf_fit = randomForest(rf_formula,
##   data=data1hip_fixed[data1hip_fixed$sources.original_source_id
##     ==data1hip_fixed$features.source_id,])
## rf_fit
## pdf(graphics('naive.pdf'))
## varImpPlot(rf_fit,main='Train on Hipparcos to Classify Ogle')
## dev.off()

## ## what are the predictions
## pred1 = predict(rf_fit,newdata=data1ogle,type='response')
## mean(pred1 != data1ogle$sources.classification)
## table(pred1,data1ogle$sources.classification)
## PrintConfusionMatrix(data1ogle$sources.classification,pred1,
##                      table.name=Tables('naive.tex'))

## ## forest constructed on ogle data
## rf_fit_ogle = randomForest(rf_formula,data=data1ogle)
## rf_fit_ogle
## PrintConfusionMatrix(data1ogle$sources.classification,
##                      rf_fit_ogle$predicted,
##                      table.name=Tables('ogle_on_ogle.tex'))
## pdf(graphics('ogle_on_ogleVarImp.pdf'))
## varImpPlot(rf_fit_ogle,main="Train on Ogle to Classify Ogle")
## dev.off()


### NOW EXAMINE RPART
### 100 % accuracy, a super easy problem

rpart_fit = rpart(rf_formula,data=data1hip_fixed[data1hip_fixed$sources.original_source_id
    ==data1hip_fixed$features.source_id,])
predictions = predict(rpart_fit,newdata=data1ogle,type="class")
mean(predictions != data1ogle$sources.classification)
table(data1ogle$sources.classification,predictions)
PrintConfusionMatrix(data1ogle$sources.classification,predictions,
                     table.name=Tables('naive_cart.tex'))

## output confusion matrix


## rownames(rpart_fit$splits) = sub("features.",
##           "",rownames(rpart_fit$splits))
pdf(graphics('hip_classifier_tree.pdf'),width=6,height=6)
par(mar=c(0,0,0,0))
plot(rpart_fit,margin=.12)
text(rpart_fit,use.n=TRUE)
dev.off()


pdf(graphics('hip_classifier_feature_space.pdf'))
to_use = (data1hip$sources.original_source_id ==
          data1hip$features.source_id)
colors1 = c("black","orange","blue")
pch1 = c(1,2,3)
par(mar=c(4.2,4.2,2.1,.2))
plot(log(data1hip$features.freq1_harmonics_amplitude_0[to_use]),
     log(data1hip$features.fold2P_slope_90percentile[to_use]),
     col=colors1[data1hip$sources.classification[to_use]],
     pch=pch1[data1hip$sources.classification[to_use]],
     xlab='log(freq1_harmonics_amplitude)',
     ylab='log(fold2P_slope_90percentile)',cex=1.5,
     lwd=1.5,cex.lab=1.5)
abline(v=log(.5615),col='grey',lwd=1.5)
lines(c(-10,log(.5615)),rep(log(1.365),2),col='grey',lwd=1.5)
legend('topleft',title="Hipparcos Sources",
       levels(data1hip$sources.classification),
       col=colors1,pch=pch1,cex=1.4)
dev.off()



###
### what does this look like for ogle data
pdf(graphics('hip_classifier_ogle_feature_space.pdf'))
nums = table(data1ogle$sources.classification)
rands = runif(nrow(data1ogle))
rr = (data1ogle$sources.classification == "RR Lyrae AB" &
      rands < .02)
ceph = (data1ogle$sources.classification ==
        "Classical Cepheid" & rands < .08)
mira = (data1ogle$sources.classification == "Mira" &
        rands < .08)
to_use = rr | ceph | mira
sum(to_use)
to_use = (to_use &
          log(data1ogle$features.freq1_harmonics_amplitude_0)<4)
to_use = (to_use &
          log(data1ogle$features.fold2P_slope_90percentile)>-8)
to_use = sample((1:nrow(data1ogle))[to_use],sum(to_use))

colors1 = c("black","orange","blue")
pch1 = c(1,2,3)
par(mar=c(4.1,4,2,.2))
plot(log(data1ogle$features.freq1_harmonics_amplitude_0[to_use]),
     log(data1ogle$features.fold2P_slope_90percentile[to_use]),
     col=colors1[data1ogle$sources.classification[to_use]],
     pch=pch1[data1ogle$sources.classification[to_use]],
     xlab='log(freq1_harmonics_amplitude)',
     ylab='log(fold2P_slope_90percentile)',
     lwd=1.5,cex.lab=1.5,cex=1.5)
abline(v=log(.5615),col='grey',lwd=1.5)
lines(c(-10,log(.5615)),rep(log(1.365),2),col='grey',lwd=1.5)
legend('topright',title="OGLE Sources",levels(data1ogle$sources.classification),col=colors1,pch=pch1,cex=1.4)
dev.off()









#### get cross validated error for this problem
source('~/Rmodules/Rfunctions.R')
data1hip_good = subset(data1hip_fixed,sources.original_source_id ==
  features.source_id)
nrow(data1hip_good)
predictions = CVrpart(data1hip_good,rf_formula)
sum(levels(data1hip_good$sources.classification)[predictions] != data1hip_good$sources.classification)
mean(levels(data1hip_good$sources.classification)[predictions] != data1hip_good$sources.classification)


## now rpart constructed on ogle data
rpart_fit_ogle = rpart(rf_formula,data=data1ogle)
plot(rpart_fit_ogle,margin=.1)
text(rpart_fit_ogle,all=TRUE,use.n=TRUE)
predictions = predict(rpart_fit_ogle,type='class')
mean(predictions != data1ogle$sources.classification)
predictions = CVrpart(data1ogle,rf_formula)
mean(levels(data1ogle$sources.classification)[predictions]
     != data1ogle$sources.classification)











#########
#########
#########
#########
######### MAKE PLOTS IN RESULTS SECTION ON THREE CLASS PROBLEM
#########
#########
#########

rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)

source("~/Rmodules/Rfunctions.R")
library("randomForest")
library("rpart")

ogle_name = c("Mira","RR Lyrae AB","Classical Cepheid")
hip_name = c("Mira","RR Lyrae, Fundamental Mode",
  "Classical Cepheid")
name_conversion = cbind(ogle_name,hip_name)
name_conversion

## load the OGLE source
features = '../../../data_processed/ogleIIIall-fund.dat'
tfe = '../../../data_processed/ogleIIIall-fund-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)
nrow(data1ogle)
table(data1ogle$sources.classification)



## load the hipparcos sources
features = '../../../data_processed/hip_train_three_class.dat'
tfe = '../../../data_processed/hip_train_three_class_tfe.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)

## get rid of several classes
nrow(data1hip)
table(data1hip$sources.classification[
  data1hip$sources.original_source_id
  == data1hip$features.source_id])
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(
  sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle$sources.xml_filename = as.factor(
  data1ogle$sources.xml_filename)
data1ogle = na.roughfix(data1ogle)
data1ogle$sources.xml_filename = as.character(
  data1ogle$sources.xml_filename)
data1ogle = RemoveInfinities(data1ogle)



## NOT USING THESE PLOTS IN PAPER
## ## get mean mag of hipparcos sources
## average.flux = aggregate(time_flux_hip$flux,
##   by=list(time_flux_hip$source_id),mean)
## sd.flux = aggregate(time_flux_hip$flux,
##   by=list(time_flux_hip$source_id),sd)


## head(average.flux)
## names(average.flux) = c("features.source_id","average_flux")
## names(sd.flux) = c("features.source_id","sd_flux")
## nrow(average.flux)
## data1hip.orig = subset(data1hip,
##   sources.original_source_id==features.source_id)
## nrow(data1hip.orig)
## data1hip.orig = merge(data1hip.orig,average.flux)
## nrow(data1hip.orig)
## names(data1hip.orig)


## ## get mean magnitude of the cepheids in OGLE
## average.flux = aggregate(time_flux_ogle$flux,
##   by=list(time_flux_ogle$source_id),mean)
## head(average.flux)
## names(average.flux) = c("features.source_id","average_flux")
## data1ogle = merge(data1ogle,average.flux)
## nrow(data1ogle)
## names(data1ogle)



## to_plot = (data1ogle$sources.classification ==
##            "Classical Cepheid")
## sum(to_plot)
## to_plot_hip = (data1hip.orig$sources.classification==
##                "Classical Cepheid")
## sum(to_plot_hip)


## ymin = min(data1hip.orig$average_flux[to_plot_hip],
##   data1ogle$average_flux[to_plot])
## ymax = max(data1hip.orig$average_flux[to_plot_hip],
##   data1ogle$average_flux[to_plot])

## ymin = 0

## pdf('cepheids_per_versus_mag_ogle_hip.pdf')
## plot(log(1/data1ogle$features.freq1_harmonics_freq_0[to_plot],
##          base=10),
##      data1ogle$average_flux[to_plot],col="#00000020",
##      ylim=c(ymin,ymax),
##      xlab="log_10(period)",ylab="mean magnitude measurement")
## points(log(1/data1ogle$features.freq1_harmonics_freq_0[
##        to_plot & suspicious],base=10),
##        data1ogle$average_flux[to_plot & suspicious],
##        col="red",pch=2)
## points(log(1/data1hip.orig$features.freq1_harmonics_freq_0[
##        to_plot_hip],base=10),
##        data1hip.orig$average_flux[to_plot_hip],
##        col="orange",pch=2)
## abline(h=11.5,col='grey')
## legend('bottomleft',c('ogle','hip'),pch=c(1,2),
##        col=c('black','orange'))
## dev.off()



## ::NEXT TWO PLOTS::
## how well does noisification match frequency for
## RR Lyrae and Miras for the 40 noisified classifier

## RR lyrae
ogles = (data1ogle$sources.classification == "RR Lyrae AB" &
         data1ogle$features.n_points >= 35 &
         data1ogle$features.n_points < 45)
hips1 = (data1hip$sources.original_source_id ==
         data1hip$features.source_id &
        data1hip$sources.classification == "RR Lyrae AB")
hips2 = (grepl('hip',data1hip$sources.noise_args) &
         grepl('first',data1hip$sources.noise_args) &
         data1hip$sources.classification == "RR Lyrae AB" &
         data1hip$features.n_points >= 35 &
         data1hip$features.n_points < 45 &
         !grepl('all',data1hip$sources.noise_args))

new_hips = data1hip[hips2,]
nrow(new_hips)
new_hips = dedupe(new_hips,"sources.original_source_id")
new_hips = new_hips[new_hips$row_id==0,]
nrow(new_hips)

## hips2 = (grepl('all',data1hip$sources.noise_args) &
##          grepl('hip',data1hip$sources.noise_args) &
##          data1hip$sources.classification == "RR Lyrae AB")
sum(ogles)
sum(hips1)
nrow(new_hips)
pdf('rrlyrae_freq_hip_ogle.pdf')
DrawKDES(c(data1ogle$features.freq1_harmonics_freq_0[ogles],
           data1hip$features.freq1_harmonics_freq_0[hips1],
           new_hips$features.freq1_harmonics_freq_0),
         c(rep("ogle",sum(ogles)),rep("hip",sum(hips1)),
         rep("hip noisified",nrow(new_hips))),
         xlab="frequency ( / day )",
         density.colors=c('black','orange','blue'),
         cex.lab=1.4,line.width=4,
         legend.title="RR Lyrae AB")
dev.off()




## classical cepheids
ogles = (data1ogle$sources.classification == "Classical Cepheid" &
         data1ogle$features.n_points >= 35 &
         data1ogle$features.n_points < 45)
hips1 = (data1hip$sources.original_source_id ==
         data1hip$features.source_id &
         data1hip$sources.classification == "Classical Cepheid")
hips2 = (grepl('hip',data1hip$sources.noise_args) &
         grepl('first',data1hip$sources.noise_args) &
         data1hip$sources.classification == "Classical Cepheid" &
         data1hip$features.n_points >= 35 &
         data1hip$features.n_points < 45 &
         !grepl('all',data1hip$sources.noise_args))

new_hips = data1hip[hips2,]
nrow(new_hips)
new_hips = dedupe(new_hips,"sources.original_source_id")
new_hips = new_hips[new_hips$row_id==0,]
nrow(new_hips)


sum(ogles)
sum(hips1)
nrow(new_hips)
pdf('cepheid_freq_hip_ogle.pdf')
DrawKDES(c(data1ogle$features.freq1_harmonics_freq_0[ogles],
           data1hip$features.freq1_harmonics_freq_0[hips1],
           new_hips$features.freq1_harmonics_freq_0),
         c(rep("ogle",sum(ogles)),rep("hip",sum(hips1)),
         rep("hip noisified",sum(hips2))),
         xlab="frequency ( / day )",
         density.colors=c('black','orange','blue'),
         cex.lab=1.4,line.width=4,
         legend.title="Classical Cepheid")
dev.off()



## for miras
ogles = (data1ogle$sources.classification == "Mira" &
         data1ogle$features.n_points >= 35 &
         data1ogle$features.n_points < 45)
hips1 = (data1hip$sources.original_source_id ==
         data1hip$features.source_id &
         data1hip$sources.classification == "Mira")
hips2 = (grepl('hip',data1hip$sources.noise_args) &
         grepl('first',data1hip$sources.noise_args) &
         data1hip$sources.classification == "Mira" &
         data1hip$features.n_points >= 35 &
         data1hip$features.n_points < 45 &
         !grepl('all',data1hip$sources.noise_args))

new_hips = data1hip[hips2,]
nrow(new_hips)
new_hips = dedupe(new_hips,"sources.original_source_id")
new_hips = new_hips[new_hips$row_id==0,]
nrow(new_hips)


sum(ogles)
sum(hips1)
nrow(new_hips)
pdf('mira_freq_hip_ogle.pdf')
DrawKDES(c(data1ogle$features.freq1_harmonics_freq_0[ogles],
           data1hip$features.freq1_harmonics_freq_0[hips1],
           new_hips$features.freq1_harmonics_freq_0),
         c(rep("ogle",sum(ogles)),rep("hip",sum(hips1)),
         rep("hip noisified",sum(hips2))),
         xlab="frequency ( / day )",
         density.colors=c('black','orange','blue'),
         cex.lab=1.4,line.width=4,
         xlimits=c(0,.015),
         legend.title="Mira")
dev.off()













#### ::NEXT THREE PLOTS::
#### HOW DOES P2PS BEHAVE
source("~/Rmodules/Rfunctions.R")

feature = "features.p2p_scatter_over_mad"

## unlabeled set -- ogle
ogles = (data1ogle$features.n_points >= 35 &
         data1ogle$features.n_points < 45)
sum(ogles)
pdf('p2p_scatter_ogle.pdf')
par(mar=c(4.5,4,.5,.5))
DrawKDES(data1ogle[ogles,feature],
         data1ogle$sources.classification[ogles],
         density.colors=c('black','orange','blue'),
         cex.lab=1.4,line.width=4,
         xlab="P2PS",
         legend.title="OGLE")
dev.off()

## without noisification
hips1 = (data1hip$sources.original_source_id ==
         data1hip$features.source_id)
pdf('p2p_scatter_hip_unnoisified.pdf')
par(mar=c(4.5,4,.5,.5))
DrawKDES(data1hip[hips1,feature],
         data1hip[hips1,"sources.classification"],
         density.colors=c('black','orange','blue'),
         cex.lab=1.4,line.width=4,
         xlab="P2PS",legend.title="Hipparcos")
dev.off()

## with noisification
hips2 = (grepl('hip',data1hip$sources.noise_args) &
         grepl('first',data1hip$sources.noise_args) &
         data1hip$features.n_points >= 35 &
         data1hip$features.n_points < 45 &
         !grepl('all',data1hip$sources.noise_args))
new_hips = data1hip[hips2,]
nrow(new_hips)
new_hips = dedupe(new_hips,"sources.original_source_id")
new_hips = new_hips[new_hips$row_id==0,]
nrow(new_hips)


pdf('p2p_scatter_hip_noisified.pdf')
par(mar=c(4.5,4,.5,.5))
DrawKDES(new_hips[,feature],
         new_hips[,"sources.classification"],
         density.colors=c('black','orange','blue'),
         cex.lab=1.4,line.width=4,
         xlab="P2PS",legend.title="Hipparcos Noisified")
dev.off()
