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


pdf(graphics('hip_classifier_feature_space.pdf'),width=6,
    height=6)
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
legend('topleft',levels(data1hip$sources.classification),
       col=colors1,pch=pch1,cex=1.4)
dev.off()



###
### what does this look like for ogle data
pdf(graphics('hip_classifier_ogle_feature_space.pdf'),
    width=6,height=6)
nums = table(data1ogle$sources.classification)
rands = runif(nrow(data1ogle))
rr = (data1ogle$sources.classification == "RR Lyrae AB" &
      rands < .02)
ceph = (data1ogle$sources.classification == "Classical Cepheid" &
        rands < .08)
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
legend('topright',levels(data1ogle$sources.classification),col=colors1,pch=pch1,cex=1.4)
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

