## nov 6, 2012

## to think about
## 1. we can identify noisy observations from
##    looking at covariance, e.g. get covariance of
##    each observation
##      - but how can we do this in a sort of non-parametric
##        way
## 2. metric for coming up with difference in ranking
##    for observations


## program setup
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)


source('~/Rmodules/Rfunctions.R')
source('../denoisification_code/denoisification.R')
source('../denoisification_code/rf_denoise.R')
library('randomForest')
library('rpart')
library('xtable')
library('np')
library('ks')
require('scatterplot3d')
require('fields')


## get the data
features = '../../data_processed/asas.dat'
tfe = '../../data_processed/asas_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


names(data1)

## add n_points column to data1 so we can
## do diagnostics by number of flux observed
d1 <- aggregate(rep(1,nrow(time_flux)),
                by=list(time_flux$source_id),sum)
names(d1) <- c("features.source_id","sources.n_points")
data1 <- merge(data1,d1,all=TRUE)




## plot period versus amplitude for everything
summary(data1$features.freq1_harmonics_freq_0)
summary(data1$features.amplitude)
dev.new()
pdf("scatterplot.pdf")
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude))
dev.off()

## plot period versus amplitude, highlighting full light
## curves in red
to_use = (data1$features.source_id !=
          data1$sources.original_source_id)
plot(log(data1$features.freq1_harmonics_freq_0[to_use]),
     log(data1$features.amplitude[to_use]))
to_use = !to_use
points(log(data1$features.freq1_harmonics_freq_0[to_use]),
       log(data1$features.amplitude[to_use]),
       col="red")



## plot n curves, each noisy version along with
## clean version (clean is triangle). each of n
## curves is colored differently
n <- 5
orig_ids <- unique(data1$sources.original_source_id)
orig_ids <- sample(orig_ids,n)
data1.sub <- subset(data1,(sources.original_source_id
                           %in% orig_ids))
nrow(data1.sub)
xlim <- c(min(log(data1$features.freq1_harmonics_freq_0)),
          max(log(data1$features.freq1_harmonics_freq_0)))
ylim <- c(min(log(data1$features.amplitude)),
          max(log(data1$features.amplitude)))
pch = ((1 + (data1.sub$sources.original_source_id
             == data1.sub$features.source_id)))
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude),col="#00000030")
t1 <- factor(data1.sub$sources.original_source_id)
points(log(data1.sub$features.freq1_harmonics_freq_0),
     log(data1.sub$features.amplitude),
     xlim=xlim,
     ylim=ylim,
     col=(1 + as.numeric(t1)),
     lwd=3,
     pch=pch)
legend("bottomleft",levels(t1),col=(1+as.numeric(t1)),
       lwd=3,pch=2)






## plot the rcorbor lightcurves
## for some reason the rcorbor do not
## have classifications in the xml files
## CONCLUSIONS: SOME OF THESE ARE DEFINITELY RCORBOR
rcorbor <- c(220040,240306,241463,242999,244506,244888,247066,247575,250762,251121,251489,251638,251987,254404,256072,256221,257713,263740)
rcorbor <- paste("100",rcorbor,".xml",sep="")
rcorbor <- ((data1$sources.original_source_id ==
             data1$features.source_id) &
            (data1$sources.xml_filename %in%
             rcorbor))
rcorbor <- data1$features.source_id[rcorbor]
rcorbor

i <- 1
i <- i + 1
DrawThreeLightCurves(rcorbor[i],data1,time_flux)




## plot period vs. amplitude for all original sources,
## color by Rcorbor
rcorbor <- c(220040,240306,241463,242999,244506,244888,247066,247575,250762,251121,251489,251638,251987,254404,256072,256221,257713,263740)
rcorbor <- paste("100",rcorbor,".xml",sep="")
data1$rcorbor <- (data1$sources.xml_filename %in%
                  rcorbor)
to_use <- (data1$features.source_id ==
          data1$sources.original_source_id)
rcorbor <- to_use & data1$rcorbor
plot(log(data1$features.freq1_harmonics_freq_0[to_use]),
     log(data1$features.amplitude[to_use]),
     col="00000040")
points(log(data1$features.freq1_harmonics_freq_0[data1$rcorbor]),
       log(data1$features.amplitude[data1$rcorbor]),
       col="black",lwd=3)
points(log(data1$features.freq1_harmonics_freq_0[rcorbor]),
       log(data1$features.amplitude[rcorbor]),
       col="red",lwd=3)




#### Questions:
#### 1. where are discovered Rcorbor in feature space
####    (different location than already known ones)
#### 2. why are Rcorbor given extremely long periods
#### 3. the density estimation problem with errors
####   is a lot about determining which obs in low
#### dens areas of features space are only there b/c
#### of error, so even if rcorbor don't change location under
#### perturbation, hopefully some uninteresting things will
#### 4. run a density estimator and see what objects
#### are in low density areas of feature space


## look at period amplitude plot for these sources
## a lot of aliasing at period 1 day, about 10%
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude),
     col=data1$sources.classification,
     pch=as.numeric(data1$sources.classification),
     lwd=2)
legend("bottomleft",levels(data1$sources.classification),
       col=1:length(levels(data1$sources.classification)),
       pch=1:length(levels(data1$sources.classification)),
       lwd=2)
sum(abs(log(data1$features.freq1_harmonics_freq_0)) < .1)




names(data1)
plot(density(data1$features.freq_signif))
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.freq_signif))



## some clustering of points with low period significance
## but not super distinctive
cols <- 1 + 1*(quantile(data1$features.freq_signif,.25) > data1$features.freq_signif)
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude),
     col=cols)

## no clustering by number of points observed, doesn't look
## like having few values causes feature errors
cols <- 1 + 1*(quantile(data1$sources.n_points,.25) > data1$sources.n_points)
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude),
     col=cols)


## figure out why things are aliased
## aliases have freq < .1
## we plot these sources
ind <- abs(log(data1$features.freq1_harmonics_freq_0)) < .1
obs <- data1$features.source_id[ind]

i <- i + 1
DrawThreeLightCurves(obs[i],data1,time_flux)
length(time_flux[time_flux$source_id == i,"time"])
plot(density(time_flux[time_flux$source_id == i,"time"] %% 1))


## what do the high amplitude sources look like
ind <- order(data1$features.amplitude,decreasing=TRUE)

i <- i + 1
DrawThreeLightCurves(ind[i],data1,time_flux)


### 1.
### run density estimator on 2 features
### sort obs by density
### view lightcurves of low prob obs.
###   are they interesting or just noisy series

### 2.
### run 5 density estimators, one on each noisified curve
### sort obs by average density
### view lightcurves with low prob
###   are they more interesting than before


### density estimation in R

### estimate density for 2 features
to_use <- (data1$features.source_id == data1$sources.original_source_id)
dens_data <- data.frame(log(
  data1$features.freq1_harmonics_freq_0[to_use]),
  log(data1$features.amplitude[to_use]))
bw <- Hpi.diag(dens_data)
d1 <- kde(dens_data,H=bw,eval.points=dens_data)
data1$dens[to_use] <- d1$estimate
data1$dens[!to_use] <- Inf


## now look at sources in low density area of feature space
## make X,Y plot and plot light curve
ords <- order(data1$dens)
source_ids <- data1$features.source_id[ords]
i <- 0


dev.off()
dev.off()
i <- i + 1
to_use <- (data1$sources.original_source_id ==
           data1$features.source_id)
plot(log(data1$features.freq1_harmonics_freq_0[to_use]),
     log(data1$features.amplitude[to_use]),col="#00000030")
to_use <- data1$features.source_id == source_ids[i]
points(log(data1$features.freq1_harmonics_freq_0[to_use]),
       log(data1$features.amplitude[to_use]),
       col="red",lwd=3)


dev.new()
DrawThreeLightCurves(source_ids[i],data1,time_flux)
length(time_flux[time_flux$source_id == source_ids[i],"time"])




## ::notes::
## 1. a bunch of sources are outliers in amplitude because
##    a few flux measurements are erroneous
## 2. several outliers are rcorbor, genuinely high amp
## 3. some outliers have period estimated wrong, appear
##    to have true period 2 - 3 times est value

## fundamental questions
## 1. some sources are non-periodic, what is a true
##    period feature here? how will noisifying effect
##    period estimate
## 2. what is introducing noise in the features
##    poor time sampling, flux measurement error, erroneous
##    flux values, sources that are mixtures of 2 objects
## 3. where should outliers be in feature space
## 4. how to incorporate measurements of feature confidence
##    and / or error into this analysis, or does this
##    analysis supplant the need for feature confidences



## what do the high amplitude sources look like
ind <- order(data1$features.amplitude,decreasing=TRUE)

i <- i + 1
DrawThreeLightCurves(ind[i],data1,time_flux)
points(log(data1$features.freq1_harmonics_freq_0[]),
       log(data1$features.amplitude[to_use]),
       col="red")
