## nov 6, 2012

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
require('scatterplot3d')
require('fields')


## get the data
features = '../../data_processed/asas.dat'
tfe = '../../data_processed/asas_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)




## add n_points column to data1 so we can
## do diagnostics by number of flux observed
d1 <- aggregate(rep(1,nrow(time_flux)),
                by=list(time_flux$source_id),sum)
names(d1) <- c("features.source_id","sources.n_points")
data1 <- merge(data1,d1,all=TRUE)




## plot period versus amplitude for everything
summary(data1$features.freq1_harmonics_freq_0)
summary(data1$features.amplitude)
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude))

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
n <- 10
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
plot(log(data1.sub$features.freq1_harmonics_freq_0),
     log(data1.sub$features.amplitude),
     xlim=xlim,
     ylim=ylim,
     col=factor(data1.sub$sources.original_source_id),
     lwd=3,
     pch=pch)



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
     log(data1$features.amplitude[to_use]))
points(log(data1$features.freq1_harmonics_freq_0[rcorbor]),
       log(data1$features.amplitude[rcorbor]),
       col="red",lwd=2)




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




d1 <- density(log(data1$features.freq1_harmonics_freq_0),
              log(data1$features.amplitude))

dens_data = data.frame(log(
  data1$features.freq1_harmonics_freq_0),
  log(data1$features.amplitude))
bw <- npudensbw(dat=dens_data)
d1 <- npudens(bw,dat=dens_data)

d1
d1(c(1,1))

