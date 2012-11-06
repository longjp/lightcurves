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




## get n_points measurement
d1 <- aggregate(rep(1,nrow(time_flux)),
                by=list(time_flux$source_id),sum)
names(d1) <- c("features.source_id","sources.n_points")
data1 <- merge(data1,d1)





t1 <- table(Tue Nov  6 13:05:40 2012_flux$source_id)
d1 <- data.frame(t1)
class(d1)
class(d1[,1])
class(d1[,2])
head(d1)
as.integer(names(table(c(15,14,15))))
names(time_flux)
table(time_flux$source_id)

head(data1)
names(data1)





summary(data1$features.freq1_harmonics_freq_0)
summary(data1$features.amplitude)
plot(log(data1$features.freq1_harmonics_freq_0),
     log(data1$features.amplitude))


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
## like having few values causes misclassifications
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

