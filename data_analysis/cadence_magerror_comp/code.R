########## 
########## 
########## MAKE PLOT COMPARING CADENCE OF OGLE AND HIPPAROCS 
##########  
##########
########## by James Long 
########## date: March 23, 2013
########## 


rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=60)


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
tfe = '../../data_processed/cadence_comparison_tfe.dat'
time_flux = read.table(tfe,header=TRUE)



### construct plot comparing magnitude error
pdf("figures/mag_error_comparison.pdf")
DrawKDES(time_flux$error,
         time_flux$survey,
         trim=.02,
         line.width=3,
         xlab="Standard Dev. of Magnitude Errors (mags)",
         cex.lab=1.2,
         density.colors=c("blue","orange"),
         line.types=c(2,1))
dev.off()


## make plot comparing cadences
## kde of number flux per l.c. by survey
mean.num.flux <- aggregate(rep(1,nrow(time_flux)),
                           by=list(time_flux$source_id,
                             time_flux$survey),
                           FUN=sum)
head(mean.num.flux)
names(mean.num.flux) <- c("source_id","survey","num_flux")

pdf("figures/measurements_per_lightcurve.pdf")
DrawKDES(mean.num.flux$num_flux,
         mean.num.flux$survey,
         trim=.05,
         line.width=3,
         xlab="Number Measurements Per Light Curve",
         cex.lab=1.2,
         density.colors=c("blue","orange"),
         line.types=c(2,1))
dev.off()







mean.num.flux
aggregate(rep(1,10),list(c(rep(1,5),rep(2,5))),FUN=sum)

list(1:10,11:20)


CurveDiffs = function(x){
  x = x[order(x)]
  x = x[2:length(x)] - x[1:(length(x) - 1)]
  return(x)
}


TotalDiffs = function(x){
  number_time_diffs = nrow(x) - length(unique(x[,1]))
  time_diffs = rep(0,number_time_diffs)
  source_ids = unique(x[,1])
  j = 1
  for(i in source_ids){
    differences = CurveDiffs(x[x[,1] == i,2])
    time_diffs[j:(j + length(differences) - 1)] = differences
    j = j + length(differences)
  }
  return(time_diffs)
}



x <- time_flux[time_flux$survey=="ogle",
               c("source_id","time.")]
time_diffs <- TotalDiffs(x)
d1 <- density(time_diffs,bw=.0001)
plot(d1,xlim=c(0,10))



x <- time_flux[time_flux$survey=="hipparcos",
               c("source_id","time.")]
time_diffs <- TotalDiffs(x)
d1 <- density(time_diffs)

mean(time_diffs < .2)
d1 <- density(time_diffs[time_diffs < .2])

plot(d1)







head(time_flux)
time
