#####
##### study dubath's p2p feature on simulated light curves
#####
##### by James Long
##### date Sept 5, 2011
#####

source('~/Rmodules/Rfunctions.R')

## P2p scatter : median of the absolute
##               values of the differences
## between successive magnitudes
##    in the raw light curve normalized
## by the MAD around the median
DubathFeature = function(fluxes){
  num = (median(abs(fluxes[2:length(fluxes)]
                    - fluxes[1:(length(fluxes) -1)])))
  den = median(abs(fluxes - median(fluxes)))
  return( num / den )
}

GenerateMeasurements = function(lc_parms,cadence=1,
  number_flux=200){
  mag = 1
  period = runif(1,min=lc_parms[1]-lc_parms[2]/2,
    max=lc_parms[1]+lc_parms[2]/2)
  phase = runif(1,min=0,max=1)
  times = cadence*(1:number_flux)
  folded_times = (times %% period) / period
  folded_times = (folded_times + phase) %% 1
  fluxes = sin(2*pi*folded_times)
  ## print("period is:")
  ## print(period)
  ## print("phase is:")
  ## print(phase)
  return(list(times,fluxes))
}

GenerateAndCompute = function(lc_parms,cadence=1,
  number_flux=200,number_reps=1){
  vals = rep(0,number_reps)
  for(i in 1:length(vals)){
    fluxes = GenerateMeasurements(lc_parms,cadence,
      number_flux)[[2]]
    vals[i] = DubathFeature(fluxes)
  }
  return(vals)
}


n = 200
class1 = c("mean"=.5,"spread"=.25)
class2 = c("mean"=5,"spread"=3)
xlab = "p2p Median"


##### 20 flux measurements
number_flux = 20

### 30 minutes
pdf(paste('dubath_feature',number_flux,'.pdf',sep=""),
    width=12,height=4)
par(mfcol=c(1,3),mar=c(4,4,.5,1))

cadence=1/48
class1_dist = GenerateAndCompute(class1,cadence,
  number_flux=number_flux,number_reps=n)
class2_dist = GenerateAndCompute(class2,cadence,
  number_flux=number_flux,number_reps=n)
DrawKDES(c(class1_dist,class2_dist),
         c(rep("class1",length(class1_dist)),
           rep("class2",length(class2_dist))),
         xlab=paste(xlab," - 30 min cadence",sep=""))


par(mar=c(4,1,.5,1))
### 2 day
cadence=2
class1_dist = GenerateAndCompute(class1,cadence,
  number_flux=number_flux,number_reps=n)
class2_dist = GenerateAndCompute(class2,cadence,
  number_flux=number_flux,number_reps=n)

DrawKDES(c(class1_dist,class2_dist),
         c(rep("class1",length(class1_dist)),
           rep("class2",length(class2_dist))),
         xlab=paste(xlab," - 2 day cadence",sep=""))

par(mar=c(4,1,.5,.5))
### 10 days
cadence=10
class1_dist = GenerateAndCompute(class1,cadence,
  number_flux=number_flux,number_reps=n)
class2_dist = GenerateAndCompute(class2,cadence,
  number_flux=number_flux,number_reps=n)


DrawKDES(c(class1_dist,class2_dist),
         c(rep("class1",length(class1_dist)),
           rep("class2",length(class2_dist))),xlab=paste(xlab," - 10 day cadence",sep=""))
dev.off()





##### 200 flux measurements
number_flux = 200

### 30 minutes
pdf(paste('dubath_feature',number_flux,'.pdf',sep=""),width=12,height=4)
par(mfcol=c(1,3),mar=c(4,4,.5,1))

cadence=1/48
class1_dist = GenerateAndCompute(class1,cadence,
  number_flux=number_flux,number_reps=n)
class2_dist = GenerateAndCompute(class2,cadence,
  number_flux=number_flux,number_reps=n)
DrawKDES(c(class1_dist,class2_dist),
         c(rep("class1",length(class1_dist)),
           rep("class2",length(class2_dist))),xlab=paste(xlab," - 30 min cadence",sep=""))


par(mar=c(4,1,.5,1))
### 2 day
cadence=2
class1_dist = GenerateAndCompute(class1,cadence,
  number_flux=number_flux,number_reps=n)
class2_dist = GenerateAndCompute(class2,cadence,
  number_flux=number_flux,number_reps=n)

DrawKDES(c(class1_dist,class2_dist),
         c(rep("class1",length(class1_dist)),
           rep("class2",length(class2_dist))),xlab=paste(xlab," - 2 day cadence",sep=""))

par(mar=c(4,1,.5,.5))
### 10 days
cadence=10
class1_dist = GenerateAndCompute(class1,cadence,
  number_flux=number_flux,number_reps=n)
class2_dist = GenerateAndCompute(class2,cadence,
  number_flux=number_flux,number_reps=n)


DrawKDES(c(class1_dist,class2_dist),
         c(rep("class1",length(class1_dist)),
           rep("class2",length(class2_dist))),xlab=paste(xlab," - 10 day cadence",sep=""))
dev.off()
