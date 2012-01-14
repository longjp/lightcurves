########## 
########## 
########## CREATE ALL GRAPHICS IN NO SMOOTHING SECTION OF  
########## PAPER (CURRENTLY SECTION 5.1 AS OF DEC 19) 
##########
########## by James Long 
########## date: 12/19/2011 
########## 





#### FOR SYNTHETIC
#### create two graphics, performance for simulated data and
#### robustness for simulated data
####

### performance for simulated
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
source('~/Rmodules/Rfunctions.R')
load('../../synthetic_no_smoothing/RData/randomForestNoisificationResults.RData')

pdf('synrfNoisificationComparison.pdf')
plotLines(errorsSD,points.levels,
          xlab="Number of Flux Measurements",
          ylab="Error",maintitle="",ymin=0,
          cex.lab=1.4)
legend("topright",c("Naive","Unordered","1 x Noise",
                    "5 x Noise"),
       col=1:length(class.names),lwd=4,cex=1.5,
       title="Classifiers",pch=1:length(class.names))
dev.off()


### robustness for simulated
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
source('~/Rmodules/Rfunctions.R')
load('../../synthetic_no_smoothing/RData/robustnessNoisificationResults.RData')

pdf('synrobustError.pdf')
plotLines(errorsSD,points.levels,
          xlab="Number of Flux Measurements",
          ylab="Error",ymin=0,maintitle="",cex.lab=1.4)
legend("topright",line.names,col=1:length(class.names),
       lwd=4,cex=1.5,title="Classifiers",
       pch=1:length(class.names))
dev.off()








#### FOR OGLE
#### create four graphics, performance for simulated data,
#### robustness for simulated data, and var imp plots
####


### performance for ogle
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
source('~/Rmodules/Rfunctions.R')
load('../../OGLE/RData/randomForestNoisificationResults.RData')


pdf('oglerfNoisificationComparison.pdf')
plotLines(errorsSD,points.levels,
          xlab="Number of Flux Measurements",
          ylab="Error",maintitle="",ymin=0,
          cex.lab=1.4)
legend("topright",c("Naive","Unordered","1 x Noise",
                    "5 x Noise"),
       col=1:length(class.names),lwd=4,cex=1.5,
       title="Classifiers",pch=1:length(class.names))
dev.off()


### robustness for simulated
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
source('~/Rmodules/Rfunctions.R')
load('../../OGLE/RData/robustnessNoisificationResults.RData')

pdf('oglerobustError.pdf')
plotLines(errorsSD,points.levels,
          xlab="Number of Flux Measurements",
          ylab="Error",ymin=0,maintitle="",cex.lab=1.4)
legend("topright",line.names,col=1:length(class.names),
       lwd=4,cex=1.5,title="Classifiers",
       pch=1:length(class.names))
dev.off()


#### variable importance plots for OGLE

rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
library('randomForest')
source('~/Rmodules/Rfunctions.R')
load('../../OGLE/RData/rfClassifiers.RData')
for(i in 1:length(points.levels)){
  rownames(rfClassifiers[[i]]$importance) =
    sub('features.','',rownames(rfClassifiers[[i]]$importance))
  pdf(paste('varImp',points.levels[i],
                     'Pt.pdf',sep=""))
  par(mar=c(4.5,1,1,1))
  varImpPlot(rfClassifiers[[i]],main="",type=1,cex.lab=1.25,
             cex=1.4,n.var=15,pch=19)
  dev.off()
}





