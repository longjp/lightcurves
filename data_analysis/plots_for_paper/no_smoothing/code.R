########## 
########## 
########## CREATE ALL GRAPHICS IN NO SMOOTHING SECTION OF  
########## PAPER (CURRENTLY SECTION 5.1 AS OF DEC 19) 
##########
########## by James Long 
########## date: 12/19/2011 
########## 


####
#### create two graphics, performance for simulated data and
#### robustness for simulated data
####


load('../../synthetic_analysis/randomForestNoisificationResults.RData')

pdf('synrfNoisificationComparison.pdf')
plotLines(errorsSD,points.levels,xlab="Number of Flux Measurements",ylab="Error",maintitle="",ymin=0)
legend("topright",c("Naive","Random","1 x Noise","5 x Noise"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()
