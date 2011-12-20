pdf(graphics('rfNoisificationComparison.pdf'))
plotLines(errorsSD,points.levels,xlab="Number of Flux Measurements",ylab="Error",maintitle="",ymin=0)
legend("topright",c("Naive","Random","1 x Noise","5 x Noise"),col=1:length(class.names),lwd=2,cex=1,title="Classifiers",pch=1:length(class.names))
dev.off()
