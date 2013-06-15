options(width=50)
source("~/Rmodules/Rfunctions.R")
library('randomForest')

## columns we will use in analysis
columns <- c("freq1_harmonics_freq_0","p2p_scatter_2praw","skew","amplitude","flux_percentile_ratio_mid20","flux_percentile_ratio_mid80","flux_percentile_ratio_mid35","classification","filename")

df1 <- read.table("../../data_processed/rr_residual.dat",
                  header=TRUE)
df1$filename[1]
df1$filename <- gsub("../data_processed/rr_residual/",
                     "",
                     df1$filename)



df1$classification <- "residual"
df1$classification <- as.factor(df1$classification)

df3 <- read.table("../../data_processed/rr_orig.dat",
                  header=TRUE)
df3$filename <- gsub("../data_processed/rr_orig/","",df3$filename)



plot(density(df1$flux_percentile_ratio_mid80))

period_range <- ((2 / df1$freq1_harmonics_freq_0) > 5) & (((2 / df1$freq1_harmonics_freq_0) <  200))

filenames <- df1$filename[order((-1*df1$flux_percentile_ratio_mid80) - max(df1$freq_signif)*(!period_range),decreasing=TRUE)]

filenames[1]




i <- 0

i <- i + 1
period <- 2/(df3[df3$filename == filenames[i],
                  "freq1_harmonics_freq_0"])
period2 <- 2/(df1[df1$filename == filenames[i],
                  "freq1_harmonics_freq_0"])
period
period2

fname <- gsub("../data_processed/rr_residual/","",
                  filenames[i])
tfe <- read.table(paste("../../data/ogle-rr-i/",
                        fname,
                        sep=""))

#pdf(paste("candidates2/",fname,".pdf",sep=""))
DrawEclipsingRR2(tfe,period,period2,main=fname)
#dev.off()




DrawEclipsingRR2 <- function(tfe,
                             period,
                             period2,
                             smoother=TRUE,
                             point.colors=FALSE,
                             plot.unfolded=TRUE,
                             plot.folded=TRUE,
                             plot.folded.twice=TRUE,
                             par=TRUE,
                             plot.errors=TRUE,
                             main="lightcurve"){
  ## if source id is a list, grab a random l.c. to plot
  if(par){
    par(mfcol=c(3,2))
  }

  times_orig <- tfe[,1]
  for(jj in c(1,2)){
    ## plot the raw light curve
    plotLightCurve(tfe,
                   point.colors=point.colors,
                   plot.errors=plot.errors,
                   maintitle=main)
    
    ## prepare for folding, supsmu needs this
    tfe[,1] = tfe[,1] - min(tfe[,1])  
    tfe[,1] = (tfe[,1] %% period) / period

    ## fold on twice period
    if(plot.folded.twice){
      plotLightCurve(tfe,xLabel="Phase",
                     maintitle=period,point.colors=point.colors,
                     plot.errors=plot.errors)
      if(smoother){
        line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
        print("length of smoothed")
        print(summary(line.smu))
        print("length of tfe[,1]")
        print(length(tfe[,1]))
        line.smu$y = -1 * line.smu$y
        lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
        line.smu = supsmu(tfe[,1],tfe[,2],
          span=.05,wt=1/tfe[,3],periodic=TRUE,bass=8)
        line.smu$y = -1 * line.smu$y
        lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)
      }
    }  

    ## fold on estimated period
    tfe[,1] = (tfe[,1] %% .5) / .5
    if(plot.folded){
      plotLightCurve(tfe,xLabel="Phase",
                     maintitle=period/2,point.colors=point.colors,
                     plot.errors=plot.errors)  
      if(smoother){
        line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
        line.smu$y = -1 * line.smu$y
        lines(line.smu$x,line.smu$y,col='red',lty=1,lwd=2)
        line.smu = supsmu(tfe[,1],tfe[,2])
        line.smu$y = -1 * line.smu$y
        lines(line.smu$x,line.smu$y,col='green',lty=1,lwd=2)
      }
    }
    line.smu$y <- -1*line.smu$y
    tfe[,2] <- (tfe[,2] - line.smu$y[rank(tfe[,1])])
    tfe[,1] <- times_orig
    period <- period2
  }
}
