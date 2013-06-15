## for reading in all derived feature files from RR
##
## by James Long
## date December 6, 2012
##


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
nrow(df1)
names(df1)

df1$classification <- "residual"
df1 <- df1[,columns]
length(columns)
length(df1)
summary(df1)

features = '../../data_processed/eclipse-rr.dat'
data1 = read.table(features,sep=';',header=TRUE)
names(data1)
## subset to just get eclipsing
## remove sources / featuers
## select only columns that match earlier
data1 <- subset(data1,sources.classification %in% c("Beta Lyrae","Algol (Beta Persei)","Beta Cephei"))
unique(data1$sources.classification)
data1$sources.classification <- "eclipsing"

## simplify names
names(data1) <- sub("sources.","",names(data1))
names(data1) <- sub("features.","",names(data1))
names(data1)

data1$filename <- data1$xml_filename
data1 <- data1[,columns]
summary(data1)



df2 <- rbind(df1,data1)
df2$classification <- as.factor(df2$classification)

nrow(df2)
nrow(df1) + nrow(data1)
unique(df2$classification)
summary(df2)


## after merge
## mark eclipsing rr source
df2$eclipsing <- 1*grepl("OGLE-BLG-RRLYR-02792.dat",
                         df2$filename)
sum(df2$eclipsing)

ConstructFormula = function(vars,response,toremove=c()){
  features <- vars[-which(vars %in% c(toremove,response))]
  f1 = formula(paste(response, " ~ ",
    paste(features,collapse=" + ")))
  return(f1)
}


f1 <- ConstructFormula(names(df2),"classification",
                       c("filename","eclipsing"))


rf.fit <- randomForest(f1,data=df2)

df2$eclipsing_prob <- predict(rf.fit,type="prob")[,"eclipsing"]
plot(df2$eclipsing_prob,rnorm(nrow(df2),sd=.1) + as.numeric(df2$classification),col=df2$classification)




df3 <- read.table("../../data_processed/rr_orig.dat",
                  header=TRUE)
df3$filename <- gsub("../data_processed/rr_orig/","",df3$filename)



period_range <- (df2$amplitude > 1)
sum(period_range)
##period_range <- ((2 / df2$freq1_harmonics_freq_0) > 30) & (((2 / df2$freq1_harmonics_freq_0) <  1000))


filenames <- df2$filename[order(df2$eclipsing_prob - 1*(df2$classification=="eclipsing") - 1*(!period_range),decreasing=TRUE)]
filenames <- as.character(filenames)



i <- 0

i <- i + 1
period <- 2/(df3[df3$filename == filenames[i],
                  "freq1_harmonics_freq_0"])
period2 <- 2/(df2[df2$filename == filenames[i],
                  "freq1_harmonics_freq_0"])
period
period2

fname <- gsub("../data_processed/rr_residual/","",
                  filenames[i])
tfe <- read.table(paste("../../data/ogle-rr-i/",
                        fname,
                        sep=""))

#pdf(paste("candidates/",fname,".pdf",sep=""))
DrawEclipsingRR2(tfe,period,period2,main=fname)
#dev.off()




filename <- "OGLE-LMC-RRLYR-03441.dat"
period <- 2/(df3[df3$filename == filename,
                  "freq1_harmonics_freq_0"])
period
tfe <- read.table(paste("../../data/ogle-rr-i/",
                        filename,
                        sep=""))
point.colors=FALSE
plot.errors=TRUE
plotLightCurve(tfe,xLabel="Phase",
               maintitle=period,point.colors=point.colors,
               plot.errors=plot.errors)
tfe <- ReturnSmoothed(tfe,period)
point.colors=FALSE
plot.errors=TRUE
plotLightCurve(tfe,xLabel="Phase",
               maintitle=period,point.colors=point.colors,
               plot.errors=plot.errors)

dev.new()
tfe.new <- tfe[(tfe[,1] > 3500) & (tfe[,1] < 4000),]
plotLightCurve(tfe.new,xLabel="Phase",
               maintitle=period,point.colors=point.colors,
               plot.errors=plot.errors)
dev.new()
tfe.new <- tfe[(tfe[,1] > 1500) & (tfe[,1] < 2500),]
plotLightCurve(tfe.new,xLabel="Phase",
               maintitle=period,point.colors=point.colors,
               plot.errors=plot.errors)

## enter tfe, period1, period2
filenames <- dir(path="../../data/ogle_near_eclipse",
                 full.names=TRUE)
filenames

i <- 0

i <- i + 1
tfe <- read.table(filenames[i])
point.colors=FALSE
plot.errors=TRUE
plotLightCurve(tfe,xLabel="Phase",
               maintitle=filenames[i],
               point.colors=point.colors,
               plot.errors=plot.errors)
abline(v=2100,col='red',lwd=2)


ReturnSmoothed <- function(tfe,
                           period){ 
  times_orig <- tfe[,1]
  tfe[,1] = tfe[,1] - min(tfe[,1])  
  tfe[,1] = (tfe[,1] %% period) / period
  line.smu = supsmu(tfe[,1],tfe[,2],periodic=TRUE)
  tfe[,2] <- (tfe[,2] - line.smu$y[rank(tfe[,1])])
  tfe[,1] <- times_orig
  return(tfe)
}



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


summary((1/df2$freq1_harmonics_freq_0[df2$classification=="eclipsing"]))


unique(df2$classification)

plot(density(1/df2$freq1_harmonics_freq_0[df2$classification=="eclipsing"]))

temp <- df2[df2$classification=="eclipsing",]
fnames <- temp$filename[order(1/temp$freq1_harmonics_freq_0,
                              decreasing=TRUE)]

