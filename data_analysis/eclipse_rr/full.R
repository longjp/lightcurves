## for reading in all derived feature files from RR
##
## by James Long
## date December 6, 2012
##


options(width=50)
library('randomForest')

## columns we will use in analysis
columns <- c("freq1_harmonics_freq_0","p2p_scatter_2praw","skew","amplitude","flux_percentile_ratio_mid20","flux_percentile_ratio_mid80","flux_percentile_ratio_mid35","classification","filename")



df1 <- read.table("../../data_processed/eclipse1.dat",
                  header=TRUE)
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

preds <- predict(rf.fit,type="prob")
df2$eclipsing_prob <- predict(rf.fit,type="prob")[,"eclipsing"]
plot(df2$eclipsing_prob,rnorm(nrow(df2),sd=.1) + as.numeric(df2$classification),col=df2$classification)



## write function to accept filename and then find + plot l.c.
filenames <- df2$filename[order(df2$eclipsing_prob - 1*(df2$classification=="eclipsing"),decreasing=TRUE)]
filenames <- as.character(filenames)

filenames <- gsub("../data_processed/eclipse/","",filenames)
filenames[1]
filenames[2]

tfe <- read.table(paste("../../data/ogle-rr-i/",filenames[1],sep=""))
tfe


freq2_harmonics_freq_0
tfe 



## enter tfe, period1, period2




DrawEclipsingRR <- function(tfe,
                            period,
                            period2,
                            smoother=TRUE,
                            point.colors=FALSE,
                            plot.unfolded=TRUE,
                            plot.folded=TRUE,
                            plot.folded.twice=TRUE,
                            par=TRUE,
                            plot.errors=TRUE){
  ## if source id is a list, grab a random l.c. to plot
  print(source_to_plot)
  if(par){
    par(mfcol=c(3,2))
  }

  times_orig <- tfe[,1]
  for(jj in c(1,2)){
    ## plot the raw light curve
    plotLightCurve(tfe,
                   point.colors=point.colors,
                   plot.errors=plot.errors)
    
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


