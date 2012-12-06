## for reading in all derived feature files from RR
##
## by James Long
## date December 6, 2012
##


options(width=50)
library('randomForest')

## columns we will use in analysis
columns <- c("freq1_harmonics_freq_0","p2p_scatter_2praw","skew","amplitude","flux_percentile_ratio_mid20","flux_percentile_ratio_mid80","flux_percentile_ratio_mid35","classification","filename")


files <- list.files("../../data_processed/eclipse/",
                    full.names=TRUE)

## setup dataframe
n_files <- length(files)
d1 <- files[1]
mylist <- strsplit(readLines(d1)," ")
var.names <- unlist(lapply(mylist,function(x) {x[1]}))
values <- unlist(lapply(mylist,function(x) {x[2]}))
df1 <- as.data.frame((matrix(rep(NA,n_files*(length(var.names) +
                                             1)),nrow=n_files)))
names(df1) <- c(var.names,"filename")


## load df1 with observations
for(i in 1:length(files)){
  d1 <- file(files[i])
  mylist <- strsplit(readLines(d1)," ")
  close(d1)
  var.names <- unlist(lapply(mylist,function(x) {x[1]}))
  values <- unlist(lapply(mylist,function(x) {x[2]}))
  df1[i,] <- c(values,files[i])
}




## convert variables from characters to doubles
summary(df1)
to_convert <- names(df1)[1:(length(df1)-1)]
for(i in to_convert){
  df1[,i] <- as.double(df1[,i])
}

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
plot(df2$eclipsing_prob,rnorm(nrow(df2),sd=.2) + as.numeric(df2$classification),col=df2$classification)



## write function to accept filename and then find + plot l.c.
