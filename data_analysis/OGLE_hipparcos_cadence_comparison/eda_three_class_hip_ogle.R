########## 
########## 
########## COMPARE DISTRIBUTIONS OF FEATURES FOR LIGHTCURVES
########## FROM OGLE AND HIPPARCOS
##########
########## by James Long 
########## date: 8/22/2011
########## 


###
### RIGHT NOW IT IS UNCLEAR WHAT THIS FILE IS FOR / WHY NECESSARY
### WE COULD JUST DO ALL OF EDA IN classify_hip_ogle_real.R
### 

## load some functions
source("~/Rmodules/Rfunctions.R")
source("functions.R")
library("randomForest")
library("rpart")

Tables = fileOutLoc('tables/')
graphics = fileOutLoc('graphics/')

ogle_name = c("Mira","RR Lyrae AB","Classical Cepheid")
hip_name = c("Mira","RR Lyrae, Fundamental Mode",
  "Classical Cepheid")
name_conversion = cbind(ogle_name,hip_name)
name_conversion

## load the OGLE source
features = '../../data_processed/ogleIIIall-fund.dat'
tfe = '../../data_processed/ogleIIIall-fund-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)
nrow(data1ogle)
table(data1ogle$sources.classification)

## load the hipparcos sources
features = '../../data_processed/hip_train_three_class.dat'
tfe = '../../data_processed/hip_train_three_class_tfe.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)

## get rid of several classes
nrow(data1hip)
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle$sources.xml_filename = as.factor(
  data1ogle$sources.xml_filename)
data1ogle = na.roughfix(data1ogle)
data1ogle$sources.xml_filename = as.character(
  data1ogle$sources.xml_filename)
data1ogle = RemoveInfinities(data1ogle)




to_use = ((data1ogle$sources.classification ==
           "Classical Cepheid") &
          (runif(nrow(data1ogle)) < 1))
sum(to_use)
plot(data1ogle[to_use,feature],
     Ffeature(data1ogle$features.freq1_harmonics_freq_0[to_use]),
     xlab="fold2P90percentile",ylab="frequency")
to_use_hip = ((data1hip$sources.original_source_id ==
               data1hip$features.source_id) &
              data1hip$sources.classification == "RR Lyrae AB")
sum(to_use_hip)
points(data1hip[to_use_hip,feature],
       Ffeature(data1hip$features.freq1_harmonics_freq_0[
                    to_use_hip]),col='blue',pch=2)
legend('topright',c('ogle','hip'),col=c('black','blue'),
       pch=c(1,2))
abline(h=1)
