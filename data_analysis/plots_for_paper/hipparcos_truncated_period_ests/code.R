########## 
########## 
########## HOW DO PERIOD ESTIMATES CHANGE IF WE ONLY USE 
########## FIRST YEAR OF HIPPAROCS OPERATION 
##########
########## by James Long 
########## date: 12/10/2011 
########## 

####
#### NOTE: code here is awkward / bad due to poor data 
#### collection. all the feature information is in
#### hipparocs_dubath_sources.dat but because i didn't
#### get class information correct there, we have to
#### load and merge with dubath_features file. but
#### the dubath features are never actually used
#### 12/10/2011
####

## setup program
rm(list=ls(all=TRUE))
set.seed(22071985)

source('~/Rmodules/Rfunctions.R')
library('randomForest')
library('rpart')
options(width=50)

## get tfes and our derived features
features = '../../../data_processed/hipparcos_dubath_sources.dat'
tfe = '../../../data_processed/hipparcos_dubath_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)
data1$features.freq1_harmonics_freq_0 = as.numeric(as.character(data1$features.freq1_harmonics_freq_0))
data1$features.freq1_harmonics_freq_0[(is.na(data1$features.freq1_harmonics_freq_0))] = 10^10
data1$features.freq1_harmonics_freq_0 = as.numeric(as.character(data1$features.freq1_harmonics_freq_0))
time_flux = read.table(tfe,sep=';',header=TRUE)
data1$sources.classification = NULL
xml_filenames = data1$sources.xml_filename
xml_filenames
xml_filenames = sub("HIP","",xml_filenames)
xml_filenames = sub(".xml","",xml_filenames)
xml_filenames = as.numeric(xml_filenames)
data1$sources.xml_filename = xml_filenames
nrow(data1)
names(data1)
table(data1$sources.classification)

## ## get dubath features and classes
filename = "../../../data_processed/dubath_features"
dubath.features = read.table(filename,sep="&",header=TRUE)
table(dubath.features$class)
dubath.names = levels(dubath.features$class)
dubath.classes = as.character(dubath.features$class)
dubath.names
our.names = c("Alpha-2 CV","Alpha Cygni","Beta Cephei","BE+GCAS","Multiple Mode Cepheid","W Virginus A","W Virginus B","Delta Cepheid","Delta Cepheid First Overtone","Delta Scuti","Delta Scuti Low Amplitude","Algol (Beta Persei)","Beta Lyrae","Ellipsoidal","W Ursae Majoris","Gamma Doradus","Long Period Variable","RR Lyrae AB","RR Lyrae C","RS+BY","RV Tauri","Slowly Pulsating B Star","SX Areitas")
name.associations = cbind(dubath.names,our.names)
name.associations
for(i in 1:nrow(name.associations)){
  dubath.classes[dubath.classes == name.associations[i,1]] = name.associations[i,2]
}
dubath.features$class = as.factor(dubath.classes)
names(dubath.features)
dubath.features.classid = dubath.features[,c(1,2)]
names(dubath.features.classid) = c("sources.xml_filename","sources.classification")
data1 = merge(data1,dubath.features.classid)
nrow(data1)
table(data1$sources.classification)




####
#### check how frequency is different for noisy sources
####

table(data1$sources.classification)
classes = c("Slowly Pulsating B Star","Long Period Variable",
  "Alpha-2 CV")
feature = "features.freq1_harmonics_freq_0"
to_use = (data1$sources.classification %in% classes &
          data1$sources.noisification == "identity")
sum(to_use)
pdf('frequency_hipparcos_full.pdf',width=6,height=5.7)
par(mar=c(4.2,4.1,.3,.2))
DrawKDES(log(data1[to_use,feature],base=10),
         data1$sources.classification[to_use],
         xlab="log(frequency)",
         location='topleft',
         density.colors=c('black','orange','blue'),
         line.width=4,label.size=1.5,
         ylimits=c(0,3))
dev.off()




##
sum(time_flux$source_id %in% data1$sources.original_source_id[to_use])
time_flux.temp = time_flux[time_flux$source_id %in% data1$sources.original_source_id[to_use],]
a = aggregate(time_flux.temp$time,by=list(time_flux.temp$source_id),length)
a = a[,2]
a
median(a)

short_curves = time_flux.temp[time_flux.temp$time < min(time_flux.temp$time) + 365,]
a = aggregate(short_curves$time,by=list(short_curves$source_id),length)
a = a[,2]
a
median(a)




to_use = (data1$sources.classification %in% classes &
          data1$sources.noisification != "identity")
sum(to_use)
pdf('frequency_hipparcos_365.pdf',width=6,height=5.7)
par(mar=c(4.2,4.1,.3,.2))
DrawKDES(log(data1[to_use,feature],base=10),
         data1$sources.classification[to_use],
         xlab="log(frequency)",
         location='topleft',
         density.colors=c('black','orange','blue'),
         line.width=4,label.size=1.5,
         ylimits=c(0,1.5))
dev.off()

