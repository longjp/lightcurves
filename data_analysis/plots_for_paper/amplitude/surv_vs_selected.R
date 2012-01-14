########## 
########## 
########## HOW DO AMPLITUDES CHANGE IN SURVEY AND  
########## SELECTED HIPPARCOS SOURCES 
##########
########## by James Long 
########## date: 1/13/2011 
########## 

source('~/Rmodules/Rfunctions.R')
library('randomForest')
library('rpart')
options(width=50)

## get tfes and our derived features
features = '../../../data_processed/hipparcos_dubath_sources.dat'
tfe = '../../../data_processed/hipparcos_dubath_tfe.dat'
data1 = read.table(features,sep=';',header=TRUE)


## find out whether source is from survey or selected in
## Hipparcos
survey_or_selected = read.table('../../../data_processed/survey_or_selected')
sourcesHIPnames = paste("HIP",paste(as.character(survey_or_selected[,1]),'.xml',sep=""),sep="")
sourcesHIPnames[1:10]
survey_or_selected[,1] = sourcesHIPnames
survey_or_selected = survey_or_selected[survey_or_selected[,1] %in% as.character(data1$sources.xml_filename),]
nrow(survey_or_selected)
names(survey_or_selected) = c("sources.xml_filename","sORs")
data1 = merge(data1,survey_or_selected)




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







## get dubath features and classes
filename = "../../hipparcos_dubath/dubath_features"
dubath.features = read.table(filename,sep="&",header=TRUE)
table(dubath.features$class)
dubath.names = levels(dubath.features$class)
dubath.classes = as.character(dubath.features$class)
dubath.names
#our.names = c("Alpha-2 Canum Venaticorum","Alpha Cygni","Beta Cephei","BE+GCAS","Multiple Mode Cepheid","W Virginus A","W Virginus B","Delta Cepheid","Delta Cepheid First Overtone","Delta Scuti","Delta Scuti Low Amplitude","Algol (Beta Persei)","Beta Lyrae","Ellipsoidal","W Ursae Majoris","Gamma Doradus","Long Period Variable","RR Lyrae AB","RR Lyrae C","RS+BY","RV Tauri","Slowly Pulsating B Star","SX Areitas")
name.associations = cbind(dubath.names,dubath.names)
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









## to_use = (data1$sources.classification == "Algol (Beta Persei)" & data1$sources.original_source_id == data1$features.source_id)
## sum(to_use)
## sum(to_use & data1$sORs == "Survey")

## pdf("beta_persei_hip.pdf")
## DrawKDES(log(data1$features.amplitude[to_use],base=10),
##          data1$sORs[to_use],
##          main.title="",xlab="log(amplitude) (mags)",
##          density.colors=c("blue","black"),cex.lab=1.4,
##          line.width=4)
## dev.off()


data1dubath = data1[data1$sources.original_source_id == data1$features.source_id,]
nrow(data1dubath)



source('~/Rmodules/Rfunctions.R')
boxplot(data1dubath$features.amplitude~data1dubath$sources.classification)

## get largest sources
d1 = names(table(data1dubath$sources.classification)[table(data1dubath$sources.classification) > 50])
data1dubath = subset(data1dubath,sources.classification %in% d1)
nrow(data1dubath)
data1dubath$sources.classification = as.factor(as.character(
  data1dubath$sources.classification))
boxplot(data1dubath$features.amplitude~data1dubath$sources.classification)
names(data1dubath)
class(data1dubath$sORs)


data1dubath$crossed = as.factor(paste(abbreviate(as.character(data1dubath$sources.classification)),abbreviate(as.character(data1dubath$sORs)),sep=""))

boxplot(log(data1dubath$features.amplitude,base=10)~data1dubath$crossed,las=2,col=rep(c(1,2),length(levels(data1dubath$sources.classification))))



levels(data1dubath$sORs) = c("sel","sur")
#levels(data1dubath$sources.classification) = c("Algol","A2CV","BL","DCeph","DScuti","LPV","RRLyrAB","B Star","WU Maj")


pdf("amplitude_survey_selected.pdf")
par(mar=c(4.3,5.1,1,1))
boxplot.n(log(data1dubath$features.amplitude,base=10)~data1dubath$sORs*data1dubath$sources.classification,horizontal=TRUE,col=rep(c("orange","blue"),length(levels(data1dubath$sources.classification))),xlab="log(amplitude)",names=FALSE,axes=FALSE,frame.plot=TRUE,cex.lab=1.4)
horizontal = .5 + 2*(1:8)
abline(h=horizontal,col='grey')
axis(1,cex.lab=1.4,main=1.4)
horizontal = -.5 + 2*(1:9)
axis(2,at=horizontal,labels=levels(data1dubath$sources.classification),las=2,cex=1.4)
legend("topright",c("Survey","Selected"),col=c("blue","orange"),
       lwd=4,cex=1,title="Source")
dev.off()

table(data1dubath$features.amplitude,data1dubath$sources.classification)
