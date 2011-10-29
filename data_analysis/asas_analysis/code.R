########## 
########## 
########## ANALYZE FEATURE DISTRIBUTIONS FOR CLASSES IN ASAS 
##########   
##########
########## by James Long 
########## date: 9/21/2011 
########## 


library('foreign')

## load asas
features = '../../data_processed/source_feats_asas.arff'
data1asas = read.arff(features)
names(data1asas)
table(data1asas$class)

## load the hipparcos sources
features = '../../data_processed/hip_train_three_class.dat'
data1hip = read.table(features,sep=';',header=TRUE)
names(data1hip)
table(data1hip)

## load ogle
features = '../../data_processed/ogleIIIall.dat'
data1ogle = read.table(features,sep=';',header=TRUE)

## identify suspicous ogle cepheids
data1ogle$sources.xml_filename = as.character(data1ogle$sources.xml_filename)
xml_filename = data1ogle$sources.xml_filename
xml_filename[grepl('rr-ab',xml_filename)][1]
xml_filename[grepl('mira',xml_filename)][1]
xml_filename[grepl('classical-cepheid',xml_filename)][1]
xml_filename = sub('../data/OGLEIII/mira/','',xml_filename)
xml_filename = sub('../data/OGLEIII/rr-ab/','',xml_filename)
xml_filename = sub('../data/OGLEIII/classical-cepheid/',
  '',xml_filename)
xml_filename = sub('.dat','',xml_filename)
data1ogle$sources.xml_filename = xml_filename

cepheid.types =
  read.table('../../data_processed/cepheid_types.txt')
head(cepheid.types)
class(cepheid.types[,1])
cepheid.fundamental = cepheid.types[cepheid.types[,4]=='F',1]
length(cepheid.fundamental)
cepheid.fundamental = as.character(cepheid.fundamental)
cepheid.fundamental
suspicious = ((data1ogle$sources.classification
               == "Classical Cepheid") &
              !(data1ogle$sources.xml_filename %in%
                cepheid.fundamental))
sum(suspicious)
sum(data1ogle$sources.classification == "Classical Cepheid")




cepheids.asas = data1asas$class == "Classical Cepheid"
cepheids.ogle = (data1ogle$sources.classification ==
                 "Classical Cepheid")
sum(cepheids.asas)
sum(cepheids.ogle)

plot(log(data1ogle$features.freq1_harmonics_freq_0[cepheids.ogle]),
     log(data1ogle$features.amplitude[cepheids.ogle]),
     col='#00000040',xlim=c(-4,2),ylim=c(-4,0))
points(log(data1ogle$features.freq1_harmonics_freq_0[suspicious]),
     log(data1ogle$features.amplitude[suspicious]),col='blue',
       pch='x')





sum(cepheids.ogle&!suspicious)

pdf('cepheids_three_surveys.pdf')
plot(log(data1ogle$features.freq1_harmonics_freq_0[cepheids.ogle&!suspicious]),log(data1ogle$features.amplitude[cepheids.ogle&!suspicious]),col='#00000040',xlim=c(-4.5,0),ylim=c(-3.5,.5),
     xlab="log(frequency)",ylab="log(amplitude)",
     main="Classical Cepheids",pch='o')

sum(cepheids.asas)
points(log(data1asas$freq1_harmonics_freq_0[cepheids.asas]),
       log(data1asas$amplitude[cepheids.asas]),
       col='blue',pch='a',lwd=2)

table(data1hip$sources.classification)
to_use = (data1hip$sources.classification ==
          "Classical Cepheid" &
          data1hip$sources.original_source_id ==
          data1hip$features.source_id)
sum(to_use)
points(log(data1hip$features.freq1_harmonics_freq_0[to_use]),
       log(data1hip$features.amplitude[to_use]),
       col='orange',pch='h',lwd=2)
legend('bottomleft',c('ogle','asas','hip'),pch=c('o','a','h'),
       col=c('black','blue','orange'))
dev.off()


names(data1asas)
