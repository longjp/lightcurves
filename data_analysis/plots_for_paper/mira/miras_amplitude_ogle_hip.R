########## 
########## 
########## DETERMINE DIFFERENCE IN MIRA AMP. DISTRIBUTIONS 
########## BETWEEN OGLE AND HIPPARCOS 
##########
########## by James Long 
########## date: DEC 13, 2011 
########## 

source("~/Rmodules/Rfunctions.R")
library("randomForest")
library("rpart")
options(width=50)
Tables = fileOutLoc('')
graphics = fileOutLoc('')


ogle_name = c("Mira","RR Lyrae AB","Classical Cepheid")
hip_name = c("Mira","RR Lyrae, Fundamental Mode","Classical Cepheid")
name_conversion = cbind(ogle_name,hip_name)
name_conversion

## load the OGLE source
features = '../../../data_processed/ogleIIIall-fund.dat'
tfe = '../../../data_processed/ogleIIIall-fund-tfe.dat'
data1ogle = read.table(features,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe,sep=';',header=TRUE)
nrow(data1ogle)

## load the hipparcos sources
features = '../../../data_processed/hip_train_three_class.dat'
tfe = '../../../data_processed/hip_train_three_class_tfe.dat'
data1hip = read.table(features,sep=';',header=TRUE)
time_flux_hip = read.table(tfe,sep=';',header=TRUE)

## get rid of several classes
nrow(data1hip)
data1hip = subset(data1hip,
  sources.classification %in% name_conversion[,"hip_name"])
data1hip = subset(data1hip,features.source_id == sources.original_source_id)
nrow(data1hip)

## change the names to match ogle
sources = as.character(data1hip$sources.classification)
sources = name_conversion[match(sources,name_conversion[,"hip_name"]),"ogle_name"]
data1hip$sources.classification = as.factor(sources)
table(data1hip$sources.classification)

## remove infinities from ogle sources
data1ogle = na.roughfix(data1ogle)
data1ogle = RemoveInfinities(data1ogle)



###
### select largest amplitude sources in hip, see if truncated
###
nrow(data1hip)
data1hip.mira = subset(data1hip,sources.classification=="Mira")
nrow(data1hip.mira)
ordered_ids = data1hip.mira$sources.original_source_id[
  order(data1hip.mira$features.amplitude,decreasing=TRUE)]
ordered_ids[1]
length(ordered_ids)
data1hip.mira$features.amplitude[data1hip.mira$sources.original_source_id==ordered_ids[1]]

i = i + 1
DrawThreeLightCurves(ordered_ids[i],data1hip,time_flux_hip)
data1hip.mira$features.amplitude[data1hip.mira$sources.original_source_id==ordered_ids[i]] * 2

d1 = density(time_flux_hip$flux)
pdf('hipparcos_flux_density.pdf')
plot(d1,main="Density of mag of flux measurement in Hipparcos",
     lwd=3,xlab="flux in mag")
dev.off()



## look at ogle miras
nrow(data1ogle)
data1ogle.mira = subset(data1ogle,sources.classification=="Mira")
nrow(data1ogle.mira)
ordered_ids = data1ogle.mira$sources.original_source_id[
  order(data1ogle.mira$features.amplitude,decreasing=TRUE)]
ordered_ids[1]
length(ordered_ids)
data1ogle.mira$features.amplitude[data1ogle.mira$sources.original_source_id==ordered_ids[1]]


i = i + 1
DrawThreeLightCurves(ordered_ids[i],data1ogle,time_flux_ogle)
data1ogle.mira$features.amplitude[data1ogle.mira$sources.original_source_id==ordered_ids[i]] * 2


timediff = function(x){
  return(max(x) - min(x))
}

diffs = aggregate(time_flux_ogle$time,list(source_id=time_flux_ogle$source_id),timediff)
head(diffs)
mean(diffs[,2])
names(diffs) = c("sources.original_source_id","time_diff")
data1ogle = merge(data1ogle,diffs)

diffs = aggregate(time_flux_hip$time,list(source_id=time_flux_hip$source_id),timediff)
head(diffs)
mean(diffs[,2])
class(diffs)
names(diffs) = c("sources.original_source_id","time_diff")
data1hip = merge(data1hip,diffs)

dev.new()
to_plot = data1ogle$sources.classification == "Mira"
plot(data1ogle$time_diff[to_plot],
     data1ogle$features.amplitude[to_plot],
     xlab="Length of Observing Time",
     ylab="Amplitude",col='orange')
median(data1ogle$time_diff[to_plot])
to_plot = data1hip$sources.classification == "Mira"
points(data1hip$time_diff[to_plot],
       data1hip$features.amplitude[to_plot],
       col='blue')
median(data1hip$time_diff[to_plot])


head(data1ogle)

###
### TODO: Turn into one plot with scatter on top of 
###       density
###

flux.error.ogle = aggregate(time_flux_ogle$error,
  by=list(time_flux_ogle$source_id),mean)
data1ogle = merge(data1ogle,flux.error.ogle,
  by.x="features.source_id",by.y="Group.1")
flux.error.hip = aggregate(time_flux_hip$error,
  by=list(time_flux_hip$source_id),mean)
data1hip = merge(data1hip,flux.error.hip,
  by.x="features.source_id",by.y="Group.1")


feature = "features.amplitude"
amps = c(data1hip[data1hip$sources.classification=="Mira",
  feature],data1ogle[data1ogle$sources.classification==
                     "Mira",feature])
classes = c(rep("hip",
  sum(data1hip$sources.classification=="Mira")),
  rep("ogle",sum(data1ogle$sources.classification=="Mira")))
length(amps)


pdf('amplitude_miras.pdf',width=6,height=5.7)
par(mar=c(4,4,.5,1))
DrawKDES(amps,classes,
         ylab="Density",xlab='amplitude (mags)',
         density.colors=c(4,1),location='topright')
dev.off()


to_use_ogle = (data1ogle$sources.classification == "Mira" &
               data1ogle$features.amplitude < 6)
to_use_hip = (data1hip$sources.original_source_id ==
              data1hip$features.source_id &
              data1hip$sources.classification == "Mira")
sum(to_use_ogle)
sum(to_use_hip)

pdf('amplitude_vs_fluxnoise_miras.pdf',width=6,heigh=5.7)
par(mar=c(4,4,.5,1))
x = (c(log(data1ogle$x[to_use_ogle]),
       log(data1hip$x[to_use_hip])))
y = (c(data1ogle$features.amplitude[to_use_ogle],
       data1hip$features.amplitude[to_use_hip]))
pchs = c(rep(1,sum(to_use_ogle)),rep(2,sum(to_use_hip)))
col1 = (c(rep('#00000040',sum(to_use_ogle)),
          rep(4,sum(to_use_hip))))
random = 1:length(x)
plot(y[random],x[random],pch=pchs[random],
     col=col1[random],xlab="amplitude (mags)",
     ylab="log(mean photometric error)")
legend("topleft",c("hip","ogle"),col=c(4,1),
       pch=c(2,1),cex=1)
dev.off()




