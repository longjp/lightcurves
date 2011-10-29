########## 
########## 
########## CADENCE COMPARISON 
##########  
##########
########## by James Long 
########## date: 7/28/2011 
########## 


rm(list=ls(all=TRUE))
set.seed(22071985)

## get the data

tfe.hipparcos = '../../data_processed/hipparcos/tfe00001.dat'
tfe.ogle = '../../data_processed/ogleIII-tfe.dat'
time_flux_hipparcos = read.table(tfe.hipparcos,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe.ogle,sep=';',header=TRUE)

## only keep time and source id
time_flux_ogle = time_flux_ogle[,c(5,2)]
time_flux_hipparcos = time_flux_hipparcos[,c(5,2)]


## need to eliminate most of the hipparcos sources
## because they don't belong to classes of interest
features.hipparcos = '../../data_processed/hipparcos/sources00001.dat'
data1 = read.table(features.hipparcos,sep=';',header=TRUE)
hip_name = c("Mira","RR Lyrae, Fundamental Mode","Classical Cepheid")
data1 = data1[data1$features.source_id == data1$sources.original_source_id,]
sources = data1$features.source_id[data1$sources.classification %in% hip_name]
sources
head(time_flux_hipparcos)
time_flux_hipparcos = time_flux_hipparcos[time_flux_hipparcos$source_id %in% sources,]
nrow(time_flux_hipparcos)


## function should take two columns, identifiers and then times
MeanNumberMeasurements = function(x){
  number.measurements = aggregate(x$time,list(source_id_num_flux=x$source_id),length)
  return(number.measurements[,2])
}

  
plot(density(MeanNumberMeasurements(time_flux_ogle)))
plot(density(MeanNumberMeasurements(time_flux_hipparcos)))


CurveDiffs = function(x){
  x = x[order(x)]
  x = x[2:length(x)] - x[1:(length(x) - 1)]
  return(x)
}
a = rnorm(10)
a
CurveDiffs(a)
a[order(a)]

TotalDiffs = function(x){
  number_time_diffs = nrow(x) - length(unique(x[,1]))
  time_diffs = rep(0,number_time_diffs)
  source_ids = unique(x[,1])
  j = 1
  for(i in source_ids){
    differences = CurveDiffs(x[x[,1] == i,2])
    time_diffs[j:(j + length(differences) - 1)] = differences
    j = j + length(differences)
  }
  return(time_diffs)
}

  
times = TotalDiffs(time_flux_ogle)


sum(times > 100) / length(times)
pdf('ogle_cadence.pdf')
plot(density(times[times<100]),main="ogle time difference, 90% of data",xlab="time diff (days)")
dev.off()


times = TotalDiffs(time_flux_hipparcos)
plot(density(times))
plot(density(times[times<50]))


max_time = 1
sum(times > max_time) / length(times)
plot(density(times[times<max_time]))


sum(times < .5) / length(times)

###
### in contrast hipparcos views are often far apart
pdf('hipparcos_cadence.pdf')
plot(density(times[times<.5]),main="hipparcos time differences, 75% of data",xlab="time diff (days)")
dev.off()




## for ogle 15% of obs are follow by another obs within 12 hours, for hipparcos 76%
