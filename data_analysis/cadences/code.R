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
tfe.ogle = '../../data_processed/OGLE/tfe00001.dat'
time_flux_hipparcos = read.table(tfe.hipparcos,sep=';',header=TRUE)
time_flux_ogle = read.table(tfe.ogle,sep=';',header=TRUE)

##
time_flux_ogle = time_flux_ogle[,c(5,2)]
time_flux_hipparcos = time_flux_hipparcos[,c(5,2)]


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
sum(times > 10) / length(times)



###
### 96% of OGLE views are within 10 days of each other, often 1 day apart
pdf('ogle_cadence.pdf')
plot(density(times[times<10]),main="ogle time difference, 96% of data",xlab="time diff (days)")
dev.off()

sum(times < .5) / length(times)



times = TotalDiffs(time_flux_hipparcos)
plot(density(times))
plot(density(times[times<50]))
sum(times > 10) / length(times)
plot(density(times[times<10]))


sum(times < .5) / length(times)

###
### in contrast hipparcos views are often far apart
pdf('hipparcos_cadence.pdf')
plot(density(times[times<.5]),main="hipparcos time differences, 75% of data",xlab="time diff (days)")
dev.off()




## for ogle 15% of obs are follow by another obs within 12 hours, for hipparcos 76%
