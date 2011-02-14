#######
####### analyze error from debosscher data
#######
####### by James Long
####### created Feb 13, 2011    last modified Feb 13, 2011
#######


# get the data
tfe = read.table("tfe.txt",sep=";",header=TRUE)
classes = read.table("class_output.txt",sep=";")
names(classes) = c("source_id","classification")
data1 = merge(tfe,classes,by=c("source_id"))



function_on_curves = function(curve_of_interest,num_flux_measurements){
  result = rep(0,length(num_flux_measurements))
  curve_of_interest = curve_of_interest[order(curve_of_interest$time),]
  for(i in 1:length(result)){
    result[i] = skew(curve_of_interest$flux[1:num_flux_measurements[i]])
  }
  return(result)
}

skew = function(x){
  return((sum((x - mean(x))^3) / (sd(x) * (length(x) - 1) / length(x))^3) / length(x))
}



curves = data1
num_flux_measurements = 10 + ((0:18) * 5)
results = matrix(0,nrow=length(unique(curves$source_id)),ncol=length(num_flux_measurements))

source_ids = unique(curves$source_id)
rownames(results) = source_ids

truth = rep(0,length(source_ids))

for(i in 1:length(source_ids)){
  curve_of_interest = subset(curves,subset=source_id==source_ids[i])
  truth[i] = skew(curve_of_interest$flux)
  results[i,] = function_on_curves(curve_of_interest,num_flux_measurements)
}  


difference = results - truth

d1 = density(difference[,1])
d2 = density(difference[,2])
dlast = density(difference[,-1])
plot(d1)
lines(d2)
lines(dlast)


skew(rnorm(10000))
