########## 
########## 
########## FUNCTIONS APPLYING CLASSIFIER CONSTRUCTED ON HIPPARCOS
########## TO OGLE SOURCES
##########
########## by James Long 
########## date: 9/16/2011
########## 


GenerateClassifiers = function(data1,points.levels,rf_features,rf_formula){
  rfClassifiers = list()
  for(i in 1:length(points.levels)){
    print(i)
    rfClassifiers[[i]] = list()
    for(j in 1:5){
      data1.temp = subset(data1,(features.n_points == points.levels[i] &
        !contains.random & row_id == (j-1) & !is_original))
      print(nrow(data1.temp))
      data1.temp[,rf_features] = na.roughfix(data1.temp[,rf_features])
      data1.temp = RemoveInfinities(data1.temp)
      rfClassifiers[[i]][[j]] = randomForest(rf_formula,data=data1.temp)
    }
  }
  return(rfClassifiers)
}


GetNoisified = function(data1,cadence){
  data1 = data1[grepl(cadence,data1$sources.noise_args),]
  data1$sources.original_source_id[grepl("all",data1$sources.noise_args)] = data1$features.source_id[grepl("all",data1$sources.noise_args)]
  data1$is_original = FALSE
  data1$is_original[data1$sources.original_source_id == data1$features.source_id] = TRUE
  sum(data1$is_original)
  data1$contains.random = grepl("random",data1$sources.noise_args)
  sum(data1$contains.random)
  data1 = dedupe(data1,c("features.n_points","sources.original_source_id",
    "contains.random","is_original"))
  data1 = subset(data1,!is_original & !contains.random)
  return(data1)
}



GetPredictions = function(data1,points.levels,number.classifiers,
  number.classes,class.names,class.ratios){
  closest.classifiers = vapply(data1$features.n_points,
    function(x){which.min(abs(points.levels - x))},c(0))
  results = array(0,dim=c(length(points.levels),number.classifiers,nrow(data1),number.classes))
  for(i in 1:dim(results)[1]){
    print(i)
    for(j in 1:dim(results)[2]){
      results[i,j,,] = predict(rfClassifiers[[i]][[j]],newdata=data1,type='prob')
    }
  }
  closest = cbind(closest.classifiers,1:length(closest.classifiers))
  results.for.points = apply(closest,1,function(x){ which.max(class.ratios*colMeans(results[x[1],,x[2],])) } )
  predictions = class.names[results.for.points]
  return(predictions)
}

