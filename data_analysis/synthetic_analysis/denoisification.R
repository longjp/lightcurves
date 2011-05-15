########## 
########## 
########## GENERAL DENOISIFICATION FUNCTIONS 
##########   
##########
########## by James Long 
########## date: MAY 14, 2011 
########## 


## get the correctly noisified training data
Denoisification = function(train.clean,
                   train.noisy,
                   test.noisy,
                   imp.variables,
                   DenoiseMethod=DenoiseRF,
                   return.all=FALSE){

  ## CHECKS!!!
  if(!identical(train.noisy$sources.original_source_id,
            train.clean$sources.original_source_id)){
    print("clean training and temp training don't match")
    return(0)
  }
  if(length(unique(train.noisy$features.n_points)) != 1){
    print("noisified training data is hetergeneous, diff n_points")
    return(0)
  }
  if(length(unique(test.noisy$features.n_points)) != 1){
    print("test data is hetergeneous, diff n_points")
    return(0)
  }

  ## return everything in output, use names
  ## to add objects i.e. output$new_obj_name = new_obj
  output = list()

  ## convert data frames to matrices so things work fast
  train.clean.m = as.matrix(train.clean[,imp.variables])
  train.noisy.m = as.matrix(train.noisy[,imp.variables])
  test.noisy.m = as.matrix(test.noisy[,imp.variables])

  ## :::DENOISING:::
  ## using function DenoiseMethod (default DenoiseRF)
  ## TODO: name elements of denoise data, this will
  ## allow different functions to access by name
  denoise.data = list(train.clean.m,
    train.noisy.m,
    test.noisy.m,
    imp.variables)
  denoise.results = DenoiseMethod(denoise.data)

  ## do we return all the elements of denoising?
  if(return.all) output$denoise.results = denoise.results
  
  ## construct rf on clean data and get probabilities
  rf_formula = GetFormula()
  rf.clean = randomForest(rf_formula,data=train.clean)
  pzx = predict(rf.clean,type='vote')
  class.names = colnames(pzx)

  ## use pyx (in denoise.results$pyx) and pzx
  ## to predict response
  predictions = class.names[apply(denoise.results$pyx,2,function(x)
    {which.max(apply(pzx,2,function(y){sum(x*y)}))})]

  ## determine error rate, attach to output, return
  result = mean(predictions !=
           test.noisy$sources.classification)
  output$error = result
  return(output)
}



##
## select the correct training and test
## data for denoisification
PrepareDataForDenoise = function(i){
  ## get the correct training data
  data1train.temp = subset(data1train,
    features.n_points == points.levels[i] &
    row_id==0 & !contains.random)
  data1train.temp = data1train.temp[
    order(data1train.temp$sources.original_source_id),]

  ## get the correct test set
  data1test.temp = subset(data1,
    sources.survey=='test' & features.n_points == points.levels[i])
  return(list(data1train.temp,data1test.temp))
}




