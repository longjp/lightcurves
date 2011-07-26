########## 
########## 
########## GENERAL DENOISIFICATION FUNCTIONS 
##########   
##########
########## by James Long 
########## date: MAY 14, 2011 
########## 

## Runs the denoisification process
## 1. DenoiseMethod(data.obj) return list containing pyx
## 2. pzx is constructed using train.clean
## 3. pzx and pyx combined with test.class gives error rate
## 4. return.all determines info returned by DenoiseMethod is
##    returned by Denoisification or just error
## 5. rf_formula is formula for constructing pzx, defaults to GetFormula()
Denoisification = function(train.clean,
                           test.class,
                           data.obj,
                           DenoiseMethod=DenoiseRF,
                           return.all=FALSE,
                           rf_formula=NULL){

  ## return everything in output, use names
  ## to add objects i.e. output$new_obj_name = new_obj
  output = list()

  ## :::DENOISING:::
  ## using function DenoiseMethod (default DenoiseRF)
  ## TODO: name elements of denoise data, this will
  ## allow different functions to access by name
  denoise.results = DenoiseMethod(data.obj)

  ## do we return all the elements of denoising?
  if(return.all) output$denoise.results = denoise.results
  
  ## construct rf on clean data and get probabilities
  if(is.null(rf_formula)) rf_formula = GetFormula()[[1]]
  rf.clean = randomForest(rf_formula,data=train.clean)
  pzx = predict(rf.clean,type='vote')
  class.names = colnames(pzx)

  
  predictions = class.names[apply(denoise.results$pyx,2,function(x)
    {which.max(apply(pzx,2,function(y){sum(x*y)}))})]

  ## determine error rate, add to output, return
  error = mean(predictions != test.class)
  output$error = error
  return(output)
}


DenoisificationTEST = function(){
  data.obj = GenerateDataObj()
  rf_formula = formula("Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
  output = Denoisification(iris,data.obj$test.class,
    data.obj,return.all=FALSE,rf_formula=rf_formula)
  return(output)
}



##
## select the correct training and test
## data for denoisification - used for RF denoisification
##
PrepareDataForDenoise = function(i,imp.variables){
  ## get the correct training data
  data1train.temp = subset(data1train,
    features.n_points == points.levels[i] &
    row_id==0 & !contains.random)
  data1train.temp = data1train.temp[
    order(data1train.temp$sources.original_source_id),]
  train.noisy.m = as.matrix(data1train.temp[,imp.variables])
  
  ## get the correct test set
  data1test.temp = subset(data1,
    sources.survey=='test' & features.n_points == points.levels[i])
  test.noisy.m = as.matrix(data1test.temp[,imp.variables])
  test.class = data1test.temp[,"sources.classification"]
  
  return(list("train.noisy.m"=train.noisy.m,
              "test.noisy.m"=test.noisy.m,
              "test.class"=test.class))
}
              

