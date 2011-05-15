########## 
########## 
########## FUNCTION USED BY RF REGRESSION 
########## DENOISIFICATION
##########
########## by James Long 
########## date: MAY 12, 2011 
########## 



##
## predict noisy version of clean curves, specifically
## predict each of the features in y.vars, using the
## features x.vars
##
PredictResponses = function(data.obj){
  require('randomForest')
  rfs = list()
  for(i in 1:length(data.obj$y.vars)){
    print(sprintf("run %s / %s",i,length(data.obj$y.vars)))
    rfs[[i]] = randomForest(data.obj$train.clean.m[,data.obj$x.vars],
         data.obj$train.noisy.m[,data.obj$y.vars[i]],
         nodesize=5)
  }
  y.hat = matrix(0,nrow=nrow(data.obj$train.clean.m),
    ncol=length(data.obj$y.vars))
  y.res = matrix(0,nrow=nrow(data.obj$train.clean.m),
    ncol=length(data.obj$y.vars))
  for(i in 1:length(data.obj$y.vars)){
    y.hat[,i] = predict(rfs[[i]])
    y.res[,i] = y.hat[,i] - data.obj$train.noisy.m[,data.obj$y.vars[i]]
  }
  sde = matrix(apply(y.res,2,sd),nrow=1)
  return(list("rfs"=rfs,"y.hat"=y.hat,"y.res"=y.res,"sde"=sde))
}

PredictResponsesTEST = function(){
  data.obj = GenerateDataObj()
  output = PredictResponses(data.obj)
  return(output)
}


## y = (# test) x (# features) from test
## y.hat = (# train) x (#features) from train
## output (# train) x (# test) x (# features)
##    where differences are taken between every
##    pair of training and test obs
YminusYhat = function(y,y.hat){
  result = array(0,dim=c(nrow(y.hat),nrow(y),ncol(y.hat)))
  for(i in 1:(dim(result)[2])){
    v.y = y[i,]
    result[,i,] = t(apply(y.hat,1,function(x){x - v.y}))
  }
  return(result)
}
## TEST: YminusYhat - seems to work
YminusYhatTEST = function(){
  print('arg1:')
  a = matrix(c(1,2,3,4),ncol=2)
  print(a)
  print('arg2:')
  b = matrix(c(10,1,15,8,0,12),ncol=2)
  print(b)
  print('function call: YminusYhat(a,b)')
  c = YminusYhat(a,b)
  print(dim(c))
  print('result c[,1,]')
  print(c[,1,])
  print('result c[,2,]')
  print(c[,2,])
  return(c)
}




## divide all the differences in big.raw by
## their standard deviations
StandardizeBigRaw = function(big.raw,sds){
  big.norm = array(0,dim=dim(big.raw))
  if(!is.matrix(sds)){
    print('sds must be a matrix')
    return(0)
  }
  ## if a single standard deviation for all obs, just repeat it
  if(dim(sds)[1] == 1){
    sds = matrix(rep(sds,dim(big.raw)[1]),
      nrow=dim(big.raw)[1],byrow=TRUE)
  }
  ## make sure sds conforms to dimensions of big.raw
  if(!identical(dim(sds),dim(big.raw)[c(1,3)])){
    print("sds wrong dimensions")
    stop
  }
  for(i in 1:(dim(big.raw)[2])){
    big.norm[,i,] = big.raw[,i,] / sds
  }
  return(big.norm)
}
## TEST: StandardizeBigRaw - seems to work
StandardizeBigRawTEST = function(){
  c = YminusYhatTEST()
  print('=======++++++++========')
  print('c[,1,] is:')
  print(c[,1,])
  print('c[,2,] is:')
  print(c[,2,])
  sds = array(c(.5,1),dim=c(1,2))
  print('sds is:')
  print(sds)
  print('dim(sds) is:')
  print(dim(sds))
  print('input: StandardizeBigRaw(c,sds)')
  c.norm = StandardizeBigRaw(c,sds)
  print('output dim:')
  print(dim(c.norm))
  print('c.norm[,1,] is:')
  print(c.norm[,1,])
  print('c.norm[,2,] is:')
  print(c.norm[,2,])
}


### main purpose is to estimate pyx using RF
### returns a bunch of other information which
### user may examine for assumption checking
### data.obj$
### 1. train.clean.m -> matrix of clean training, named columns
### 2. train.noisy.m -> matrix of noisy version of the training
### 3. test.noisy.m -> test curves
### 4. x.vars -> x's used to predict . . .
### 5. y.vars -> 1 RF regression for each of these
DenoiseRF = function(data.obj){
  muyx = PredictResponses(data.obj)
  
  ## get test.noisy - E(train.noisy|train.clean) for all
  ## test.noisy obs and all (train.noisy,train.clean) pairs
  ## for every feature, then standardize by sde
  big.raw = YminusYhat(data.obj$test.noisy.m,muyx$y.hat)  
  big.norm = StandardizeBigRaw(big.raw,muyx$sde)
  
  ## take the product across all the features i.e.
  ## p(y_1|x) x . . . x p(y_m|x) = p(y|x)
  ## should be (#train) x (# test)
  ## 1 column for each test (each y)
  pyx = apply(big.norm,c(1,2),function(x){prod(dnorm(x))})
  return(list("pyx"=pyx,"muyx"=muyx))
}


DenoiseRFTEST = function(){
  data.obj = GenerateDataObj()
  output = DenoiseRF(data.obj)
  return(output)
}


GenerateDataObj = function(OFFSET=0,TESTNUM=100,
                           NOISE.LEVEL=1/2,NOISE.RATIO=1:4){
  data(iris)
  x.vars = names(iris)[1:4]
  y.vars = names(iris)[1:4]
  train.clean.m = as.matrix(iris[,1:4])
  train.noisy.m = as.matrix(iris[,1:4]) + 2 + 
    (matrix(rnorm(nrow(iris)*4)*NOISE.LEVEL,nrow=nrow(iris)) *
      matrix(NOISE.RATIO,ncol=4,nrow=nrow(iris),byrow=TRUE))
  test.noisy.m = as.matrix(iris[((OFFSET+1):(OFFSET+TESTNUM)),1:4]) + 
    (matrix(rnorm(TESTNUM*4)*NOISE.LEVEL,nrow=TESTNUM) *
     matrix(NOISE.RATIO,ncol=4,TESTNUM,byrow=TRUE)) + 2
  test.class = iris[((OFFSET+1):(OFFSET+TESTNUM)),5]
  data.obj = list("train.clean.m"=train.clean.m,
    "train.noisy.m"=train.noisy.m,
    "test.noisy.m"=test.noisy.m,
    "x.vars"=x.vars,
    "y.vars"=y.vars,
    "test.class"=test.class)
  return(data.obj)
}





