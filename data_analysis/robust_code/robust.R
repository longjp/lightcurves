########## 
########## 
########## CODE FOR ANALYZING PERFORMANCE WHEN MTRY IS ADJUSTED 
##########   
##########
########## by James Long 
########## date: 5/9/2011 
########## 





# basic summary statistics
names(data1)
head(data1)
sum(is.na(data1))

names(time_flux)
head(time_flux)
sum(is.na(time_flux))

print("number of values imputed in data1:")
print(sum(is.na(data1)))


data1 = na.roughfix(data1) # this isn't fair, uses better data
data1[data1==Inf] = 0




###
### IMPORTANT:: Divide into test and training.
###  Following code is very sensitive to whether
###  this is done right.
###
data1test = subset(data1,subset=(sources.survey=="test"))
data1train = subset(data1,subset=(sources.survey=="train"))
nrow(data1test)
nrow(data1train)
contains.random = grepl("random",data1train$sources.noise_args)
data1train$contains.random = contains.random
data1train = dedupe(data1train,
  c("features.n_points","sources.original_source_id",
    "contains.random")
  )
length(unique(data1train$row_id))
table(data1train$row_id)

### CRITICAL:: This is how period plots determine what
### obs to use. If this is not set correctly clean test
### curves may be used in graphs. Usually
### points.levels = c(10,20,30,40,50,60,70,80,90,100)
points.levels = unique(data1test$features.n_points[data1test$features.source_id != data1test$sources.original_source_id])
points.levels = points.levels[order(points.levels)]
points.levels

### ALSO IMPORTANT:: Have an assigned order for classes
class.names = names(table(data1$sources.classification))


###
### formula for data analysis
###
data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id",
  "features.max_slope","features.min",
  "features.linear_trend","features.max",
  "features.weighted_average","features.median")
data1_features = data1_features[!(data1_features %in%
  to_remove)]
rf_formula = formula(paste("sources.classification ~ ",
  paste(data1_features,collapse=" + ")))
rf_formula


data1train.temp = subset(data1train,
  sources.original_source_id == features.source_id)
nrow(data1train.temp)


## construct forests on clean training using mtrys and
## then test on 10*(1:10) sampled curves in test
mtrys = c(1,2,5,10,20)
error = matrix(0,nrow=length(mtrys),ncol=length(points.levels))
for(i in 1:length(mtrys)){
  print(i)
  #rf = randomForest(rf_formula,data=data1train.temp,mtry=mtrys[i])
  rf = randomForest(rf_formula,data=data1train.temp,mtry=mtrys[i],ntree=5000)
  for(j in 1:length(points.levels)){
    data1test.temp = subset(data1test,
      features.n_points == points.levels[j])
    predictions = predict(rf,newdata=data1test.temp)
    error[i,j] = mean(predictions !=
           data1test.temp$sources.classification)
  }
}

error = computeStandardErrors(error,500,sderror=1)
pdf(graphics('mtrys.pdf'))
plotLines(error,points.levels,xlab="Number of Flux Measurements in Test Light Curves",ylab="Error",maintitle="Features Per Split and Test Set Performance")
legend(60, .55,mtrys,col=1:(dim(error)[1]),lwd=2,cex=1,title="Features / Split",pch=1:length(class.names))
dev.off()
