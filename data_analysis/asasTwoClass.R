#####
##### ASAS two-class test
#####
#####

library(rpart)
library(randomForest)
library('plotrix')

# get all of the data
data1 = read.table('outputRtest.txt',header=T,sep=';',na.strings="False")
survey = as.character(data1$sources.survey)
survey[survey == "test"] = "ASAS"
data1$sources.survey = as.factor(survey)



pdf('pair_slope_trend.pdf')
d1 = density(data1$features.pair_slope_trend,bw=.01)
plot(d1,main="pair_slope_trend for ASAS")
dev.off()





# get just the asas data
asas = subset(data1,subset=(data1$sources.survey == "ASAS"))

# keep just the two classes
of_interest = c("Close Binary Eclipsing Systems","Contact Systems")
asas = subset(asas,subset=(asas$sources.classification %in% of_interest))

# reset the classes
asas$sources.classification = as.factor(as.character(asas$sources.classification))


# make sure features are in the correct format
for(i in names(asas)){
	print(paste(i,": ",class(asas[,i])))
}

for(i in names(asas)){
	if(class(asas[,i]) != "numeric"){
		print(paste(i,": ",class(asas[,i])))
	}
}



##### get the noisified data

# get all of the data
data1 = read.table('noisified_curves.txt',header=T,sep=';',na.strings="False")
survey = as.character(data1$sources.survey)
survey[survey == "test"] = "ASAS"
data1$sources.survey = as.factor(survey)


# get just the asas data
asas_noisified = subset(data1,subset=(data1$sources.survey == "ASAS"))

sum(is.na(asas_noisified))

# make sure features are in the correct format
for(i in names(asas_noisified)){
	print(paste(i,": ",class(asas_noisified[,i])))
}

for(i in names(asas_noisified)){
	if(class(asas_noisified[,i]) != "numeric"){
		print(paste(i,": ",class(asas_noisified[,i])))
	}
}





###
### find originals to delete
###
# 1. under 50 n_points
# 2. missing data in originals or thinned
#    ( how many with missing data in thinned, but not original)
#

# testing
sum(asas$sources.original_source_id == asas$features.source_id)
sum(asas_noisified$sources.original_source_id == asas_noisified$features.source_id)

# find number of rows with missing data + delete them
row_all_present = function(x){
	return(sum(is.na(x)) == 0)
}


none_missing = apply(asas,1,row_all_present)
greater_49 = asas$features.n_points > 49
to_keep = none_missing & greater_49
sources_to_throw_out = asas$features.source_id[!to_keep]
none_missing = apply(asas_noisified,1,row_all_present)
none_infinite = rowSums(asas_noisified == Inf) == 0
to_keep = none_missing & none_infinite
noisified_to_throw_out = asas_noisified$sources.original_source_id[!to_keep]
to_ditch = unique(c(sources_to_throw_out,noisified_to_throw_out))

# do the subsetting
asas_noisified = subset(asas_noisified,subset=!(asas_noisified$sources.original_source_id %in% to_ditch))
asas = subset(asas,subset=!(asas$features.source_id %in% to_ditch))

# check that this worked
nrow(asas) == nrow(asas_noisified)
sum(asas_noisified$sources.original_source_id %in% asas$features.source_id) == nrow(asas_noisified)
sum(asas$sources.original_source_id %in% asas_noisified$sources.original_source_id) == nrow(asas)







###
### divide original id's into training and test
###
train = runif(nrow(asas)) > .5
test = !train
train = asas$features.source_id[train]
test = asas$features.source_id[test]

asas_train = subset(asas,subset=(asas$features.source_id %in% train))
asas_test = subset(asas,subset=(asas$features.source_id %in% test))


asas_noisified_train = subset(asas_noisified,subset=(asas_noisified$sources.original_source_id %in% train))
asas_noisified_test = subset(asas_noisified,subset=(asas_noisified$sources.original_source_id %in% test))

# check that this worked
nrow(asas_noisified_train) == nrow(asas_train)
nrow(asas_noisified_test) == nrow(asas_test)
sum(asas_noisified_train$sources.original_source_id %in% asas_train$features.source_id) == nrow(asas_noisified_train)
sum(asas_train$features.source_id %in% asas_noisified_train$sources.original_source_id) == nrow(asas_train)
sum(asas_noisified_test$sources.original_source_id %in% asas_test$features.source_id) == nrow(asas_noisified_test)
sum(asas_test$features.source_id %in% asas_noisified_test$sources.original_source_id) == nrow(asas_test)




#####
##### now train and test: clean-on-clean, clean-on-noisy, noisy-on-noisy, noisy-on-clean
#####

## construct formula
asas_features = names(asas)[grep("features.*",names(asas))]
to_remove = c("features.n_points","features.source_id")
asas_features = asas_features[!(asas_features %in% to_remove)]
asas_formula = formula(paste("sources.classification ~ ",paste(asas_features,collapse=" + ")))



### first build clean and noisy rf classifiers
rf_asas = randomForest(asas_formula,data=asas_train)
rf_noisified_asas = randomForest(asas_formula,data=asas_noisified_train)

## now build clean and noisy rpart classifiers
rpart_asas = rpart(asas_formula,data=asas_train)
rpart_noisified_asas = rpart(asas_formula,data=asas_noisified_train)




###
### rf predictions
###
# clean-on-clean
sum(predict(rf_asas,newdata=asas_test,type="response") != asas_test$sources.classification) / nrow(asas_test)
# clean-on-noisy
sum(predict(rf_asas,newdata=asas_noisified_test,type="response") != asas_noisified_test$sources.classification) / nrow(asas_noisified_test)
# noisy_on_noisy
sum(predict(rf_noisified_asas,newdata=asas_noisified_test,type="response") != asas_noisified_test$sources.classification) / nrow(asas_noisified_test)
# noisy_on_clean
sum(predict(rf_noisified_asas,newdata=asas_test,type="response") != asas_test$sources.classification) / nrow(asas_test)



###
### rpart predictions
###
# clean-on-clean
sum(predict(rpart_asas,newdata=asas_test,type="class") != asas_test$sources.classification) / nrow(asas_test)
# clean-on-noisy
sum(predict(rpart_asas,newdata=asas_noisified_test,type="class") != asas_noisified_test$sources.classification) / nrow(asas_noisified_test)
# noisy_on_noisy
sum(predict(rpart_noisified_asas,newdata=asas_noisified_test,type="class") != asas_noisified_test$sources.classification) / nrow(asas_noisified_test)
# noisy_on_clean
sum(predict(rpart_noisified_asas,newdata=asas_test,type="class") != asas_test$sources.classification) / nrow(asas_test)




### print clean and noisy rpart trees
plot(rpart_asas,margin=.2,uniform=T,compress=T,branch=1)
text(rpart_asas)

plot(rpart_noisified_asas,margin=.2,uniform=T,compress=T,branch=1)
text(rpart_noisified_asas)



###
### which features are important in each rf classifier
###

varImpPlot(rf_asas)
dev.new()
varImpPlot(rf_noisified_asas)




# pair slope classifier on clean data
rpart_pair_slope = rpart(sources.classification ~ features.pair_slope_trend,data=asas_train)
rpart_pair_slope
sum(predict(rpart_pair_slope,newdata=asas_test,type="class") != asas_test$sources.classification) / nrow(asas_test)

# pair slope classifier on noisy data
rpart_pair_slope_noisy = rpart(sources.classification ~ features.pair_slope_trend,data=asas_noisified_train)
rpart_pair_slope_noisy
sum(predict(rpart_pair_slope_noisy,newdata=asas_noisified_test,type="class") != asas_noisified_test$sources.classification) / nrow(asas_test)



####
#### print kde's of pair slope by class / noisy
####


pdf('noisy_pair_slope_trend.pdf')
d1 = density(asas_noisified$features.pair_slope_trend[asas_noisified$sources.classification == "Contact Systems"])
d2 = density(asas_noisified$features.pair_slope_trend[asas_noisified$sources.classification == "Close Binary Eclipsing Systems"])

ymax = max(d1$y,d2$y)
plot(d1,col='orange',ylim=c(0,ymax*1.05),main="Pair Slope Trend For Noisy ASAS Data (First 35 Flux Measurements)",sub="Orange = Contact Systems, Blue = Close Binary Eclipsing Systems")
lines(d2,col='blue')
dev.off()

pdf('clean_pair_slope_trend.pdf')
d1 = density(asas$features.pair_slope_trend[asas$sources.classification == "Contact Systems"])
d2 = density(asas$features.pair_slope_trend[asas$sources.classification == "Close Binary Eclipsing Systems"])

ymax = max(d1$y,d2$y)
plot(d1,col='orange',ylim=c(0,ymax*1.05),main="Pair Slope Trend For Clean ASAS Data",sub="Orange = Contact Systems, Blue = Close Binary Eclipsing Systems")
lines(d2,col='blue')
dev.off()



####
#### add noisy pair slope trend feature to clean classifier and see if this 
#### improves performance
####

just_pair_slope = subset(asas_noisified_train,select=c("features.pair_slope_trend","sources.original_source_id"))
names(just_pair_slope) = c("features.pair_slope_trend_noisy","sources.original_source_id")
asas_train_2 = merge(asas_train,just_pair_slope)

just_pair_slope = subset(asas_noisified_test,select=c("features.pair_slope_trend","sources.original_source_id"))
names(just_pair_slope) = c("features.pair_slope_trend_noisy","sources.original_source_id")
asas_test_2 = merge(asas_test,just_pair_slope)


## construct formula
asas_features = names(asas_train_2)[grep("features.*",names(asas_train_2))]
to_remove = c("features.n_points","features.source_id")
asas_features = asas_features[!(asas_features %in% to_remove)]
asas_formula = formula(paste("sources.classification ~ ",paste(asas_features,collapse=" + ")))



rf_asas_2 = randomForest(asas_formula,data=asas_train_2)
# clean-on-clean
sum(predict(rf_asas_2,newdata=asas_test_2,type="response") != asas_test_2$sources.classification) / nrow(asas_test_2)








#####
##### why not use all noisy features?
#####

names(asas_noisified_train) = paste(names(asas_noisified_train),".noisified",sep="")
asas_noisified_train$sources.original_source_id = asas_noisified_train$sources.original_source_id.noisified
asas_train_2 = merge(asas_train,asas_noisified_train)


names(asas_noisified_test) = paste(names(asas_noisified_test),".noisified",sep="")
asas_noisified_test$sources.original_source_id = asas_noisified_test$sources.original_source_id.noisified
asas_test_2 = merge(asas_test,asas_noisified_test)




## construct formula
asas_features = names(asas_train_2)[grep("features.*",names(asas_train_2))]
to_remove = c("features.n_points","features.source_id","features.n_points.noisified","features.source_id.noisified")
asas_features = asas_features[!(asas_features %in% to_remove)]
asas_formula = formula(paste("sources.classification ~ ",paste(asas_features,collapse=" + ")))

rf_asas_2 = randomForest(asas_formula,data=asas_train_2)
# clean-on-clean
sum(predict(rf_asas_2,newdata=asas_test_2,type="response") != asas_test_2$sources.classification) / nrow(asas_test_2)





