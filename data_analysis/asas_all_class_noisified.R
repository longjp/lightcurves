#####
##### ASAS all-class test
#####
##### by James Long
##### date Jan 25
#####

library(rpart)
library(randomForest)
library('plotrix')
library(xtable)
library(vioplot)


#####
##### get the clean data
#####

# get all of the data
data1 = read.table('outputRtest.txt',header=T,sep=';',na.strings="False")
survey = as.character(data1$sources.survey)
survey[survey == "test"] = "ASAS"
data1$sources.survey = as.factor(survey)


# get just the asas data
asas = subset(data1,subset=(data1$sources.survey == "ASAS"))
nrow(asas)

# get rid of small classes
small_classes = c("Alpha2 Canum Venaticorum","Beta Cephei")
asas = subset(asas,subset=!(asas$sources.classification %in% small_classes))

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




#####
##### get the noisified data
#####


# get all of the data
asas_noisified = read.table('all_asas_35_features.txt',header=T,sep=';',na.strings="False")

sum(is.na(asas_noisified))

# get rid of small classes
small_classes = c("Alpha2 Canum Venaticorum","Beta Cephei")
asas_noisified = subset(asas_noisified,subset=!(asas_noisified$sources.classification %in% small_classes))

# reset the classes
asas_noisified$sources.classification = as.factor(as.character(asas_noisified$sources.classification))



# make sure features are in the correct format
for(i in names(asas_noisified)){
	print(paste(i,": ",class(asas_noisified[,i])))
}

for(i in names(asas_noisified)){
	if(class(asas_noisified[,i]) != "numeric"){
		print(paste(i,": ",class(asas_noisified[,i])))
	}
}


nrow(asas_noisified)


# testing
sum(asas$sources.original_source_id == asas$features.source_id)
sum(asas_noisified$sources.original_source_id == asas_noisified$features.source_id)


###
### delete some observations
###

# clean sources to throw out
none_missing = rowSums(is.na(asas)) == 0
greater_49 = asas$features.n_points > 49
to_keep = none_missing & greater_49
sources_to_throw_out = asas$features.source_id[!to_keep]

# noisy souces to throw out
none_missing = rowSums(is.na(asas_noisified)) == 0
none_infinite = rowSums(asas_noisified == Inf) == 0
to_keep = none_missing & none_infinite
noisified_to_throw_out = asas_noisified$sources.original_source_id[!to_keep]

# combine the two
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


### bayes rate?
1 - max(table(asas$sources.classification) / nrow(asas))

###
### rf predictions
###
errors = c(0,0,0,0)
names_errors = c("Clean Train / Clean Test","Clean Train / Noisy Test","Noisy Train / Noisy Test","Noisy Train / Clean Test")
names(errors) = names_errors

# clean-on-clean
errors[1] = sum(predict(rf_asas,newdata=asas_test,type="response") != asas_test$sources.classification) / nrow(asas_test)
# clean-on-noisy
errors[2] = sum(predict(rf_asas,newdata=asas_noisified_test,type="response") != asas_noisified_test$sources.classification) / nrow(asas_noisified_test)
# noisy_on_noisy
errors[3] = sum(predict(rf_noisified_asas,newdata=asas_noisified_test,type="response") != asas_noisified_test$sources.classification) / nrow(asas_noisified_test)
# noisy_on_clean
errors[4] = sum(predict(rf_noisified_asas,newdata=asas_test,type="response") != asas_test$sources.classification) / nrow(asas_test)

errors = as.data.frame(errors)
colnames(errors) = "Error Rate"
outputX = xtable(errors,digits=3,caption="Error rates for clean / noisy training data used to predict clean / noisy test data")
outputX

print(outputX,type='latex',file='report/errors.tex',include.rownames=TRUE,append=F)





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



##### single feature distributions by class in clean and noisy data

vioplot(asas_noisified[,1],asas_noisified[,2], names=c("Feat 1","Feat 2"),
   col="gold")
title("Violin Plots of Feature Distributions")




###
### which features are important in each rf classifier
###

pdf('report/var_imp_clean.pdf')
varImpPlot(rf_asas,main="Feature Imp. Clean Classifier")
dev.off()

pdf('report/var_imp_noisy.pdf')
varImpPlot(rf_noisified_asas,main="Feature Imp. Noisy Classifier")
dev.off()



# clean on clean
rf_asas_predictions = predict(rf_asas,newdata=asas_test,type="response")
pdf('report/asas_confusion_clean_on_clean.pdf')
table_to_print = table(rf_asas_predictions,asas_test$sources.classification)
column_sums = colSums(table_to_print)
table_to_print = scale(table_to_print, center=F, scale = colSums(table_to_print))

par(mar=c(4,12,14,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="Clean Training - Clean Test",ylab="",axes=FALSE,show.values=2,vcol="black")

axis(3,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)
dev.off()



# clean_on_noisy predictions
rf_asas_clean_on_noisy = predict(rf_asas,newdata=asas_noisified_test,type="response")
pdf('report/asas_confusion_clean_on_noisy.pdf')
table_to_print = table(rf_asas_clean_on_noisy,asas_noisified_test$sources.classification)
column_sums = colSums(table_to_print)
table_to_print = scale(table_to_print, center=F, scale = colSums(table_to_print))

par(mar=c(4,12,14,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="Clean Training - Noisy Test",ylab="",axes=FALSE,show.values=2,vcol="black")

axis(3,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)
dev.off()



# noisy_on_noisy predictions
rf_asas_noisy_on_noisy = predict(rf_noisified_asas,newdata=asas_noisified_test,type="response")
pdf('report/asas_confusion_noisy_on_noisy.pdf')
table_to_print = table(rf_asas_noisy_on_noisy,asas_noisified_test$sources.classification)
column_sums = colSums(table_to_print)
table_to_print = scale(table_to_print, center=F, scale = colSums(table_to_print))

par(mar=c(4,12,14,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="Noisy Training - Noisy Test",ylab="",axes=FALSE,show.values=2,vcol="black")

axis(3,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)
dev.off()


# noisy_on_clean predictions
rf_asas_noisy_on_clean = predict(rf_noisified_asas,newdata=asas_test,type="response")
pdf('report/asas_confusion_noisy_on_clean.pdf')
table_to_print = table(rf_asas_noisy_on_clean,asas_test$sources.classification)
column_sums = colSums(table_to_print)
table_to_print = scale(table_to_print, center=F, scale = colSums(table_to_print))

par(mar=c(4,12,14,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="Noisy Training - Clean Test",ylab="",axes=FALSE,show.values=2,vcol="black")

axis(3,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)
dev.off()











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





