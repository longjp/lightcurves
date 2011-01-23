####
#### ASAS classification work
####
#### by James Long
#### date Jan 19, 2011
####


library(rpart)
library(randomForest)
library('plotrix')

# get all of the data
data1 = read.table('outputRtest.txt',header=T,sep=';',na.strings="False")
survey = as.character(data1$sources.survey)
survey[survey == "test"] = "ASAS"
data1$sources.survey = as.factor(survey)


# get just the asas data
asas = subset(data1,subset=(data1$sources.survey == "ASAS"))


# get rid of small classes
small_classes = c("Alpha2 Canum Venaticorum","Beta Cephei")
asas = subset(asas,subset=!(asas$sources.classification %in% small_classes))

# reset the classes
asas$sources.classification = as.factor(as.character(asas$sources.classification))

# delete all data with fewer than MIN_POINTS
MIN_POINTS = 50
asas = subset(asas,subset=(asas$features.n_points > 49))

# find number of rows with missing data + delete them
row_all_present = function(x){
	return(sum(is.na(x)) == 0)
}
none_missing = apply(asas,1,row_all_present)
nrow(asas) - sum(none_missing)
asas = subset(asas,subset=none_missing)

# show some summary statistics
summary(asas$features.n_points)
hist(asas$features.n_points)
boxplot(asas$features.n_points ~ asas$sources.classification)

# set training and test
training = runif(nrow(asas)) > .5
test = !training
asasTraining = subset(asas,subset=training)
asasTest = subset(asas,subset=test)


## construct formula
asas_features = names(asas)[grep("features.*",names(asas))]
to_remove = c("features.n_points","features.source_id")
asas_features = asas_features[!(asas_features %in% to_remove)]
asas_formula = formula(paste("sources.classification ~ ",paste(asas_features,collapse=" + ")))

# train
rf_asas = randomForest(asas_formula,data=asasTraining)
rpart_asas = rpart(asas_formula,data=asasTraining)

# test, get rf error rates
rf_asas_predictions = predict(rf_asas,newdata=asasTest,type="response")
sum(rf_asas_predictions != asasTest$sources.classification)
sum(rf_asas_predictions != asasTest$sources.classification) / nrow(asasTest)

# test, get rpart error rates
rpart_asas_predictions = predict(rpart_asas,newdata=asasTest,type="class")
sum(rpart_asas_predictions != asasTest$sources.classification)
sum(rpart_asas_predictions != asasTest$sources.classification) / nrow(asasTest)



# rf-table print the table
table_to_print = table(rf_asas_predictions,asasTest$sources.classification)
column_sums = colSums(table_to_print)
table_to_print = scale(table_to_print, center=F, scale = colSums(table_to_print))

par(mar=c(4,12,14,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="ASAS Test Set Confusion Matrix",ylab="",axes=FALSE,show.values=2,vcol="black")

axis(3,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)




# rpart-table print the table
table_to_print = table(rpart_asas_predictions,asasTest$sources.classification)
column_sums = colSums(table_to_print)
table_to_print = scale(table_to_print, center=F, scale = colSums(table_to_print))

par(mar=c(4,12,14,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="ASAS Test Set Confusion Matrix",ylab="",axes=FALSE,show.values=2,vcol="black")

axis(3,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)



plot(rpart_asas,margin=.2)
text(rpart_asas,cex=.7)




### what about close binary eclipsing systems and contact systems
desired_classes = c("Close Binary Eclipsing Systems","Contact Systems")

asasSysTraining = subset(asasTraining,subset=(asasTraining$sources.classification %in% desired_classes))
asasSysTraining$sources.classification = as.factor(as.character(asasSysTraining$sources.classification))
asasSysTest = subset(asasTest,subset=(asasTest$sources.classification %in% desired_classes))
asasSysTest$sources.classification = as.factor(as.character(asasSysTest$sources.classification))
nrow(asasSysTraining) + nrow(asasSysTest)


rf_sys = randomForest(asas_formula,data=asasSysTraining)
rf_sys_predictions = predict(rf_sys,newdata=asasSysTest)
table(rf_sys_predictions,asasSysTest$sources.classification)





#####
##### read two classes in
#####

# get all of the data
data1 = read.table('noisified_curves.txt',header=T,sep=';',na.strings="False")
survey = as.character(data1$sources.survey)
survey[survey == "test"] = "ASAS"
data1$sources.survey = as.factor(survey)


# get just the asas data
asas_noisified = subset(data1,subset=(data1$sources.survey == "ASAS"))
asas_noisified = subset(asas_noisified,subset=(asas_noisified$features.n_points > 49))

sum(is.na(asas_noisified))

