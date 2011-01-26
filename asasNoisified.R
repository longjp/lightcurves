#####
##### get noisy ASAS data, compare confusion matrix to the clean case
#####
##### by James Long
##### date Jan 25, 2011

library(rpart)
library(randomForest)
library('plotrix')

# get all of the data
asas = read.table('all_asas_35_features.txt',header=T,sep=';',na.strings="False")

# get rid of small classes
small_classes = c("Alpha2 Canum Venaticorum","Beta Cephei")
asas = subset(asas,subset=!(asas$sources.classification %in% small_classes))

# reset the classes
asas$sources.classification = as.factor(as.character(asas$sources.classification))

# remove missing and infinite observations
none_missing = rowSums(is.na(asas)) == 0
none_infinite = rowSums(asas == Inf) == 0
asas = subset(asas,subset=(none_missing & none_infinite))

# set training and test
training = runif(nrow(asas)) > .5
test = !training
asasTraining = subset(asas,subset=training)
asasTest = subset(asas,subset=test)

## construct formula
asas_features = names(asas)[grep("features.*",names(asas))]
to_remove = c("features.n_points","features.source_id,features.pair_slope_trend")
asas_features = asas_features[!(asas_features %in% to_remove)]
asas_formula = formula(paste("sources.classification ~ ",paste(asas_features,collapse=" + ")))

# train
rf_asas = randomForest(asas_formula,data=asasTraining)

# see what classes there are
table(asasTest$sources.classification)
# see what classes there are, fractionally
table(asasTest$sources.classification) / nrow(asasTest)
# bayes error assuming uniformative features is about:
1 - max(table(asasTest$sources.classification) / nrow(asasTest))

# test, get rf error rates
rf_asas_predictions = predict(rf_asas,newdata=asasTest,type="response")
sum(rf_asas_predictions != asasTest$sources.classification)
sum(rf_asas_predictions != asasTest$sources.classification) / nrow(asasTest)


# rf-table print the table
pdf('asas_confusion_noisy.pdf')

table_to_print = table(rf_asas_predictions,asasTest$sources.classification)
column_sums = colSums(table_to_print)
table_to_print = scale(table_to_print, center=F, scale = colSums(table_to_print))

par(mar=c(4,12,14,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="ASAS Test Set Confusion Matrix - Noisy Data",ylab="",axes=FALSE,show.values=2,vcol="black")

axis(3,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)

dev.off()

varImpPlot(rf_asas)


