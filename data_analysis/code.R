####
#### R file to analyze lightcurve data output by 
#### lightcurve database
####
#### by James Long
#### date Jan 12, 2011
####

library(rpart)
library(randomForest)
library('plotrix')

#####
##### to do:
#####
## 1. get results from 3 classes in presentable form
## 2. assess significance of results
## 3. what happens if I remove features used in these trees, still a way to separate
## 3b. what is practical meaning of these features
## 4. train on ASAS, classify debosscher
## 5. make 4 and its inverse presentable, think about what these mean
## 6. what are QSO features?
## 8. read that website on R graphics


## load data
data1 = read.table('outputRtest.txt',header=T,sep=';',na.strings="False")
survey = as.character(data1$sources.survey)
survey[survey == "test"] = "ASAS"
data1$sources.survey = as.factor(survey)

tfe = read.table('tfe.txt',header=T,sep=';')

sum(data1$sources.classification == "Mira")
names(data1)
nrow(data1)
head(data1)

sum(is.na(data1))


for(i in names(data1)){
	print(paste(i,": ",class(data1[,i])))
}

for(i in names(data1)){
	if(class(data1[,i]) != "numeric"){
		print(paste(i,": ",class(data1[,i])))
	}
}


# missing values?
sum(is.na(data1))





##
## check that folding is being done correctly
##

find_curve = function(data1_row){
	curves_sub = subset(tfe,subset=(tfe$source_id == as.double(data1_row["features.source_id"])))
	return(curves_sub)
}

fold_curve = function(times,freq){
	number_periods = times * freq
	folded = number_periods - floor(number_periods)
	return(folded)
}



obs_vec = (1:nrow(data1))[data1$sources.survey == "ASAS" & data1$sources.classification == "Mira"][1:25]
length(obs_vec)


for(i in obs_vec){
	relevant_curves = find_curve(data1[i,])
	par(mfcol=c(2,1))
	plot(relevant_curves[,2],relevant_curves[,3],main=paste(data1[i,"sources.survey"], " - ", data1[i,"sources.classification"]))

	folded_times = fold_curve(relevant_curves[,2],data1[i,"features.freq1_harmonics_freq_0"])

	plot(folded_times,relevant_curves[,3])
	Sys.sleep(1)
}






###
### determine frequency of sources by class name
###

table(data1$sources.classification,data1$sources.survey)




###
### construct a forest on debosscher data
###

## construct formula
data1_features = names(data1)[grep("features.*",names(data1))]
to_remove = c("features.n_points","features.source_id")
data1_features = data1_features[!(data1_features %in% to_remove)]
rf_formula = formula(paste("sources.classification ~ ",paste(data1_features,collapse=" + ")))

# subset so only using debosscher, remake classification factor
data1deboss = subset(data1,subset=(data1$sources.survey == "debosscher"))
data1deboss$sources.classification = as.factor(as.character(data1deboss$sources.classification))

# subset asas as well, remove all na's
row_all_present = function(x){
	return(sum(is.na(x)) == 0)
}


data1asas = subset(data1,subset=(data1$sources.survey == "ASAS"))
to_keep = apply(data1asas,1,row_all_present)
none_infinite = rowSums(data1asas == Inf) == 0
data1asas = subset(data1asas,subset=(to_keep & none_infinite))
data1asas$sources.classification = as.factor(as.character(data1asas$sources.classification))



rf_deboss = randomForest(rf_formula,data=data1deboss)
rpart_deboss = rpart(rf_formula,data=data1deboss)


#for(i in names(data1asas)){
#	if(class(data1[,i]) != "numeric"){
#		print(paste(i,": ",class(data1[,i])))
#	}
#}

# predictions from tree
asas_rpart_predictions = predict(rpart_deboss,newdata=data1asas,type="class")
# predictions from forest
asas_rf_predictions = predict(rf_deboss,newdata=data1asas,type="class")





# print the table
table_to_print = table(data1asas$sources.classification,asas_rf_predictions)
table_to_print = table_to_print / rowSums(table_to_print)

par(mar=c(12,12,4,1))
color2D.matplot(table_to_print,extremes=c('white','red'),xlab="",ylab="",axes=FALSE,show.values=T,main="ASAS Classified Using Debosscher as Training",vcol="black")

axis(1,at=(1:ncol(table_to_print) - .5),labels=colnames(table_to_print),las=2)
axis(2,at=(1:nrow(table_to_print) - .5),labels=rownames(table_to_print)[nrow(table_to_print):1],las=1)






###
### build a classifier on three shared classes
###
table(data1$sources.classification,data1$sources.survey)


compare = function(class_compare){
	data1_single = subset(data1,subset=data1$sources.classification==class_compare)

	## construct formula
	features = names(data1_single)[grep("features.*",names(data1))]
	to_remove = c("features.n_points","features.source_id")
	data1_features = data1_features[!(data1_features %in% to_remove)]
	rf_formula = formula(paste("sources.survey ~ ",paste(data1_features,collapse=" + ")))

	# divide into training and test
	test = runif(nrow(data1_single)) > .8
	training = !test	
	rpart_single = rpart(rf_formula,data=data1_single,subset=training)
	predictions = predict(rpart_single,newdata=data1_single[test,],type="class")
	error_rate = sum(predictions != data1_single[test,"sources.survey"]) / length(predictions)

	# error rate - bayes error
	min_bayes = sum(data1_single$sources.survey == "ASAS") / nrow(data1_single)
	bayes_rate = min(min_bayes,1 - min_bayes)
	return(list(rpart_single,error_rate,bayes_rate))
}




# Mira 
class_name = "Mira"
compare_output = compare(class_name)

pdf(class_name)
plot(compare_output[[1]],margin=.1,main=class_name)
text(compare_output[[1]])
dev.off()

compare_output[[2]]
compare_output[[3]]


# Delta Scuti
class_name = "Delta Scuti"
compare_output = compare(class_name)

pdf(class_name)
plot(compare_output[[1]],margin=.1,main=class_name)
text(compare_output[[1]])
dev.off()

compare_output[[2]]
compare_output[[3]]




# RR Lyrae Fundamental Mode
class_name = "RR Lyrae, Fundamental Mode"
compare_output = compare(class_name)

pdf(class_name)
plot(compare_output[[1]],margin=.1,main=class_name)
text(compare_output[[1]])
dev.off()

compare_output[[2]]
compare_output[[3]]



