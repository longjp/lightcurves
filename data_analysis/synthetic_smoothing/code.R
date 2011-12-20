########## 
########## 
########## COMPARE HIP / OGLE CADENCES ON SYNTHETIC LCs
########## THIS INVOLVES SMOOTHING
##########
########## by James Long 
########## date: 8/2/11 
########## 


# program setup
rm(list=ls(all=TRUE))
set.seed(22071985)
source('~/Rmodules/Rfunctions.R')
library('randomForest')
library('rpart')
library('xtable')

## the most interesting features
good_features = c(
  "features.p2p_scatter_over_mad",
  "features.small_kurtosis",
  "features.p2p_scatter_2praw",
  "features.beyond1std",
  "features.freq1_harmonics_amplitude_0",
  "features.freq1_harmonics_amplitude_1",
  "features.qso_log_chi2nuNULL_chi2nu",      
  "features.percent_difference_flux_percentile",
  "features.qso_log_chi2_qsonu",
  "features.flux_percentile_ratio_mid20",
  "features.freq1_harmonics_rel_phase_3",      
  "features.flux_percentile_ratio_mid80",      
  "features.skew",      
  "features.median_buffer_range_percentage",   
  "features.fold2P_slope_90percentile",        
  "features.median_absolute_deviation",      
  "features.flux_percentile_ratio_mid35",
  "features.freq1_harmonics_freq_0",      
  "features.flux_percentile_ratio_mid65",
  "features.freq_signif",
  "features.std",                  
  "features.scatter_res_raw",        
  "features.fold2P_slope_10percentile",      
  "features.flux_percentile_ratio_mid50",
  "features.percent_amplitude",
  "features.amplitude",       
  "features.p2p_scatter_pfold_over_mad",
  "features.p2p_ssqr_diff_over_var",
  "features.stetson_j",
  "features.stetson_k")






## load the data
features = '../../data_processed/cadence_comparison/ogle_versus_hipparcos.dat'
tfe = '../../data_processed/cadence_comparison/tfe_ogle_versus_hipparcos.dat'
data1total = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)



## examine this
nrow(data1total)
nrow(time_flux)

table(data1total$sources.survey)
table(data1total$sources.noisification)

names(data1total)
table(data1total$sources.noisification)


## break up by obs type
data1total$obs_type[data1total$sources.survey ==
                    "ogle_test"] = "ogle_test"
data1total$obs_type[data1total$sources.survey ==
                    "hipparcos_test"] = "hipparcos_test"
data1total$obs_type[data1total$sources.survey == "ogle_train" &
                    data1total$sources.noisification !=
                    "cadence_noisify_smoothed"] = "ogle_train"
data1total$obs_type[(data1total$sources.survey ==
                    "hipparcos_train" &
                    data1total$sources.noisification !=
                    "cadence_noisify_smoothed")] =
                    "hipparcos_train"
to_select = (data1total$sources.survey ==
                    "ogle_train" &
                    data1total$sources.noisification ==
                    "cadence_noisify_smoothed" &
                    grepl("hip",data1total$sources.noise_args))
data1total$obs_type[to_select] = "ogle_train_smoothed_hipparcos"
to_select = (data1total$sources.survey == "ogle_train" &
             data1total$sources.noisification ==
             "cadence_noisify_smoothed" &
             grepl("ogle",data1total$sources.noise_args))
data1total$obs_type[to_select] = "ogle_train_smoothed_ogle"
to_select = (data1total$sources.survey == "hipparcos_train" &
             data1total$sources.noisification ==
             "cadence_noisify_smoothed" &
             grepl("hip",data1total$sources.noise_args))
data1total$obs_type[to_select] =
             "hipparcos_train_smoothed_hipparcos"
to_select = (data1total$sources.survey == "hipparcos_train" &
             data1total$sources.noisification ==
             "cadence_noisify_smoothed" &
             grepl("ogle",data1total$sources.noise_args))
data1total$obs_type[to_select] = "hipparcos_train_smoothed_ogle"
table(data1total$obs_type)


## now change some noisification to identity
sum(grepl("all",data1total$sources.noise_args))
to_select = grepl("all",data1total$sources.noise_args)
data1total$sources.noisification[to_select] = "identity"
sum(data1total$sources.noisification == "identity")
data1total$sources.original_source_id[data1total$sources.noisification == "identity"] = data1total$features.source_id[data1total$sources.noisification == "identity"]


sum(data1total$sources.original_source_id == data1total$features.source_id)



## now dedupe
contains.random = grepl("random",data1total$sources.noise_args)
sum(contains.random)
data1total$contains.random = contains.random
data1total$is_original = 1*(data1total$sources.original_source_id ==
  data1total$features.source_id)
data1total = dedupe(data1total,
  c("features.n_points","sources.original_source_id",
    "contains.random","is_original","obs_type")
  )





####
#### VIEW A FEW FEATURES
####

Ffeature = function(x){
  return(x)
}

Produce3dScatterPlot = function(feature,
  train_name,n_points,Ffeature,new=TRUE){
  which_points = ((data1total$obs_type == train_name) &
                  (data1total$features.n_points == n_points) &
                  (data1total$row_id == 0) &
                  (!data1total$contains.random))
  if(new)  dev.new()
  Draw3dScatterplot(Ffeature(data1total[which_points,feature]),
                    data1total$sources.classification[which_points],
                    xlab=paste(sub("features.","",feature),
                      " --- ",train_name))
}


ProduceKDE = function(feature,
  train_name,n_points){
  which_points = ((data1total$obs_type == train_name) &
                  (data1total$features.n_points == n_points) &
                  (data1total$row_id == 0) &
                  (!data1total$contains.random))
  feature_vals = data1total[which_points,feature]
  class_vals = data1total$sources.classification[which_points]
  class_vals = as.character(class_vals)
  class_vals[class_vals != "Mira"] = "Other"
  ordering = order(class_vals)
  print(length(class_vals))
  return(list(feature_vals[ordering],class_vals[ordering]))
}


the_data = ProduceKDE("features.amplitude","ogle_train",10)
pdf('amplitude_ogle_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],trim=.01,xlab="amplidude (mags)")
dev.off()
the_data = ProduceKDE("features.amplitude","hipparcos_train",10)
pdf('amplitude_hip_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],trim=.01,xlab="amplidude (mags)")
dev.off()


the_data = ProduceKDE("features.p2p_scatter_over_mad",
  "ogle_train",10)
pdf('p2p_ogle_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],trim=.01,
         xlab="P2PS")
dev.off()

the_data = ProduceKDE("features.p2p_scatter_over_mad",
  "hipparcos_train",10)
pdf('p2p_hip_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],trim=.01,
         xlab="P2PS")
dev.off()






## now view

graphics = fileOutLoc('figures_cadences/scatterplots/')

train_names = c("hipparcos_train","ogle_train",
  "hipparcos_train_smoothed_hipparcos",
  "hipparcos_train_smoothed_ogle",
  "ogle_train_smoothed_ogle",
  "ogle_train_smoothed_hipparcos")


features = c(
  "features.linear_trend",
  "features.p2p_scatter_over_mad",
  "features.p2p_ssqr_diff_over_var",
  "features.freq1_harmonics_freq_0",
  "features.amplitude",
  "features.skew",
  "features.qso_log_chi2nuNULL_chi2nu",
  "features.qso_log_chi2_qsonu")
n_points = c(10,50,100)

for(feature in features){
  for(n_point in n_points){
    for(i in train_names){
      pdf(graphics(paste(n_point,feature,i,'.pdf',sep="")))
      Produce3dScatterPlot(feature,i,n_point,Ffeature,new=FALSE)
      dev.off()
    }
  }
}






########
########  RUN ANALYSIS
########


### change to only train / test for survey
data1total$sources.survey = as.character(data1total$sources.survey)
data1total$sources.survey[grepl("train",data1total$sources.survey)] = "train"
data1total$sources.survey[grepl("test",data1total$sources.survey)] = "test"
data1total$sources.survey = as.factor(data1total$sources.survey)
table(data1total$sources.survey)




## ditch the row_id, this will be recreated
data1total$row_id = NULL
data1total$contains.random = NULL
data1total$is_original = NULL


names(data1total)
table(data1total$obs_type)


## set the test / train combinations you wish to run
test_names = c("hipparcos_test","ogle_test")
train_names = c("hipparcos_train","ogle_train",
  "hipparcos_train_smoothed_hipparcos",
  "hipparcos_train_smoothed_ogle",
  "ogle_train_smoothed_ogle",
  "ogle_train_smoothed_hipparcos")

for(a_test_name in test_names){
  for(a_train_name in train_names){
    graphics = fileOutLoc(paste('figures_cadences/',
      a_test_name,a_train_name,sep=""))
    tables = fileOutLoc(paste('tables_cadences/',
      a_test_name,a_train_name,sep=""))
    RData = fileOutLoc(paste('RData/',a_test_name,a_train_name,sep=""))
    data1 = subset(data1total,obs_type %in% c(a_test_name,a_train_name))
    data1$obs_type = NULL
    print("the test / train sets are:")
    print(a_test_name)
    print(a_train_name)
    print("the sources table is:")
    print(table(data1$sources.survey))
    source('../noisification_code/noisification_analysis.R')
  }
}

    



### ANALYSIS RESULTS FOR HIPPARCOS TEST
errorsSD.toplot = array(0,c(5,length(points.levels),3))

a_test_name = "hipparcos_test"
a_train_name = "hipparcos_train"
RData = fileOutLoc(paste('RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[1,,] = errorsSD[1,,]
errorsSD.toplot[4,,] = errorsSD[4,,]

a_train_name = "ogle_train"
RData = fileOutLoc(paste('RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[2,,] = errorsSD[4,,]
errorsSD.toplot[5,,] = errorsSD[1,,]


a_train_name = "ogle_train_smoothed_hipparcos"
RData = fileOutLoc(paste('RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[3,,] = errorsSD[4,,]


pdf('hipparcosTestCadence.pdf')
plotLines(errorsSD.toplot,points.levels,ylab="Error Rate",xlab="Number Flux Test Set",maintitle="Unlabeled Data at Hipparcos Cadence")
legend("topright",c("Hipparcos Cadence Naive","Ogle Cadence Noisified","Ogle Smoothed To Hipparcos - Noisified","Hipparcos Cadence Noisified","Ogle Naive"),col=c(1,2,3,4,5),lwd=2,cex=1,title="Training Sets",pch=1:5)
dev.off()





### ANALYSIS RESULTS FOR OGLE TEST
errorsSD.toplot = array(0,c(5,length(points.levels),3))
a_test_name = "ogle_test"
a_train_name = "ogle_train"
RData = fileOutLoc(paste('RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[1,,] = errorsSD[1,,]
errorsSD.toplot[4,,] = errorsSD[4,,]


a_train_name = "hipparcos_train"
RData = fileOutLoc(paste('RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[2,,] = errorsSD[4,,]
errorsSD.toplot[5,,] = errorsSD[1,,]

a_train_name = "hipparcos_train_smoothed_ogle"
RData = fileOutLoc(paste('RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[3,,] = errorsSD[4,,]


pdf('ogleTestCadence.pdf')
plotLines(errorsSD.toplot,points.levels,ylab="Error Rate",xlab="Number Flux Test Set",maintitle="Unlabeled Data at Ogle Cadence")
legend("topright",c("Ogle Cadence Naive","Hipparcos Cadence Noisified","Hipparcos Smoothed To Ogle - Noisified","Ogle Cadence Noisified","Hipparcos Naive"),col=c(1,2,3,4,5),lwd=2,cex=1,title="Training Sets",pch=1:5)
dev.off()
